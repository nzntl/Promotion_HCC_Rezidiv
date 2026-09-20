"""Blockvergleich (kumulative Ablation) am Landmark POD 180: GBSA in fuenffach wiederholter 5-fach-Kreuzvalidierung
mit gepaartem Bootstrap. Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx, data_dynamic_surv.xlsx;
schreibt Tab_3_17_Ablation_Landscape.tex.
"""
from __future__ import annotations

# ── Run constants ──
OUTER_K       = 5
OUTER_REPEATS = 5
BOOTSTRAP_B   = 5000
SEED          = 42
LANDMARK_DAYS = 180

def _gbsa_params_pod180():
    import json, statistics
    pfad = Path(__file__).resolve().parent / "stage3_pod180_hyperparams.json"
    rueckfall = dict(n_estimators=100, learning_rate=0.073, max_depth=2)
    if not pfad.exists():
        print(f"[stage3c] WARN: {pfad.name} fehlt, nutze Rueckfallwerte "
              f"{rueckfall}")
        return rueckfall
    with pfad.open(encoding="utf-8") as f:
        folds = json.load(f).get("GBSA", [])
    if not folds:
        print(f"[stage3c] WARN: kein GBSA-Eintrag in {pfad.name}, nutze "
              f"Rueckfallwerte {rueckfall}")
        return rueckfall
    med = {}
    for k in ("n_estimators", "learning_rate", "max_depth"):
        werte = [f[k] for f in folds if k in f]
        if not werte:
            return rueckfall
        m = statistics.median(werte)
        med[k] = int(round(m)) if k != "learning_rate" else float(m)
    print(f"[stage3c] GBSA-Hyperparameter aus {len(folds)} POD-180-Falten: {med}")
    return med

import os, sys, warnings
from pathlib import Path

GBSA_PARAMS = _gbsa_params_pod180()

os.environ.setdefault("PYTHONIOENCODING", "utf-8")
try: sys.stdout.reconfigure(encoding="utf-8")
except Exception: pass

import numpy as np
import pandas as pd

from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.model_selection import StratifiedKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler, FunctionTransformer

from sksurv.ensemble import GradientBoostingSurvivalAnalysis
from sksurv.metrics import concordance_index_censored
from sksurv.util import Surv


def _kat_als_text(X):
    """Kategoriale Spalten einheitlich als Text; echte Fehlwerte bleiben fehlend."""
    return pd.DataFrame(X).apply(
        lambda s: s.map(lambda v: np.nan if pd.isna(v) else str(v)))


warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)
np.random.seed(SEED)

ROOT     = Path(__file__).resolve().parents[2]
DATA_DIR = ROOT / "01_Daten"
OUT_DIR  = ROOT / "03_Tables_Figures"
OUT_DIR.mkdir(parents=True, exist_ok=True)

print(f"[stage3c] OUTER_K={OUTER_K} REPEATS={OUTER_REPEATS} "
      f"BOOTSTRAP_B={BOOTSTRAP_B} LANDMARK={LANDMARK_DAYS}")

# ── Daten ──
static  = pd.read_excel(DATA_DIR / "data_static.xlsx")
dynamic = pd.read_excel(DATA_DIR / "data_dynamic.xlsx")
surv    = pd.read_excel(DATA_DIR / "data_dynamic_surv.xlsx")

mask = ~(static["Rezidivtumor"].astype(str).str.lower() == "ja")
static_an = static.loc[mask].copy()
analysis_pseu = set(static_an["Pseudonym"])
dynamic_an = dynamic[dynamic["Pseudonym"].isin(analysis_pseu)].copy()
surv_an    = surv[surv["Pseudonym"].isin(analysis_pseu)].copy()

at_risk = surv_an[surv_an["time"] > LANDMARK_DAYS].copy()
at_risk["res_time"]  = at_risk["time"] - LANDMARK_DAYS
at_risk["res_event"] = at_risk["event"].astype(int)

def latest_tlv_before(landmark_days):
    d = dynamic_an[(dynamic_an["datasource"] == "volumetry")
                   & dynamic_an["lab_date_POD_imp"].notna()
                   & (dynamic_an["lab_date_POD_imp"] > 0)
                   & (dynamic_an["lab_date_POD_imp"] <= landmark_days)].copy()
    if d.empty:
        return pd.DataFrame(columns=["Pseudonym","TLV_LM","dTLV_FLV_LM"])
    d = d.sort_values(["Pseudonym","lab_date_POD_imp"]) \
         .groupby("Pseudonym").tail(1)
    return d[["Pseudonym","TLV","d_TLV_rel_FLV_base"]].rename(
        columns={"TLV":"TLV_LM","d_TLV_rel_FLV_base":"dTLV_FLV_LM"})

tlv_lm = latest_tlv_before(LANDMARK_DAYS)
feat = static_an.merge(tlv_lm, on="Pseudonym", how="left")
feat = feat.merge(at_risk[["Pseudonym","res_time","res_event"]],
                  on="Pseudonym", how="inner").reset_index(drop=True)

for _col, _cls in [("aetio_masld", "MASLD"), ("aetio_viral", "viral"),
                   ("aetio_ethyltox", "ethyltox")]:
    feat[_col] = feat["aetio_model"].eq(_cls).astype(float) \
                    .mask(feat["aetio_model"].isna())
feat["dm_bin"] = feat["DM"].astype(str).str.lower().map({"ja": 1, "nein": 0})

# ── Feature-Bloecke (kumulativ definiert) ──
def cols_present(want):
    return [c for c in want if c in feat.columns]

blocks_def = [
    ("Routinebefunde", dict(
        num = cols_present(["Alter","BMI","Tumordurchmesser","ChildPugh_Punkte",
                            "n_op_segments","ND_count"]),
        cat = cols_present(["mwd","Leberzirrhose","T","N","M","lesion_multi_bin",
                            "Symptom_cat","DM"]),
        bin = cols_present(["aetio_masld","aetio_viral","aetio_ethyltox",
                            "ND_Nikotin","D_major"]) +
              [c for c in ["D_ND_cat_cardiovascular", "D_ND_cat_metabolic",
                "D_ND_cat_endokrin", "D_ND_cat_substance"]
               if c in feat.columns],
    )),
    ("+ präop Labor", dict(
        num = cols_present(["Bili_preop_val","Albumin_preop","Quick_preop",
                            "deritis_preop","astalb_preop","APRI_preop","ALBI_score",
                            "Leukos_preop", "CRP_preop",
                            "Thrombos_preop"]),
        cat = cols_present(["ALBI_grade"]),
        bin = [],
    )),
    ("+ präop Volumetrie", dict(
        num = cols_present(["FLV","TLV_preop","TTLVR_preop"]),
        cat = [],
        bin = [],
    )),
    ("+ funktionelle Regeneration", dict(
        num = [c for c in feat.columns if c.startswith("slope_")],
        cat = [],
        bin = [],
    )),
    ("+ volumetrische Regeneration", dict(
        num = cols_present(["TLV_LM","dTLV_FLV_LM"]),
        cat = [],
        bin = [],
    )),
]

cumulative_blocks = []
acc = dict(num=[], cat=[], bin=[])
for label, b in blocks_def:
    acc = dict(num = acc["num"] + b["num"],
               cat = acc["cat"] + b["cat"],
               bin = acc["bin"] + b["bin"])
    acc = dict(num = list(dict.fromkeys(acc["num"])),
               cat = list(dict.fromkeys(acc["cat"])),
               bin = list(dict.fromkeys(acc["bin"])))
    cumulative_blocks.append((label, dict(acc)))

for label, b in cumulative_blocks:
    print(f"[stage3c] block '{label}': num={len(b['num'])} "
          f"cat={len(b['cat'])} bin={len(b['bin'])}")

for c in set().union(*[b["num"]+b["bin"] for _, b in cumulative_blocks]):
    if c in feat.columns:
        feat[c] = pd.to_numeric(feat[c], errors="coerce")

y_full = Surv.from_arrays(event=feat["res_event"].astype(bool).values,
                          time=feat["res_time"].astype(float).values)
strata = feat["res_event"].astype(int).values
n_pat  = len(feat)
print(f"[stage3c] at-risk n={n_pat}  events={int(strata.sum())}")

try:
    _ohe = OneHotEncoder(handle_unknown="ignore", sparse_output=False)
except TypeError:
    _ohe = OneHotEncoder(handle_unknown="ignore", sparse=False)

def make_pre(num_, cat_, bin_):
    transformers = []
    if num_:
        transformers.append(("num", Pipeline([
            ("imp", SimpleImputer(strategy="median")),
            ("sc",  StandardScaler())]), num_))
    if cat_:
        transformers.append(("cat", Pipeline([
            ("txt", FunctionTransformer(_kat_als_text)),
                              ("imp", SimpleImputer(strategy="constant",
                                  fill_value="MISSING")),
            ("oh",  _ohe)]), cat_))
    if bin_:
        transformers.append(("bin",
            SimpleImputer(strategy="constant", fill_value=0), bin_))
    if not transformers:
        raise ValueError("empty feature block")
    return ColumnTransformer(transformers)

# ── Nested 5x5-CV OOF risk per block ──
oof_risks_per_block = {}
for label, b in cumulative_blocks:
    cols = b["num"] + b["cat"] + b["bin"]
    print(f"\n[stage3c] >>> Block '{label}' ({len(cols)} Features)")
    X_df = feat[cols]
    risks_all = np.full((OUTER_REPEATS, n_pat), np.nan)
    for rep in range(OUTER_REPEATS):
        cv = StratifiedKFold(n_splits=OUTER_K, shuffle=True,
                             random_state=SEED + 100*rep)
        for fold, (tr, te) in enumerate(cv.split(np.zeros(n_pat), strata)):
            try:
                pre = make_pre(b["num"], b["cat"], b["bin"])
                X_tr = pre.fit_transform(X_df.iloc[tr])
                X_te = pre.transform(X_df.iloc[te])
                rsf = GradientBoostingSurvivalAnalysis(
                    **GBSA_PARAMS, random_state=SEED + rep)
                rsf.fit(X_tr, y_full[tr])
                risks_all[rep, te] = rsf.predict(X_te)
            except Exception as exc:
                print(f"[stage3c]   rep {rep} fold {fold} failed: {exc}")
    mean_risk = np.nanmean(risks_all, axis=0)
    oof_risks_per_block[label] = mean_risk
    keep = ~np.isnan(mean_risk)
    if keep.sum() >= 5:
        c = concordance_index_censored(y_full["event"][keep],
                                       y_full["time"][keep],
                                       mean_risk[keep])[0]
    else:
        c = np.nan
    print(f"[stage3c]   '{label}' OOF C-Index = {c:.3f} "
          f"(n_valid={keep.sum()})")

# ── Bootstrap: C-Index pro Block + paired Delta-C vs Vorblock ──
print("\n[stage3c] Bootstrap (B=%d) ueber Patienten-Resamples ..." % BOOTSTRAP_B)
rng = np.random.default_rng(SEED)
labels = [lab for lab, _ in cumulative_blocks]

C_boot   = {lab: [] for lab in labels}
dC_boot  = {labels[i]: [] for i in range(1, len(labels))}

for _ in range(BOOTSTRAP_B):
    idx = rng.integers(0, n_pat, n_pat)
    y_b = y_full[idx]
    for lab in labels:
        r = oof_risks_per_block[lab][idx]
        keep = ~np.isnan(r)
        if keep.sum() < 5 or y_b["event"][keep].sum() == 0:
            C_boot[lab].append(np.nan); continue
        c = concordance_index_censored(y_b["event"][keep],
                                       y_b["time"][keep], r[keep])[0]
        C_boot[lab].append(c)
    for i in range(1, len(labels)):
        a, p = labels[i], labels[i-1]
        ra = oof_risks_per_block[a][idx]
        rp = oof_risks_per_block[p][idx]
        valid = (~np.isnan(ra)) & (~np.isnan(rp))
        if valid.sum() < 5 or y_b["event"][valid].sum() == 0:
            dC_boot[a].append(np.nan); continue
        ca = concordance_index_censored(y_b["event"][valid],
                                        y_b["time"][valid], ra[valid])[0]
        cp = concordance_index_censored(y_b["event"][valid],
                                        y_b["time"][valid], rp[valid])[0]
        dC_boot[a].append(ca - cp)

def pcl(v, p):
    v = [x for x in v if not np.isnan(x)]
    return float(np.percentile(v, p)) if v else np.nan

# ── Tabelle bauen ──
rows = []
prev_lab = None
for lab in labels:
    keep = ~np.isnan(oof_risks_per_block[lab])
    if keep.sum() >= 5:
        c_pt = concordance_index_censored(y_full["event"][keep],
                                           y_full["time"][keep],
                                           oof_risks_per_block[lab][keep])[0]
    else:
        c_pt = np.nan
    c_lo, c_hi = pcl(C_boot[lab], 2.5), pcl(C_boot[lab], 97.5)
    if prev_lab is None:
        dc_str = ""; sig_str = "--"
    else:
        dc_med = pcl(dC_boot[lab], 50)
        dc_lo, dc_hi = pcl(dC_boot[lab], 2.5), pcl(dC_boot[lab], 97.5)
        dc_str = (f"{dc_med:+.3f} ({dc_lo:+.3f}; {dc_hi:+.3f})"
                  if not np.isnan(dc_med) else "--")
        sig_str = "ja" if (not np.isnan(dc_lo)) and dc_lo > 0 else "nein"
    rows.append(dict(
        stufe = "ABCDE"[labels.index(lab)],
        block = lab,
        c_oof = (f"{c_pt:.3f} ({c_lo:.3f}–{c_hi:.3f})"
                 if not np.isnan(c_pt) else "--"),
        delta_c = dc_str,
        signif  = sig_str,
    ))
    prev_lab = lab

def esc(s):
    s = str(s)
    for ch in "&%$#_{}":
        s = s.replace(ch, "\\" + ch)
    return s

out_path = OUT_DIR / "Tab_3_17_Ablation_Landscape.tex"
with out_path.open("w", encoding="utf-8") as f:
    f.write(f"% Kumulative Ablation Tab 3.17 -- {OUTER_REPEATS}-fach wiederholte {OUTER_K}-Fold-CV (feste Hyperparameter) "
            f"GBSA Landmark POD {LANDMARK_DAYS}; Bootstrap B={BOOTSTRAP_B} "
            f"(paired per patient). Quelle: 02_Code/py/stage3c_nested_cv_ablation.py\n")
    f.write("\\begin{tabular}{llrr}\n\\toprule\n")
    f.write("Block & Merkmale & C-Index & Differenz zum vorigen Block "
            "\\\\\n\\midrule\n")
    for r in rows:
        f.write(" & ".join(esc(r[k]) for k in ("stufe","block","c_oof",
                                                "delta_c"))
                + " \\\\\n")
    f.write("\\bottomrule\n\\end{tabular}\n")
print(f"\n[stage3c] wrote {out_path}")
print("[stage3c] done.")
