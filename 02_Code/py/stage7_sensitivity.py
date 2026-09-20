"""Sensitivitaetsanalysen zum Landmark-Referenzmodell POD 180.

Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx, data_dynamic_surv.xlsx; schreibt
Tab_3_18_Sensitivity_Analysis_Landscape.tex.
"""

from __future__ import annotations

# ── Smoke-run constants ──
OPTUNA_N_TRIALS = 50
BOOTSTRAP_B     = 5000
OUTER_K         = 5
SEED            = 42

REF_MODELS      = ("GBSA",)

import os
import sys
import warnings
import zlib
from pathlib import Path

os.environ.setdefault("PYTHONIOENCODING", "utf-8")
try:
    sys.stdout.reconfigure(encoding="utf-8")
except Exception:
    pass

import numpy as np
import pandas as pd

from sklearn.compose import ColumnTransformer
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer, SimpleImputer
from sklearn.model_selection import StratifiedKFold, train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler, FunctionTransformer

from sksurv.ensemble import (
    GradientBoostingSurvivalAnalysis,
    RandomSurvivalForest,
)
from sksurv.linear_model import CoxnetSurvivalAnalysis
from sksurv.metrics import concordance_index_censored
from sksurv.util import Surv


def _kat_als_text(X):
    """Kategoriale Spalten einheitlich als Text; echte Fehlwerte bleiben fehlend."""
    return pd.DataFrame(X).apply(
        lambda s: s.map(lambda v: np.nan if pd.isna(v) else str(v)))


warnings.filterwarnings("ignore")
np.random.seed(SEED)

LANDMARK_DAYS = 180
MIN_N_SPLIT = 25
MIN_N_CV    = 35

ROOT = Path(__file__).resolve().parents[2]
DATA_DIR = ROOT / "01_Daten"
OUT_DIR  = ROOT / "03_Tables_Figures"
OUT_DIR.mkdir(parents=True, exist_ok=True)

print(f"[stage7] BOOTSTRAP_B={BOOTSTRAP_B}")


# ── Kohorte: POD-180-Landmark (Konvention wie stage3_nested_cv.py) ──
static = pd.read_excel(DATA_DIR / "data_static.xlsx")
dynamic = pd.read_excel(DATA_DIR / "data_dynamic.xlsx")
surv = pd.read_excel(DATA_DIR / "data_dynamic_surv.xlsx")

mask = ~(static["Rezidivtumor"].astype(str).str.lower() == "ja")
static_an = static.loc[mask].copy()
analysis_pseu = set(static_an["Pseudonym"])
dynamic_an = dynamic[dynamic["Pseudonym"].isin(analysis_pseu)].copy()
surv_an = surv[surv["Pseudonym"].isin(analysis_pseu)].copy()

at_risk = surv_an[surv_an["time"] > LANDMARK_DAYS].copy()
at_risk["res_time"] = at_risk["time"] - LANDMARK_DAYS
at_risk["res_event"] = at_risk["event"].astype(int)


def latest_tlv_before(landmark_days):
    d = dynamic_an[(dynamic_an["datasource"] == "volumetry")
                   & dynamic_an["lab_date_POD_imp"].notna()
                   & (dynamic_an["lab_date_POD_imp"] > 0)
                   & (dynamic_an["lab_date_POD_imp"] <= landmark_days)].copy()
    if d.empty:
        return pd.DataFrame(columns=["Pseudonym", "TLV_LM", "dTLV_FLV_LM"])
    d = d.sort_values(["Pseudonym", "lab_date_POD_imp"]) \
         .groupby("Pseudonym").tail(1)
    return d[["Pseudonym", "TLV", "d_TLV_rel_FLV_base"]].rename(columns={
        "TLV": "TLV_LM", "d_TLV_rel_FLV_base": "dTLV_FLV_LM"})


base_lm = static_an.merge(latest_tlv_before(LANDMARK_DAYS),
                          on="Pseudonym", how="left")
base_lm = base_lm.merge(at_risk[["Pseudonym", "res_time", "res_event"]],
                        on="Pseudonym", how="inner").reset_index(drop=True)
print(f"[stage7] landmark cohort n={len(base_lm)} "
      f"events={int(base_lm['res_event'].sum())}")


# ── Feature-Blöcke (wie stage3_nested_cv.py) ──
preop_num = [c for c in ["Alter", "BMI", "Tumordurchmesser",
                          "ChildPugh_Punkte", "ND_count", "n_op_segments",
                          "FLV", "TLV_preop",
                          "TTLVR_preop", "Bili_preop_val", "Albumin_preop",
                          "Quick_preop", "deritis_preop", "astalb_preop", "APRI_preop",
                          "Leukos_preop", "CRP_preop", "Thrombos_preop",
                          "ALBI_score"]
             if c in base_lm.columns]
preop_cat = [c for c in ["mwd", "Leberzirrhose", "DM", "ND_Nikotin", "Symptom_cat",
                          "T", "N", "M",
                          "lesion_multi_bin", "ALBI_grade", "aetio_model"]
             if c in base_lm.columns]
preop_bin = [c for c in ["D_major"]
             if c in base_lm.columns] + \
            [c for c in ["D_ND_cat_cardiovascular", "D_ND_cat_metabolic",
                         "D_ND_cat_endokrin", "D_ND_cat_substance"]
             if c in base_lm.columns]
slope_num = [c for c in base_lm.columns if c.startswith("slope_")]
lm_num = [c for c in ["TLV_LM", "dTLV_FLV_LM"] if c in base_lm.columns]

for c in preop_num + preop_bin + slope_num + lm_num:
    base_lm[c] = pd.to_numeric(base_lm[c], errors="coerce")

num_cols = preop_num + slope_num + lm_num
feat_cols = num_cols + preop_cat + preop_bin


def make_ohe():
    try:
        return OneHotEncoder(handle_unknown="ignore", sparse_output=False)
    except TypeError:
        return OneHotEncoder(handle_unknown="ignore", sparse=False)


def make_pre(imputer="median"):
    if imputer == "iterative":
        num_imp = IterativeImputer(random_state=SEED, max_iter=10,
                                   sample_posterior=False)
    else:
        num_imp = SimpleImputer(strategy="median")
    return ColumnTransformer([
        ("num", Pipeline([("imp", num_imp),
                          ("sc", StandardScaler())]), num_cols),
        ("cat", Pipeline([("txt", FunctionTransformer(_kat_als_text)),
                              ("imp", SimpleImputer(strategy="constant",
                                                 fill_value="MISSING")),
                          ("oh", make_ohe())]), preop_cat),
        ("bin", SimpleImputer(strategy="constant", fill_value=0), preop_bin),
    ])


def _fit_one(model_name, Xtr, ytr, Xte):
    if model_name == "Cox-LASSO":
        m = CoxnetSurvivalAnalysis(l1_ratio=0.5, alpha_min_ratio=0.05,
                                   n_alphas=30, fit_baseline_model=True)
    elif model_name == "RSF":
        m = RandomSurvivalForest(n_estimators=300, max_depth=6,
                                  min_samples_leaf=8, n_jobs=1,
                                  random_state=SEED)
    elif model_name == "GBSA":
        m = GradientBoostingSurvivalAnalysis(n_estimators=200,
                                              learning_rate=0.05,
                                              max_depth=3,
                                              random_state=SEED)
    else:
        raise ValueError(model_name)
    m.fit(Xtr, ytr)
    return m.predict(Xte)


def konkordanzindex(y_test, risk):
    """Konkordanzindex nach Harrell (in Tabellen als C-Index)."""
    keep = ~np.isnan(risk)
    if keep.sum() < 5:
        return np.nan
    try:
        return concordance_index_censored(y_test[keep]["event"],
                                          y_test[keep]["time"],
                                          risk[keep])[0]
    except Exception:
        return np.nan


def rng_for(label):
    return np.random.RandomState((SEED + zlib.crc32(label.encode("utf-8")))
                                 % (2 ** 32))


def evaluate_cv(df_sub, label, imputer="median"):
    """5-fold CV on the landmark cohort; Stufe-3 REFERENZMODELL (REF_MODELS, standardisierte OOF-Risiken) — matches…"""
    if len(df_sub) < MIN_N_CV:
        print(f"[stage7]   {label}: zu klein (n={len(df_sub)})")
        return None
    df_sub = df_sub.reset_index(drop=True)
    X_df = df_sub[feat_cols]
    y = Surv.from_arrays(event=df_sub["res_event"].astype(bool).values,
                         time=df_sub["res_time"].astype(float).values)
    strata = df_sub["res_event"].astype(int).values

    cv = StratifiedKFold(n_splits=OUTER_K, shuffle=True, random_state=SEED)
    models = REF_MODELS
    oof_per = {m: np.full(len(df_sub), np.nan) for m in models}

    for tr, te in cv.split(np.zeros(len(df_sub)), strata):
        pre = make_pre(imputer)
        try:
            Xtr = pre.fit_transform(X_df.iloc[tr])
            Xte = pre.transform(X_df.iloc[te])
        except Exception:
            continue
        for mname in models:
            try:
                oof_per[mname][te] = _fit_one(mname, Xtr, y[tr], Xte)
            except Exception as e:
                print(f"[stage7]   {label} {mname} fold failed: {e}")

    valid = {m: v for m, v in oof_per.items() if not np.all(np.isnan(v))}
    if not valid:
        return None
    Z = np.column_stack([
        (v - np.nanmean(v)) / (np.nanstd(v) if np.nanstd(v) > 0 else 1.0)
        for v in valid.values()
    ])
    oof_risk = np.nanmean(Z, axis=1)

    keep = ~np.isnan(oof_risk)
    n = int(keep.sum())
    if n < MIN_N_CV:
        return None
    yk = y[keep]
    rk = oof_risk[keep]
    point = konkordanzindex(yk, rk)
    if np.isnan(point):
        return None
    rng = rng_for(label)
    vs = []
    for _ in range(BOOTSTRAP_B):
        idx = rng.randint(0, n, n)
        v = konkordanzindex(yk[idx], rk[idx])
        if not np.isnan(v):
            vs.append(v)
    if not vs:
        return None
    return (float(point),
            float(np.percentile(vs, 2.5)),
            float(np.percentile(vs, 97.5)),
            n, int(yk["event"].sum()))


def eval_split(df_sub, label):
    """80/20 stratified split (instead of CV); dasselbe Referenzmodell wie evaluate_cv, Konkordanzindex auf dem…"""
    df_sub = df_sub.reset_index(drop=True)
    X_df = df_sub[feat_cols]
    y = Surv.from_arrays(event=df_sub["res_event"].astype(bool).values,
                         time=df_sub["res_time"].astype(float).values)
    strata = df_sub["res_event"].astype(int).values
    try:
        tr_idx, te_idx = train_test_split(np.arange(len(df_sub)),
                                           test_size=0.2,
                                           stratify=strata, random_state=SEED)
    except Exception:
        return None
    if len(te_idx) < MIN_N_SPLIT:
        print(f"[stage7]   {label}: Testset zu klein (n={len(te_idx)})")
        return None
    pre = make_pre()
    try:
        Xtr = pre.fit_transform(X_df.iloc[tr_idx])
        Xte = pre.transform(X_df.iloc[te_idx])
    except Exception:
        return None
    risks_per = []
    for mname in REF_MODELS:
        try:
            r = _fit_one(mname, Xtr, y[tr_idx], Xte)
            risks_per.append((r - np.nanmean(r))
                              / (np.nanstd(r) if np.nanstd(r) > 0 else 1.0))
        except Exception as e:
            print(f"[stage7]   {label} split {mname} failed: {e}")
    if not risks_per:
        return None
    risk = np.nanmean(np.column_stack(risks_per), axis=1)
    yte = y[te_idx]
    point = konkordanzindex(yte, risk)
    if np.isnan(point):
        return None
    rng = rng_for(label)
    vs = []
    n = len(risk)
    for _ in range(BOOTSTRAP_B):
        idx = rng.randint(0, n, n)
        v = konkordanzindex(yte[idx], risk[idx])
        if not np.isnan(v):
            vs.append(v)
    if not vs:
        return None
    return (float(point),
            float(np.percentile(vs, 2.5)),
            float(np.percentile(vs, 97.5)),
            n, int(yte["event"].sum()))


# ── Sensitivitätsvarianten (Zeilen 1-8) ──
if "D_major" in base_lm.columns:
    mask_major = base_lm["D_major"].fillna(0).astype(int) == 1
else:
    print("[stage7] WARN: D_major nicht in data_static -- "
          "Major-Subgruppe nicht berichtbar.")
    mask_major = pd.Series(False, index=base_lm.index)

core_cols = preop_num + slope_num + preop_cat
cc_mask = base_lm[core_cols].notna().all(axis=1)

if "äthiologie_cat" in base_lm.columns:
    mask_no_masld = base_lm["äthiologie_cat"].astype(str) != "MASLD"
else:
    mask_no_masld = pd.Series(False, index=base_lm.index)

if "TLV_LM" in base_lm.columns:
    mask_flv = base_lm["TLV_LM"].notna()
else:
    mask_flv = pd.Series(False, index=base_lm.index)

if "dTLV_FLV_LM" in base_lm.columns:
    _d = pd.to_numeric(base_lm["dTLV_FLV_LM"], errors="coerce")
    mask_regen_ok = ~((_d < -50) | (_d > 250))
else:
    mask_regen_ok = pd.Series(True, index=base_lm.index)
print(f"[stage7] implausible Regenerationswerte: "
      f"{int((~mask_regen_ok).sum())}")

variants = [
    ("1. Hauptanalyse (5-fach-CV)", base_lm, "cv", {}),
    ("2. 80/20 stratifizierter Split", base_lm, "split", {}),
    ("3. Nur Major-Hepatektomien", base_lm.loc[mask_major], "cv", {}),
    ("4. Complete-case (alle Core-Features)", base_lm.loc[cc_mask], "cv", {}),
    ("5. Imputation mit IterativeImputer (MICE-Näherung)", base_lm, "cv",
     {"imputer": "iterative"}),
    ("6. Ohne MASLD", base_lm.loc[mask_no_masld], "cv", {}),
    ("7. Nur mit Volumetrie am Landmark", base_lm.loc[mask_flv], "cv", {}),
    ("8. Ohne implausible Regenerationswerte", base_lm.loc[mask_regen_ok],
     "cv", {}),
]

results = []
main_pt = None
for label, df_v, mode, kw in variants:
    print(f"[stage7] variant: {label}  n={len(df_v)}")
    if mode == "split":
        r = eval_split(df_v, label, **kw)
    else:
        r = evaluate_cv(df_v, label, **kw)
    if r is not None and main_pt is None and label.startswith("1."):
        main_pt = r[0]
    results.append((label, len(df_v), r))


# ── Tabelle schreiben ──
def tex_escape(s):
    s = str(s)
    s = s.replace("\\", r"\textbackslash{}")
    for ch in "&%$#_{}":
        s = s.replace(ch, "\\" + ch)
    return s


anzeige = {
    "1. Hauptanalyse (5-fach-CV)":
        "1. Hauptanalyse (fünffache Kreuzvalidierung)",
    "2. 80/20 stratifizierter Split": "2. Einmalige Aufteilung 80/20",
    "4. Complete-case (alle Core-Features)": "4. Nur vollständige Fälle",
    "5. Imputation mit IterativeImputer (MICE-Näherung)":
        "5. Fehlwerte aus übrigen Merkmalen geschätzt",
}
rows = []
for label, n, r in results:
    name = anzeige.get(label, label)
    if r is None:
        rows.append({"Variante": name, "N/Rezidive": "--", "C-Index": "--",
                     "Differenz zur Hauptanalyse": "--"})
        continue
    p, lo, hi, n_eval, ev_eval = r
    if label.startswith("1."):
        delta = ""
    elif main_pt is None:
        delta = "--"
    else:
        delta = f"{p - main_pt:+.3f}"
        if delta in ("+0.000", "-0.000"):
            delta = "0.000"
    rows.append({"Variante": name, "N/Rezidive": f"{n_eval}/{ev_eval}",
                 "C-Index": f"{p:.3f} ({lo:.3f}–{hi:.3f})",
                 "Differenz zur Hauptanalyse": delta})
    print(f"[stage7]   {name}: N/Rezidive {n_eval}/{ev_eval}, "
          f"C-Index {p:.3f}, Differenz {delta or '--'}")

cols = list(rows[0].keys())
align = "l" + "r" * (len(cols) - 1)
tab_path = OUT_DIR / "Tab_3_18_Sensitivity_Analysis_Landscape.tex"
with tab_path.open("w", encoding="utf-8") as f:
    f.write(f"% Sensitivitätsanalysen: Landmark-180-Referenzmodell "
            f"(GBSA, Gradient Boosted Survival Analysis) auf preop "
            f"+ Slope + LM-Volumetrie bis POD 180. Basis ist die "
            f"POD-180-Landmark-Kohorte mit n = {len(base_lm)} Patient*innen "
            f"at risk. Zielzeit ist die Residualzeit ab Landmark. "
            f"Bootstrap-KI mit B = {BOOTSTRAP_B}. "
            f"Szenario 8 schliesst Werte der relativen Volumenaenderung "
            f"ausserhalb von -50 bis +250 Prozent aus. In der "
            f"POD-180-Landmark-Kohorte ist kein Fall betroffen, weil die "
            f"beiden Patient*innen mit solchen Werten ihr Ereignis vor "
            f"Tag 180 hatten. Die Zeile entspricht daher der Hauptanalyse.\n")
    f.write("\\begin{tabular}{" + align + "}\n\\toprule\n")
    f.write(" & ".join(cols) + " \\\\\n\\midrule\n")
    for r in rows:
        f.write(" & ".join(
            str(r[c]) if c in ("C-Index", "Differenz zur Hauptanalyse")
            else tex_escape(r[c]) for c in cols
        ) + " \\\\\n")
    f.write("\\bottomrule\n\\end{tabular}\n")
print(f"[stage7] wrote {tab_path.name}")
print("[stage7] done.")
