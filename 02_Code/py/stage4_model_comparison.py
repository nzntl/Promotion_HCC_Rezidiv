"""Konsolidierter Modellvergleich aus den Out-of-Fold-Risikoscores von Stufe 2 und 3 und dem Cox-LASSO des R-Strangs.

Schreibt Tab_3_15_Consolidated_Comparison.tex und _cache/stage4_risks.npz.
"""

from __future__ import annotations

# ── Smoke-run constants ──
OPTUNA_N_TRIALS = 50
BOOTSTRAP_B     = 5000
OUTER_K         = 5
SEED            = 42

import os
import sys
import warnings
from pathlib import Path

os.environ.setdefault("PYTHONIOENCODING", "utf-8")
try:
    sys.stdout.reconfigure(encoding="utf-8")
except Exception:
    pass

import numpy as np
import pandas as pd

from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.model_selection import StratifiedKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler

from sksurv.linear_model import CoxPHSurvivalAnalysis
from sksurv.metrics import (
    brier_score,
    concordance_index_censored,
    concordance_index_ipcw,
    cumulative_dynamic_auc,
)
from sksurv.util import Surv

warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=PendingDeprecationWarning)
warnings.filterwarnings("ignore", category=UserWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)
np.random.seed(SEED)

ROOT = Path(__file__).resolve().parents[2]
DATA_DIR  = ROOT / "01_Daten"
OUT_DIR   = ROOT / "03_Tables_Figures"
CACHE_DIR = ROOT / "02_Code" / "py" / "_cache"
OUT_DIR.mkdir(parents=True, exist_ok=True)
CACHE_DIR.mkdir(parents=True, exist_ok=True)

print(f"[stage4mc] BOOTSTRAP_B={BOOTSTRAP_B}")


# ── Load cohort + cached OOF risks ──
static = pd.read_excel(DATA_DIR / "data_static.xlsx")
surv = pd.read_excel(DATA_DIR / "data_dynamic_surv.xlsx")
mask = ~(static["Rezidivtumor"].astype(str).str.lower() == "ja")
df = static.loc[mask].merge(surv, on="Pseudonym", how="inner") \
                   .dropna(subset=["time", "event"]).reset_index(drop=True)
y_struct = Surv.from_arrays(event=df["event"].astype(bool).values,
                            time=df["time"].astype(float).values)
print(f"[stage4mc] cohort n={len(df)} events={int(df['event'].sum())}")


def load_npz(path):
    if not path.exists():
        print(f"[stage4mc] missing {path.name} — skip")
        return None
    return np.load(path, allow_pickle=True)


cache2  = load_npz(CACHE_DIR / "stage2_oof_risk.npz")
cache90 = load_npz(CACHE_DIR / "stage3_pod90.npz")
cache180 = load_npz(CACHE_DIR / "stage3_pod180.npz")

risks_aligned = {}


def align_to_df(pseu_sub, vals_sub):
    out = np.full(len(df), np.nan)
    df_pseu = df["Pseudonym"].values
    pmap = {p: i for i, p in enumerate(df_pseu)}
    for p, v in zip(pseu_sub, vals_sub):
        if p in pmap:
            out[pmap[p]] = v
    return out


REF_MODEL = "GBSA"


def _add_ref_row(cache, label):
    """Nimmt REF_MODEL aus dem Cache und meldet, wenn es fehlt."""
    if cache is None:
        return
    if REF_MODEL not in cache.files:
        print(f"[stage4mc] WARN: {REF_MODEL} fehlt in {label}, Zeile entfaellt")
        return
    aligned = align_to_df(cache["pseudonyms"], cache[REF_MODEL])
    ok = ~np.isnan(aligned)
    if ok.sum() < 10:
        print(f"[stage4mc] WARN: {label} hat zu wenige Werte, Zeile entfaellt")
        return
    c = concordance_index_censored(
        y_struct["event"][ok], y_struct["time"][ok], aligned[ok])[0]
    print(f"[stage4mc]   {label:28s} C={c:.3f}")
    risks_aligned[label] = aligned


if cache2 is not None:
    pseu2 = cache2["pseudonyms"]
    for k in cache2.files:
        if k in ("pseudonyms", "time", "event", REF_MODEL):
            continue
        aligned = align_to_df(pseu2, cache2[k])
        ok = ~np.isnan(aligned)
        if ok.sum() < 10:
            continue
        c = concordance_index_censored(
            y_struct["event"][ok], y_struct["time"][ok], aligned[ok])[0]
        print(f"[stage4mc]   (nur Log) Ausgang {k:10s} C={c:.3f}")

_add_ref_row(cache2, f"Ausgangsmodell ({REF_MODEL})")
_add_ref_row(cache90, f"Landmark POD 90 ({REF_MODEL})")
_add_ref_row(cache180, f"Landmark POD 180 ({REF_MODEL})")


# ── Cox models A / B / C — refit in OUTER_K-fold CV to get OOF linear predictors ──
def build_feature_block(df, names):
    """Return X (numpy) and column descriptor for selected names."""
    X = df[names].copy()
    for c in X.columns:
        X[c] = pd.to_numeric(X[c], errors="coerce")
    return X


# ── Plan-Architektur der Cox-Modelle ──
df["_sex_m"]         = df["mwd"].astype(str).str.lower().eq("m").astype(int)
df["_cirrhosis_b"]   = df["Leberzirrhose"].astype(str).str.lower().eq("ja").astype(int)
df["_lesion_multi_b"] = pd.to_numeric(
    df.get("lesion_multi_bin", pd.Series([np.nan]*len(df))),
    errors="coerce")
df["_LVR_index"]     = pd.to_numeric(df.get("TLV_first_postop"),
                                     errors="coerce") / \
                       pd.to_numeric(df.get("FLV"), errors="coerce")
df["_FLV_TLV_ratio"] = pd.to_numeric(df.get("FLV"), errors="coerce") / \
                       pd.to_numeric(df.get("TLV_preop"), errors="coerce")
for _col, _cls in [("aetio_masld", "MASLD"), ("aetio_viral", "viral"),
                   ("aetio_ethyltox", "ethyltox")]:
    df[_col] = df["aetio_model"].eq(_cls).astype(float) \
                  .mask(df["aetio_model"].isna())
df["dm_bin"]         = df["DM"].astype(str).str.lower() \
                          .map({"ja": 1, "nein": 0})
df["nikotin_bin"]    = pd.to_numeric(df.get("ND_Nikotin"), errors="coerce")

cox_a_feats = [c for c in [
    "Alter", "_sex_m", "_cirrhosis_b",
    "_lesion_multi_b", "Tumordurchmesser",
    "aetio_masld", "aetio_viral", "aetio_ethyltox",
    "dm_bin", "nikotin_bin"] if c in df.columns]
cox_b_feats = cox_a_feats + [c for c in [
    "Bili_preop_val", "Albumin_preop", "Quick_preop",
    "deritis_preop", "astalb_preop",
    "APRI_preop", "ALBI_score"] if c in df.columns]
cox_c_feats = cox_b_feats + [c for c in [
    "_FLV_TLV_ratio", "TTLVR_preop"] if c in df.columns]
cox_d_feats = cox_c_feats + [c for c in [
    "TLV_first_postop", "_LVR_index",
    "slope_Quick_POD1_7",
    "slope_GOT_POD1_7", "slope_GPT_POD1_7",
    "slope_CRP_POD1_7", "slope_Albumin_POD1_7",
    "slope_Thrombos_POD1_7"] if c in df.columns]

leberzirrhose_dummy = (df.get("Leberzirrhose",
                              pd.Series(["nein"]*len(df))).astype(str)
                       .str.lower().eq("ja").astype(int)).values


def cox_cv_lp(feats, label):
    """Return OOF linear predictor for a Cox model trained on `feats` in OUTER_K-fold CV."""
    if not feats:
        return None
    X = build_feature_block(df, feats).values
    strata = ((df["event"] == 1) & (df["time"] <= 730)).astype(int).values
    cv = StratifiedKFold(n_splits=OUTER_K, shuffle=True, random_state=SEED)
    lp = np.full(len(df), np.nan)
    for tr, te in cv.split(np.zeros(len(df)), strata):
        Xtr = X[tr].copy()
        Xte = X[te].copy()
        med = np.nanmedian(Xtr, axis=0)
        for j in range(Xtr.shape[1]):
            Xtr[np.isnan(Xtr[:, j]), j] = med[j]
            Xte[np.isnan(Xte[:, j]), j] = med[j]
        sc_mu = Xtr.mean(axis=0)
        sc_sd = Xtr.std(axis=0) + 1e-9
        Xtr = (Xtr - sc_mu) / sc_sd
        Xte = (Xte - sc_mu) / sc_sd
        try:
            m = CoxPHSurvivalAnalysis(alpha=0.01)
            m.fit(Xtr, y_struct[tr])
            lp[te] = m.predict(Xte)
        except Exception as e:
            print(f"[stage4mc]   Cox {label} fold failed: {e}")
    return lp


# ── Cox-OOF-Source: prefer Cox-LASSO OOF-LPs from R (06d_Cox_LASSO_Main.R) ──
lasso_csv = CACHE_DIR / "cox_lasso_oof_lp.csv"
cox_lasso_loaded = False
if lasso_csv.exists():
    try:
        lasso_df = pd.read_csv(lasso_csv)
        pmap = {p: i for i, p in enumerate(df["Pseudonym"].values)}
        for src_col, label in [
            ("oof_lp_A_lasso", "Cox A LASSO (Routinebefunde)"),
            ("oof_lp_B_lasso", "Cox B LASSO (+ präop Labor)"),
            ("oof_lp_C_lasso", "Cox C LASSO (+ präop Volumetrie)"),
            ("oof_lp_D_lasso", "Cox D LASSO (+ Regeneration)"),
        ]:
            if src_col not in lasso_df.columns:
                continue
            lp = np.full(len(df), np.nan)
            for p, v in zip(lasso_df["Pseudonym"].values,
                            lasso_df[src_col].values):
                if p in pmap and not pd.isna(v):
                    lp[pmap[p]] = float(v)
            risks_aligned[label] = lp
            keep = ~np.isnan(lp)
            c = concordance_index_censored(y_struct["event"][keep],
                                           y_struct["time"][keep],
                                           lp[keep])[0]
            print(f"[stage4mc]   {label:34s} OOF C={c:.3f} (LASSO)")
        cox_lasso_loaded = True
        print(f"[stage4mc] Cox-LASSO OOF-LPs aus {lasso_csv.name} geladen")
    except Exception as e:
        print(f"[stage4mc] WARN: Cox-LASSO-CSV-Read fehlgeschlagen: {e}")
        cox_lasso_loaded = False

if not cox_lasso_loaded:
    print("[stage4mc] kein cox_lasso_oof_lp.csv -> Fallback auf sksurv-Cox-CV")
    for label, feats in [("Cox A (Routinebefunde)",          cox_a_feats),
                         ("Cox B (+ präop Labor)", cox_b_feats),
                         ("Cox C (+ präop Volumetrie)",      cox_c_feats),
                         ("Cox D (+ Regeneration)",     cox_d_feats)]:
        lp = cox_cv_lp(feats, label)
        if lp is not None:
            risks_aligned[label] = lp
            keep = ~np.isnan(lp)
            c = concordance_index_censored(y_struct["event"][keep],
                                           y_struct["time"][keep], lp[keep])[0]
            print(f"[stage4mc]   {label:28s} OOF C={c:.3f}")


# ── Metrics with bootstrap CI ──
AUC_TIMES = np.array([365.0, 730.0, 1095.0])


def fmt_ci(p, lo, hi, ndigits=3):
    if np.isnan(p):
        return "--"
    return f"{p:.{ndigits}f} ({lo:.{ndigits}f}–{hi:.{ndigits}f})"


def boot_metric(metric_fn, n, rng):
    vs = []
    for _ in range(BOOTSTRAP_B):
        idx = rng.randint(0, n, n)
        try:
            v = metric_fn(idx)
            if v is not None and not np.isnan(v):
                vs.append(v)
        except Exception:
            continue
    if not vs:
        return (np.nan, np.nan, np.nan)
    try:
        point = float(metric_fn(np.arange(n)))
        if np.isnan(point):
            point = float(np.mean(vs))
    except Exception:
        point = float(np.mean(vs))
    return (point, float(np.percentile(vs, 2.5)),
            float(np.percentile(vs, 97.5)))


rng = np.random.RandomState(SEED)


def all_metrics(name, risk):
    keep = ~np.isnan(risk)
    if keep.sum() < 30:
        return {"Modell": name, **{k: "--" for k in
                ["C-Index", "Uno-C", "AUC 12 Mo", "AUC 24 Mo", "AUC 36 Mo"]}}

    risk_kept = risk[keep]
    y_kept = y_struct[keep]
    n = len(risk_kept)
    yk_idx = np.arange(n)

    def f_harrell(idx):
        return concordance_index_censored(y_kept["event"][idx],
                                          y_kept["time"][idx],
                                          risk_kept[idx])[0]

    def f_uno(idx):
        try:
            return concordance_index_ipcw(y_kept[idx], y_kept[idx],
                                          risk_kept[idx], tau=1095.0)[0]
        except Exception:
            return np.nan

    def f_auc_at(idx, kk):
        try:
            a, _ = cumulative_dynamic_auc(y_kept[idx], y_kept[idx],
                                          risk_kept[idx], AUC_TIMES)
            return a[kk]
        except Exception:
            return np.nan

    pH = boot_metric(f_harrell, n, rng)
    pU = boot_metric(f_uno, n, rng)
    aucs = [boot_metric(lambda idx, kk=kk: f_auc_at(idx, kk), n, rng)
            for kk in range(3)]

    return {"Modell": name,
            "C-Index": fmt_ci(*pH),
            "Uno-C": fmt_ci(*pU),
            "AUC 12 Mo": fmt_ci(*aucs[0]),
            "AUC 24 Mo": fmt_ci(*aucs[1]),
            "AUC 36 Mo": fmt_ci(*aucs[2])}


model_order = []
for k in ["Cox A LASSO (Routinebefunde)", "Cox B LASSO (+ präop Labor)",
          "Cox C LASSO (+ präop Volumetrie)", "Cox D LASSO (+ Regeneration)",
          "Cox A (Routinebefunde)", "Cox B (+ präop Labor)",
          "Cox C (+ präop Volumetrie)", "Cox D (+ Regeneration)"]:
    if k in risks_aligned:
        model_order.append(k)
for k in list(risks_aligned.keys()):
    if k not in model_order:
        model_order.append(k)

rows = [all_metrics(name, risks_aligned[name]) for name in model_order]


# ── Cache + emit ──
np.savez(CACHE_DIR / "stage4_risks.npz",
         pseudonyms=df["Pseudonym"].values,
         time=df["time"].values, event=df["event"].values,
         model_names=np.array(model_order, dtype=object),
         **{k.replace(" ", "_"): v for k, v in risks_aligned.items()})
print(f"[stage4mc] cached stage4_risks.npz")


def tex_escape(s):
    s = str(s)
    s = s.replace("\\", r"\textbackslash{}")
    for ch in "&%$#_{}":
        s = s.replace(ch, "\\" + ch)
    return s


cols = ["Modell", "C-Index", "AUC 24 Mo"]
align = "l" + "r" * (len(cols) - 1)
tab_path = OUT_DIR / "Tab_3_15_Consolidated_Comparison.tex"
with tab_path.open("w", encoding="utf-8") as f:
    f.write("% Konsolidierter Modellvergleich. Cox A/B/C/D: penalisierte "
            "Cox-LASSO, verschachtelte 10x10-CV OOF-LP (aus 06d). "
            "Praeop-Modell und Landmark-Modelle: nested 5x5-CV OOF-Risiko-Scores. "
            f"Bootstrap-KI B={BOOTSTRAP_B}.\n")
    f.write("\\begin{tabular}{" + align + "}\n\\toprule\n")
    f.write(" & ".join(cols) + " \\\\\n\\midrule\n")
    for r in rows:
        f.write(" & ".join(tex_escape(r[c]) for c in cols) + " \\\\\n")
    f.write("\\bottomrule\n\\end{tabular}\n")
print(f"[stage4mc] wrote {tab_path.name}")

rows_csv = CACHE_DIR / "stage4_comparison_rows.csv"
pd.DataFrame(rows)[cols].to_csv(rows_csv, index=False, encoding="utf-8")
print(f"[stage4mc] wrote {rows_csv.name}")
print("[stage4mc] done.")
