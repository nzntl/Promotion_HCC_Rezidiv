"""Ausgangsmodell: verschachtelte 5x5-Kreuzvalidierung mit Optuna-Tuning und Bootstrap-Konfidenzintervallen.

Liest 01_Daten/data_static.xlsx, data_dynamic_surv.xlsx; schreibt
Tab_3_10_Stage2_ML_Performance_Landscape.tex, _cache/stage2_oof_risk.npz, stage2_hyperparams_final.json.
"""

from __future__ import annotations

# ── Smoke-run constants (override to FULL values for the over-night run) ──
OPTUNA_N_TRIALS = 50
BOOTSTRAP_B     = 5000
OUTER_K         = 5
INNER_K         = 5
SEED            = 42

import json
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
from sklearn.preprocessing import OneHotEncoder, StandardScaler, FunctionTransformer

from sksurv.ensemble import GradientBoostingSurvivalAnalysis, RandomSurvivalForest
from sksurv.linear_model import CoxnetSurvivalAnalysis
from sksurv.metrics import (
    concordance_index_censored,
    concordance_index_ipcw,
    cumulative_dynamic_auc,
    integrated_brier_score,
)
from sksurv.util import Surv

import optuna
optuna.logging.set_verbosity(optuna.logging.WARNING)


def _kat_als_text(X):
    """Kategoriale Spalten einheitlich als Text; echte Fehlwerte bleiben fehlend."""
    return pd.DataFrame(X).apply(
        lambda s: s.map(lambda v: np.nan if pd.isna(v) else str(v)))


warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)

np.random.seed(SEED)


# ── Paths ──
ROOT = Path(__file__).resolve().parents[2]
DATA_DIR  = ROOT / "01_Daten"
OUT_DIR   = ROOT / "03_Tables_Figures"
CACHE_DIR = ROOT / "02_Code" / "py" / "_cache"
OUT_DIR.mkdir(parents=True, exist_ok=True)
CACHE_DIR.mkdir(parents=True, exist_ok=True)

print(f"[stage2cv] SMOKE? OPTUNA_N_TRIALS={OPTUNA_N_TRIALS} "
      f"BOOTSTRAP_B={BOOTSTRAP_B}")
print(f"[stage2cv] ROOT      = {ROOT}")
print(f"[stage2cv] OUT_DIR   = {OUT_DIR}")
print(f"[stage2cv] CACHE_DIR = {CACHE_DIR}")


# ── Load cohort ──
static = pd.read_excel(DATA_DIR / "data_static.xlsx")
surv = pd.read_excel(DATA_DIR / "data_dynamic_surv.xlsx")
mask = ~(static["Rezidivtumor"].astype(str).str.lower() == "ja")
df = static.loc[mask].merge(surv, on="Pseudonym", how="inner") \
                   .dropna(subset=["time", "event"]).reset_index(drop=True)
print(f"[stage2cv] analysis cohort: n={len(df)} events={int(df['event'].sum())}")


num_feats = ["Alter", "BMI", "Tumordurchmesser",
             "ChildPugh_Punkte", "ND_count", "n_op_segments",
              "FLV", "TLV_preop",
             "TTLVR_preop", "Bili_preop_val", "Albumin_preop",
             "Quick_preop", "deritis_preop", "astalb_preop",
             "APRI_preop", "ALBI_score", "Leukos_preop", "CRP_preop",
             "Thrombos_preop"]
cat_feats = ["mwd", "Leberzirrhose", "DM", "ND_Nikotin", "Symptom_cat", "T", "N", "M",
             "lesion_multi_bin", "ALBI_grade", "aetio_model"]
seg_feats = ["D_major"]
ND_CAT_FEATS = ["D_ND_cat_cardiovascular", "D_ND_cat_metabolic",
                "D_ND_cat_endokrin", "D_ND_cat_substance"]
nd_cat_feats = [c for c in ND_CAT_FEATS if c in df.columns]
feat_cols = [c for c in (num_feats + cat_feats + seg_feats + nd_cat_feats)
             if c in df.columns]

X_df = df[feat_cols].copy()
for c in num_feats + seg_feats + nd_cat_feats:
    if c in X_df.columns:
        X_df[c] = pd.to_numeric(X_df[c], errors="coerce")

num_in_X = [c for c in num_feats if c in X_df.columns]
cat_in_X = [c for c in cat_feats if c in X_df.columns]
bin_in_X = [c for c in seg_feats + nd_cat_feats if c in X_df.columns]

try:
    _ohe = OneHotEncoder(handle_unknown="ignore", sparse_output=False)
except TypeError:
    _ohe = OneHotEncoder(handle_unknown="ignore", sparse=False)


def make_preprocessor():
    return ColumnTransformer([
        ("num", Pipeline([("imp", SimpleImputer(strategy="median")),
                          ("sc", StandardScaler())]), num_in_X),
        ("cat", Pipeline([("txt", FunctionTransformer(_kat_als_text)),
                              ("imp", SimpleImputer(strategy="constant",
                                                fill_value="MISSING")),
                          ("oh", _ohe)]), cat_in_X),
        ("bin", SimpleImputer(strategy="constant", fill_value=0), bin_in_X),
    ])


y_struct = Surv.from_arrays(event=df["event"].astype(bool).values,
                            time=df["time"].astype(float).values)
print(f"[stage2cv] feature columns ({len(feat_cols)})")


# ── Metrics ──
AUC_TIMES = np.array([365.0, 730.0, 1095.0])


def harrell(y, risk):
    keep = ~np.isnan(risk)
    if keep.sum() < 5:
        return np.nan
    return concordance_index_censored(y["event"][keep], y["time"][keep],
                                      risk[keep])[0]


def uno(y_train, y_test, risk, tau):
    keep = ~np.isnan(risk)
    if keep.sum() < 5:
        return np.nan
    try:
        return concordance_index_ipcw(y_train, y_test[keep], risk[keep],
                                      tau=tau)[0]
    except Exception:
        return np.nan


def td_auc(y_train, y_test, risk, times):
    keep = ~np.isnan(risk)
    if keep.sum() < 5:
        return [np.nan] * len(times)
    try:
        aucs, _ = cumulative_dynamic_auc(y_train, y_test[keep], risk[keep],
                                         times)
        return list(aucs)
    except Exception:
        return [np.nan] * len(times)


def safe_ibs(y_train, y_test, surv_fn_matrix, eval_times):
    try:
        return integrated_brier_score(y_train, y_test, surv_fn_matrix,
                                      eval_times)
    except Exception:
        return np.nan


# ── Optuna objectives per model — minimise (1 - C-Index) on inner-CV ──
def fit_predict_coxnet(params, Xtr, ytr, Xte):
    m = CoxnetSurvivalAnalysis(
        l1_ratio=params["l1_ratio"],
        alpha_min_ratio=params["alpha_min_ratio"],
        n_alphas=30, fit_baseline_model=True,
    )
    m.fit(Xtr, ytr)
    return m, m.predict(Xte)


def fit_predict_rsf(params, Xtr, ytr, Xte):
    m = RandomSurvivalForest(
        n_estimators=int(params["n_estimators"]),
        max_depth=int(params["max_depth"]),
        min_samples_leaf=int(params["min_samples_leaf"]),
        n_jobs=1, random_state=SEED,
    )
    m.fit(Xtr, ytr)
    return m, m.predict(Xte)


def fit_predict_gbsa(params, Xtr, ytr, Xte):
    m = GradientBoostingSurvivalAnalysis(
        n_estimators=int(params["n_estimators"]),
        learning_rate=params["learning_rate"],
        max_depth=int(params["max_depth"]),
        random_state=SEED)
    m.fit(Xtr, ytr)
    return m, m.predict(Xte)


MODEL_FIT = {
    "Cox-LASSO": fit_predict_coxnet,
    "RSF":       fit_predict_rsf,
    "GBSA":      fit_predict_gbsa,
}


def suggest_params(trial, model):
    if model == "Cox-LASSO":
        return {"l1_ratio": trial.suggest_float("l1_ratio", 0.1, 1.0),
                "alpha_min_ratio": trial.suggest_float("alpha_min_ratio",
                                                       0.01, 0.2, log=True)}
    if model == "RSF":
        return {"n_estimators": trial.suggest_int("n_estimators", 100, 400,
                                                   step=100),
                "max_depth": trial.suggest_int("max_depth", 3, 8),
                "min_samples_leaf": trial.suggest_int("min_samples_leaf",
                                                       4, 20)}

    if model == "GBSA":
        return {"n_estimators": trial.suggest_int("n_estimators", 100, 400,
                                                   step=100),
                "learning_rate": trial.suggest_float("learning_rate", 0.02,
                                                      0.2, log=True),
                "max_depth": trial.suggest_int("max_depth", 2, 5)}
    raise ValueError(model)


# ── Wiedergabe-Modus (optional) ──
# HCC_HP_REPLAY=1 uebernimmt die je Fold gewaehlten Hyperparameter des Referenzlaufs aus
# 02_Code/py/referenz/ (ersatzweise 02_Code/py/) und ueberspringt die Optuna-Suche.
# Ohne die Variable laeuft die Suche unveraendert wie im Referenzlauf.
def _lade_hp_replay(dateiname):
    import os, json
    from pathlib import Path as _P
    if os.environ.get("HCC_HP_REPLAY") != "1":
        return None
    basis = _P(__file__).resolve().parent
    for pfad in (basis / "referenz" / dateiname, basis / dateiname):
        if pfad.exists():
            with pfad.open(encoding="utf-8") as f:
                roh = json.load(f)
            print(f"[replay] Hyperparameter aus {pfad.relative_to(basis)}, keine Optuna-Suche")
            return {m: {e["outer_fold"]: {k: v for k, v in e.items() if k != "outer_fold"}
                        for e in lst} for m, lst in roh.items()}
    raise FileNotFoundError(f"HCC_HP_REPLAY=1, aber {dateiname} fehlt in py/referenz/ und py/")

HP_REPLAY = _lade_hp_replay("stage2_hyperparams_final.json")


# ── Nested CV ──
strata = df["event"].astype(int).values
outer_cv = StratifiedKFold(n_splits=OUTER_K, shuffle=True, random_state=SEED)

oof_risk = {m: np.full(len(df), np.nan) for m in MODEL_FIT}
hp_log = {m: [] for m in MODEL_FIT}

for outer_i, (tr_idx, te_idx) in enumerate(outer_cv.split(np.zeros(len(df)),
                                                          strata)):
    print(f"[stage2cv] outer fold {outer_i+1}/{OUTER_K} "
          f"(train n={len(tr_idx)} test n={len(te_idx)})")
    pre = make_preprocessor()
    X_tr = pre.fit_transform(X_df.iloc[tr_idx])
    X_te = pre.transform(X_df.iloc[te_idx])
    y_tr = y_struct[tr_idx]
    y_te = y_struct[te_idx]

    inner_strata = strata[tr_idx]

    for model in MODEL_FIT:
        def objective(trial, model=model, X_tr=X_tr, y_tr=y_tr,
                      inner_strata=inner_strata):
            params = suggest_params(trial, model)
            cv = StratifiedKFold(n_splits=INNER_K, shuffle=True,
                                 random_state=SEED + trial.number)
            cs = []
            for itr, ite in cv.split(np.zeros(len(y_tr)), inner_strata):
                try:
                    _, pred = MODEL_FIT[model](params, X_tr[itr], y_tr[itr],
                                               X_tr[ite])
                except Exception:
                    return 1.0
                c = harrell(y_tr[ite], pred)
                if np.isnan(c):
                    return 1.0
                cs.append(c)
            return 1.0 - float(np.mean(cs))

        if HP_REPLAY is not None:
            best = dict(HP_REPLAY[model][outer_i])
        else:
            study = optuna.create_study(direction="minimize",
                                        sampler=optuna.samplers.TPESampler(
                                            seed=SEED + outer_i))
            study.optimize(objective, n_trials=OPTUNA_N_TRIALS, show_progress_bar=False)
            best = study.best_params
        hp_log[model].append({"outer_fold": outer_i, **best})

        try:
            _, pred = MODEL_FIT[model](best, X_tr, y_tr, X_te)
            oof_risk[model][te_idx] = pred
            c_outer = harrell(y_te, pred)
            print(f"[stage2cv]   {model:9s} best={best} test-C={c_outer:.3f}")
        except Exception as e:
            print(f"[stage2cv]   {model:9s} FAILED on outer fit: {e}")


# ── Bootstrap CIs on OOF risk scores ──
rng = np.random.RandomState(SEED)


def bootstrap_metric(risk, fn):
    vals = []
    n = len(risk)
    for _ in range(BOOTSTRAP_B):
        idx = rng.randint(0, n, n)
        try:
            v = fn(idx)
            if not (v is None or (isinstance(v, float) and np.isnan(v))):
                vals.append(v)
        except Exception:
            continue
    if not vals:
        return (np.nan, np.nan, np.nan)
    try:
        point = float(fn(np.arange(n)))
        if np.isnan(point):
            point = float(np.mean(vals))
    except Exception:
        point = float(np.mean(vals))
    return (point,
            float(np.percentile(vals, 2.5)),
            float(np.percentile(vals, 97.5)))


def fmt_ci(point, lo, hi, ndigits=3):
    if np.isnan(point):
        return "--"
    return f"{point:.{ndigits}f} ({lo:.{ndigits}f}–{hi:.{ndigits}f})"


_ens_members = ["Cox-LASSO", "RSF", "GBSA"]
_ens_valid = {k: oof_risk[k] for k in _ens_members
              if k in oof_risk and not np.all(np.isnan(oof_risk[k]))}
if len(_ens_valid) >= 2:
    _Z = np.column_stack([
        (v - np.nanmean(v)) / (np.nanstd(v) if np.nanstd(v) > 0 else 1)
        for v in _ens_valid.values()
    ])
    oof_risk["Ensemble"] = np.nanmean(_Z, axis=1)
    print(f"[stage2cv] Ensemble aus {len(_ens_valid)} Modellen: "
          f"{', '.join(_ens_valid)}")
else:
    print("[stage2cv] WARN: Ensemble uebersprungen, zu wenige gueltige Modelle")

rows = []
for model, risk in oof_risk.items():
    if np.all(np.isnan(risk)):
        rows.append({"Modell": model, **{k: "--" for k in
                     ["C-Index", "Uno-C", "AUC 12 Mo", "AUC 24 Mo",
                      "AUC 36 Mo", "IBS", "In-sample C", "Abstand"]}})
        continue

    def f_harrell(idx, risk=risk):
        return harrell(y_struct[idx], risk[idx])

    def f_uno(idx, risk=risk):
        return uno(y_struct[idx], y_struct[idx], risk[idx], tau=730.0)

    def f_td(idx, risk=risk, t=AUC_TIMES):
        return td_auc(y_struct[idx], y_struct[idx], risk[idx], t)

    pH, lH, hH = bootstrap_metric(risk, f_harrell)
    pU, lU, hU = bootstrap_metric(risk, f_uno)

    def f_auc_at(idx, kk, risk=risk):
        a = f_td(idx)
        return a[kk] if not (a is None or len(a) <= kk) else np.nan
    auc_strs = []
    for kk in range(3):
        p, lo, hi = bootstrap_metric(risk, lambda idx, kk=kk: f_auc_at(idx, kk))
        auc_strs.append(fmt_ci(p, lo, hi))

    insample_c = np.nan
    try:
        pre_full = make_preprocessor()
        X_full = pre_full.fit_transform(X_df)
        best_params_full = hp_log[model][-1]
        clean = {k: v for k, v in best_params_full.items()
                 if k != "outer_fold"}
        _, pred_full = MODEL_FIT[model](clean, X_full, y_struct, X_full)
        insample_c = harrell(y_struct, pred_full)
    except Exception:
        pass

    if np.isnan(pH) or np.isnan(insample_c):
        abstand = "--"
    else:
        tausendstel = (round(float(f"{insample_c:.3f}") * 1000)
                       - round(float(f"{pH:.3f}") * 1000))
        abstand = f"{tausendstel / 1000:.3f}"

    rows.append({
        "Modell":      model,
        "C-Index":   fmt_ci(pH, lH, hH),
        "Uno-C":       fmt_ci(pU, lU, hU),
        "AUC 12 Mo":   auc_strs[0],
        "AUC 24 Mo":   auc_strs[1],
        "AUC 36 Mo":   auc_strs[2],
        "In-sample C": f"{insample_c:.3f}" if not np.isnan(insample_c) else "--",
        "Abstand":     abstand,
    })


# ── Cache OOF risks + emit LaTeX table + hyperparam JSON ──
np.savez(CACHE_DIR / "stage2_oof_risk.npz",
         pseudonyms=df["Pseudonym"].values,
         time=df["time"].values,
         event=df["event"].values,
         **{k: v for k, v in oof_risk.items()})
print(f"[stage2cv] cached OOF risks → "
      f"{CACHE_DIR/'stage2_oof_risk.npz'}")

with (CACHE_DIR.parent / "stage2_hyperparams_final.json").open(
        "w", encoding="utf-8") as f:
    json.dump(hp_log, f, indent=2)
print(f"[stage2cv] hyperparams → stage2_hyperparams_final.json")


def tex_escape(s):
    s = str(s)
    s = s.replace("\\", r"\textbackslash{}")
    for ch in "&%$#_{}":
        s = s.replace(ch, "\\" + ch)
    return s


tab_path = OUT_DIR / "Tab_3_10_Stage2_ML_Performance_Landscape.tex"
with tab_path.open("w", encoding="utf-8") as f:
    f.write("% Ausgangsmodell: ML auf den Ausgangsmerkmalen. Nested 5x5-CV + Optuna "
            f"({OPTUNA_N_TRIALS} Trials) + Bootstrap (B={BOOTSTRAP_B}). "
            "Werte: Punktschätzung über OOF + 95%-Bootstrap-KI "
            "(C-Index, AUC 24 Monate). "
            "Spalte C-Index Trainingsdaten zeigt Optimismus-Bias, "
            "Abstand aus den gerundeten Werten.\n")
    cols = ["Modell", "C-Index", "AUC 24 Mo", "In-sample C", "Abstand"]
    kopf = ["Modell", "C-Index (95\\% KI)", "AUC 24 Monate (95\\% KI)",
            "C-Index Trainingsdaten", "Abstand"]
    align = "l" + "r" * (len(cols) - 1)
    f.write("\\begin{tabular}{" + align + "}\n\\toprule\n")
    f.write(" & ".join(kopf) + " \\\\\n\\midrule\n")
    for r in rows:
        f.write(" & ".join(tex_escape(r[c]) for c in cols) + " \\\\\n")
    f.write("\\bottomrule\n\\end{tabular}\n")
print(f"[stage2cv] wrote {tab_path.name}")
print("[stage2cv] done.")
