"""Landmark-Modelle POD 90 und POD 180: verschachtelte 5x5-Kreuzvalidierung mit Optuna und Bootstrap.

Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx, data_dynamic_surv.xlsx; schreibt
Tab_3_11_*.tex, Tab_3_12_*.tex, _cache/stage3_pod90.npz, _cache/stage3_pod180.npz und die Hyperparameter-JSONs.
"""

from __future__ import annotations

# ── Smoke-run constants ──
OPTUNA_N_TRIALS = 50
BOOTSTRAP_B     = 5000
OUTER_K         = 5
INNER_K         = 5
SEED            = 42
LANDMARK_DAYS   = (90, 180)

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
)
from sksurv.util import Surv

import optuna


def _kat_als_text(X):
    """Kategoriale Spalten einheitlich als Text; echte Fehlwerte bleiben fehlend."""
    return pd.DataFrame(X).apply(
        lambda s: s.map(lambda v: np.nan if pd.isna(v) else str(v)))

optuna.logging.set_verbosity(optuna.logging.WARNING)

warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)
np.random.seed(SEED)

ROOT = Path(__file__).resolve().parents[2]
DATA_DIR  = ROOT / "01_Daten"
OUT_DIR   = ROOT / "03_Tables_Figures"
CACHE_DIR = ROOT / "02_Code" / "py" / "_cache"
OUT_DIR.mkdir(parents=True, exist_ok=True)
CACHE_DIR.mkdir(parents=True, exist_ok=True)

print(f"[stage3cv] SMOKE? OPTUNA_N_TRIALS={OPTUNA_N_TRIALS} "
      f"BOOTSTRAP_B={BOOTSTRAP_B}")


# ── Cohort + landmark feature matrix ──
static = pd.read_excel(DATA_DIR / "data_static.xlsx")
dynamic = pd.read_excel(DATA_DIR / "data_dynamic.xlsx")
surv = pd.read_excel(DATA_DIR / "data_dynamic_surv.xlsx")

mask = ~(static["Rezidivtumor"].astype(str).str.lower() == "ja")
static_an = static.loc[mask].copy()
analysis_pseu = set(static_an["Pseudonym"])
dynamic_an = dynamic[dynamic["Pseudonym"].isin(analysis_pseu)].copy()
surv_an = surv[surv["Pseudonym"].isin(analysis_pseu)].copy()

preop_num = [c for c in ["Alter", "BMI", "Tumordurchmesser",
                          "ChildPugh_Punkte", "ND_count", "n_op_segments",
                          "FLV", "TLV_preop",
                          "TTLVR_preop", "Bili_preop_val", "Albumin_preop",
                          "Quick_preop", "deritis_preop", "astalb_preop", "APRI_preop",
                          "Leukos_preop", "CRP_preop",
                          "Thrombos_preop",
                          "ALBI_score"]
             if c in static_an.columns]
preop_cat = [c for c in ["mwd", "Leberzirrhose", "DM", "ND_Nikotin", "Symptom_cat",
                          "T", "N", "M",
                          "lesion_multi_bin", "ALBI_grade", "aetio_model"]
             if c in static_an.columns]
ND_CAT_FEATS = ["D_ND_cat_cardiovascular", "D_ND_cat_metabolic",
                "D_ND_cat_endokrin", "D_ND_cat_substance"]
preop_bin = [c for c in ["D_major"]
             if c in static_an.columns] + \
             [c for c in ND_CAT_FEATS if c in static_an.columns]
slope_num = [c for c in static_an.columns if c.startswith("slope_")]
print(f"[stage3cv] preop block: num={len(preop_num)} cat={len(preop_cat)} "
      f"bin={len(preop_bin)} slope={len(slope_num)}")


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


try:
    _ohe = OneHotEncoder(handle_unknown="ignore", sparse_output=False)
except TypeError:
    _ohe = OneHotEncoder(handle_unknown="ignore", sparse=False)


# ── Model factories (parameters supplied per outer fold by Optuna) ──
def fit_predict(model, params, Xtr, ytr, Xte):
    if model == "Cox-LASSO":
        m = CoxnetSurvivalAnalysis(
            l1_ratio=params["l1_ratio"],
            alpha_min_ratio=params["alpha_min_ratio"],
            n_alphas=30, fit_baseline_model=True)
    elif model == "RSF":
        m = RandomSurvivalForest(
            n_estimators=int(params["n_estimators"]),
            max_depth=int(params["max_depth"]),
            min_samples_leaf=int(params["min_samples_leaf"]),
            n_jobs=1, random_state=SEED)
    elif model == "GBSA":
        m = GradientBoostingSurvivalAnalysis(
            n_estimators=int(params["n_estimators"]),
            learning_rate=params["learning_rate"],
            max_depth=int(params["max_depth"]),
            random_state=SEED)
    else:
        raise ValueError(model)
    m.fit(Xtr, ytr)
    return m, m.predict(Xte)


def suggest(model, trial):
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


MODELS = ["Cox-LASSO", "RSF", "GBSA"]


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


def fmt_ci(p, lo, hi, ndigits=3):
    if np.isnan(p):
        return "--"
    return f"{p:.{ndigits}f} ({lo:.{ndigits}f}–{hi:.{ndigits}f})"


def bootstrap(metric_fn, n, rng):
    vals = []
    for _ in range(BOOTSTRAP_B):
        idx = rng.randint(0, n, n)
        try:
            v = metric_fn(idx)
            if isinstance(v, list):
                vals.append(v)
            elif not np.isnan(v):
                vals.append(v)
        except Exception:
            continue
    if not vals:
        return None
    return vals


# ── Landmark-level nested CV ──
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


def run_landmark(landmark_days):
    print(f"\n[stage3cv] ═══ Landmark POD {landmark_days} ═══")
    at_risk = surv_an[surv_an["time"] > landmark_days].copy()
    at_risk["res_time"] = at_risk["time"] - landmark_days
    at_risk["res_event"] = at_risk["event"].astype(int)

    tlv_lm = latest_tlv_before(landmark_days)
    feat = static_an.merge(tlv_lm, on="Pseudonym", how="left")
    feat = feat.merge(at_risk[["Pseudonym", "res_time", "res_event"]],
                      on="Pseudonym", how="inner").reset_index(drop=True)

    for c in preop_num + preop_bin + slope_num + ["TLV_LM", "dTLV_FLV_LM"]:
        if c in feat.columns:
            feat[c] = pd.to_numeric(feat[c], errors="coerce")
    lm_num = [c for c in ["TLV_LM", "dTLV_FLV_LM"] if c in feat.columns]

    X_df = feat[preop_num + slope_num + lm_num + preop_cat + preop_bin]
    n = len(feat)
    n_event = int(feat["res_event"].sum())
    print(f"[stage3cv]   at-risk n={n}  events={n_event}")

    def make_pre():
        return ColumnTransformer([
            ("num", Pipeline([("imp", SimpleImputer(strategy="median")),
                              ("sc", StandardScaler())]),
             preop_num + slope_num + lm_num),
            ("cat", Pipeline([("txt", FunctionTransformer(_kat_als_text)),
                              ("imp", SimpleImputer(strategy="constant",
                                                    fill_value="MISSING")),
                              ("oh", _ohe)]), preop_cat),
            ("bin", SimpleImputer(strategy="constant", fill_value=0),
             preop_bin),
        ])

    y_full = Surv.from_arrays(event=feat["res_event"].astype(bool).values,
                              time=feat["res_time"].astype(float).values)

    eval_times_post = np.array([t - landmark_days
                                for t in (365, 730, 1095)
                                if t - landmark_days > 0], dtype=float)

    strata = feat["res_event"].astype(int).values
    outer_cv = StratifiedKFold(n_splits=OUTER_K, shuffle=True,
                               random_state=SEED)

    cache_path = CACHE_DIR / f"stage3_pod{landmark_days}.npz"
    _from_cache = (os.environ.get("STAGE_FROM_CACHE") == "1"
                   and cache_path.exists())
    HP_REPLAY = _lade_hp_replay(f"stage3_pod{landmark_days}_hyperparams.json")
    oof_risk = {m: np.full(n, np.nan) for m in MODELS}
    hp_log = {m: [] for m in MODELS}
    horizon_res = 730.0 - landmark_days
    oof_p24 = {m: np.full(n, np.nan) for m in MODELS}

    for outer_i, (tr_idx, te_idx) in enumerate(outer_cv.split(np.zeros(n),
                                                              strata)):
        if _from_cache:
            continue
        print(f"[stage3cv]   outer {outer_i+1}/{OUTER_K}")
        pre = make_pre()
        X_tr = pre.fit_transform(X_df.iloc[tr_idx])
        X_te = pre.transform(X_df.iloc[te_idx])
        y_tr = y_full[tr_idx]
        y_te = y_full[te_idx]
        inner_strata = strata[tr_idx]

        for model in MODELS:
            def objective(trial, model=model, X_tr=X_tr, y_tr=y_tr,
                          inner_strata=inner_strata):
                params = suggest(model, trial)
                cv = StratifiedKFold(n_splits=INNER_K, shuffle=True,
                                     random_state=SEED + trial.number)
                cs = []
                for itr, ite in cv.split(np.zeros(len(y_tr)), inner_strata):
                    try:
                        _, pred = fit_predict(model, params, X_tr[itr],
                                              y_tr[itr], X_tr[ite])
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
                study.optimize(objective, n_trials=OPTUNA_N_TRIALS,
                               show_progress_bar=False)
                best = study.best_params
            hp_log[model].append({"outer_fold": outer_i, **best})
            try:
                mdl, pred = fit_predict(model, best, X_tr, y_tr, X_te)
                oof_risk[model][te_idx] = pred
                try:
                    sfs = mdl.predict_survival_function(X_te,
                                                        return_array=False)
                    oof_p24[model][te_idx] = [1.0 - sf(horizon_res)
                                              for sf in sfs]
                except Exception as e_p:
                    print(f"[stage3cv]     {model:9s} keine "
                          f"Ueberlebensfunktion: {e_p}")
                print(f"[stage3cv]     {model:9s} test-C="
                      f"{harrell(y_te, pred):.3f}")
            except Exception as e:
                print(f"[stage3cv]     {model:9s} FAILED: {e}")

    valid = {k: v for k, v in oof_risk.items()
             if not np.all(np.isnan(v))}
    if valid:
        Z = np.column_stack([
            (v - np.nanmean(v)) / (np.nanstd(v) if np.nanstd(v) > 0 else 1)
            for v in valid.values()
        ])
        oof_risk["Ensemble"] = np.nanmean(Z, axis=1)

    if _from_cache:
        cz = np.load(cache_path, allow_pickle=True)
        _pos = {str(p): i for i, p in enumerate(cz["pseudonyms"])}
        _order = [_pos[str(p)] for p in feat["Pseudonym"].values]
        _keys = ([m for m in MODELS if m in cz.files]
                 + (["Ensemble"] if "Ensemble" in cz.files else []))
        oof_risk = {m: np.asarray(cz[m], dtype=float)[_order] for m in _keys}
        print(f"[stage3cv]   STAGE_FROM_CACHE=1: loaded OOF risks "
              f"{list(oof_risk)} (nested CV skipped)")

    rng = np.random.RandomState(SEED + landmark_days)

    rows = []
    for model, risk in oof_risk.items():
        if np.all(np.isnan(risk)):
            rows.append({"Modell": model, "C-Index": "--", "Uno-C": "--",
                         "AUC 12 Mo": "--", "AUC 24 Mo": "--",
                         "AUC 36 Mo": "--"})
            continue

        def f_harrell(idx, risk=risk):
            return harrell(y_full[idx], risk[idx])

        def f_uno(idx, risk=risk):
            return uno(y_full[idx], y_full[idx], risk[idx],
                       tau=float(eval_times_post.max()))

        def f_td(idx, risk=risk, t=eval_times_post):
            return td_auc(y_full[idx], y_full[idx], risk[idx], t)

        vs = bootstrap(f_harrell, n, rng) or []
        pH = (harrell(y_full, risk), float(np.percentile(vs, 2.5)),
              float(np.percentile(vs, 97.5))) if vs else (np.nan,)*3
        vs = bootstrap(f_uno, n, rng) or []
        pU = (uno(y_full, y_full, risk, tau=float(eval_times_post.max())),
              float(np.percentile(vs, 2.5)),
              float(np.percentile(vs, 97.5))) if vs else (np.nan,)*3

        td_full = f_td(np.arange(n))
        auc_strs = []
        for kk in range(3):
            def f_kk(idx, kk=kk, risk=risk):
                a = f_td(idx)
                return a[kk] if kk < len(a) else np.nan
            vs = bootstrap(f_kk, n, rng) or []
            if vs:
                pt = td_full[kk] if kk < len(td_full) else np.nan
                auc_strs.append(fmt_ci(pt,
                                       float(np.percentile(vs, 2.5)),
                                       float(np.percentile(vs, 97.5))))
            else:
                auc_strs.append("--")

        rows.append({"Modell": model,
                     "C-Index": fmt_ci(*pH),
                     "Uno-C": fmt_ci(*pU),
                     "AUC 12 Mo": auc_strs[0],
                     "AUC 24 Mo": auc_strs[1] if len(auc_strs) > 1 else "--",
                     "AUC 36 Mo": auc_strs[2] if len(auc_strs) > 2 else "--"})

    if not _from_cache:
        np.savez(CACHE_DIR / f"stage3_pod{landmark_days}.npz",
                 pseudonyms=feat["Pseudonym"].values,
                 res_time=feat["res_time"].values,
                 res_event=feat["res_event"].values,
                 **{k: v for k, v in oof_risk.items()},
                 **{f"p24_{k}": v for k, v in oof_p24.items()})
        print(f"[stage3cv]   cached → stage3_pod{landmark_days}.npz")

        with (CACHE_DIR.parent / f"stage3_pod{landmark_days}_hyperparams.json"
              ).open("w", encoding="utf-8") as f:
            json.dump(hp_log, f, indent=2)

    return rows


def tex_escape(s):
    s = str(s)
    s = s.replace("\\", r"\textbackslash{}")
    for ch in "&%$#_{}":
        s = s.replace(ch, "\\" + ch)
    return s


def write_table(rows, path, caption):
    keep = ["Modell", "C-Index", "AUC 24 Mo"]
    cols = [c for c in keep if c in rows[0]]
    kopf = {"Modell": "Modell", "C-Index": "C-Index (95\\% KI)",
            "AUC 24 Mo": "AUC 24 Monate (95\\% KI)"}
    align = "l" + "r" * (len(cols) - 1)
    with path.open("w", encoding="utf-8") as f:
        f.write("% " + caption + "\n")
        f.write("\\begin{tabular}{" + align + "}\n\\toprule\n")
        f.write(" & ".join(kopf[c] for c in cols) + " \\\\\n\\midrule\n")
        for r in rows:
            f.write(" & ".join(tex_escape(r[c]) for c in cols) + " \\\\\n")
        f.write("\\bottomrule\n\\end{tabular}\n")
    print(f"[stage3cv] wrote {path.name}")


for lm in LANDMARK_DAYS:
    rows = run_landmark(lm)
    write_table(rows,
                OUT_DIR / f"Tab_3_{'11' if lm==90 else '12'}"
                          f"_Stage3_Landmark_POD{lm}_Landscape.tex",
                f"Landmark-ML bei POD {lm}; nested {OUTER_K}x{INNER_K}-CV "
                f"+ Optuna ({OPTUNA_N_TRIALS} Trials/Outer-Fold) + Bootstrap "
                f"(B={BOOTSTRAP_B}). Werte: OOF-Punktschaetzung + 95%-KI.")

print("[stage3cv] done.")
