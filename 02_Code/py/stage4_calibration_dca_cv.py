"""Kalibrierung und Entscheidungskurvenanalyse auf Out-of-Fold-Vorhersagen.

Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx, data_dynamic_surv.xlsx; schreibt
Abb_3_13_Calibration_24mo.png und Abb_3_14_DCA.png.
"""

from __future__ import annotations

import os
import shutil
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
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.model_selection import StratifiedKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler, FunctionTransformer

from sksurv.ensemble import GradientBoostingSurvivalAnalysis
from sksurv.nonparametric import kaplan_meier_estimator
from sksurv.util import Surv

warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)

SEED = 42
LANDMARK = 180
N_SPLITS = 5
np.random.seed(SEED)

COL_BLUE = "#3B6FAB"
COL_RED = "#C7261B"
COL_YEL = "#E1A11A"

ROOT = Path(__file__).resolve().parents[2]
DATA_DIR = ROOT / "01_Daten"
OUT_DIR = ROOT / "03_Tables_Figures"
CACHE_DIR = ROOT / "02_Code" / "py" / "_cache"
LIVE_DIR = ROOT / "05_Target_Structure" / "01_figs_tabs"
OUT_DIR.mkdir(parents=True, exist_ok=True)

print("[stage4cv] start")

# ── Load + analysis cohort (identical to stage4_interpretability.py) ──
static = pd.read_excel(DATA_DIR / "data_static.xlsx")
dynamic = pd.read_excel(DATA_DIR / "data_dynamic.xlsx")
surv = pd.read_excel(DATA_DIR / "data_dynamic_surv.xlsx")

mask = ~(static["Rezidivtumor"].astype(str).str.lower() == "ja")
static_an = static.loc[mask].copy()
analysis_pseu = set(static_an["Pseudonym"])
dynamic_an = dynamic[dynamic["Pseudonym"].isin(analysis_pseu)].copy()
surv_an = surv[surv["Pseudonym"].isin(analysis_pseu)].copy()

# ── Merkmalsbasis — WORTGLEICH aus stage3_nested_cv.py uebernommen ──
preop_num = [c for c in ["Alter", "BMI", "Tumordurchmesser",
                         "ChildPugh_Punkte", "ND_count", "n_op_segments",
                         "FLV", "TLV_preop",
                         "TTLVR_preop", "Bili_preop_val", "Albumin_preop",
                         "Quick_preop", "deritis_preop", "astalb_preop", "APRI_preop",
                         "Leukos_preop", "CRP_preop", "Thrombos_preop",
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
print(f"[stage4cv] Merkmalsbasis wie stage3: num={len(preop_num)} "
      f"cat={len(preop_cat)} bin={len(preop_bin)} slope={len(slope_num)}")

try:
    _ohe = OneHotEncoder(handle_unknown="ignore", sparse_output=False)
except TypeError:
    _ohe = OneHotEncoder(handle_unknown="ignore", sparse=False)


def landmark_cohort(landmark_days):
    at_risk = surv_an[surv_an["time"] > landmark_days].copy()
    at_risk["res_time"] = at_risk["time"] - landmark_days
    at_risk["res_event"] = at_risk["event"].astype(int)

    d = dynamic_an[(dynamic_an["datasource"] == "volumetry")
                   & dynamic_an["lab_date_POD_imp"].notna()
                   & (dynamic_an["lab_date_POD_imp"] > 0)
                   & (dynamic_an["lab_date_POD_imp"] <= landmark_days)]
    if not d.empty:
        d = d.sort_values(["Pseudonym", "lab_date_POD_imp"]).groupby(
            "Pseudonym").tail(1)
        tlv_lm = d[["Pseudonym", "TLV", "d_TLV_rel_FLV_base"]].rename(
            columns={"TLV": "TLV_LM", "d_TLV_rel_FLV_base": "dTLV_FLV_LM"})
    else:
        tlv_lm = pd.DataFrame(columns=["Pseudonym", "TLV_LM", "dTLV_FLV_LM"])

    feat = static_an.merge(tlv_lm, on="Pseudonym", how="left")
    feat = feat.merge(at_risk[["Pseudonym", "res_time", "res_event"]],
                      on="Pseudonym", how="inner")
    return feat


feat = landmark_cohort(LANDMARK).reset_index(drop=True)
for c in preop_num + preop_bin + slope_num + ["TLV_LM", "dTLV_FLV_LM"]:
    if c in feat.columns:
        feat[c] = pd.to_numeric(feat[c], errors="coerce")
lm_num = [c for c in ["TLV_LM", "dTLV_FLV_LM"] if c in feat.columns]

X_df = feat[preop_num + slope_num + lm_num + preop_cat + preop_bin]


def _kat_als_text(X):
    """Kategoriale Spalten einheitlich als Text; echte Fehlwerte bleiben fehlend."""
    return pd.DataFrame(X).apply(
        lambda s: s.map(lambda v: np.nan if pd.isna(v) else str(v)))


def build_pre():
    """Vorverarbeitung wortgleich aus stage3_nested_cv.py."""
    return ColumnTransformer([
        ("num", Pipeline([("imp", SimpleImputer(strategy="median")),
                          ("sc", StandardScaler())]),
         preop_num + slope_num + lm_num),
        ("cat", Pipeline([("txt", FunctionTransformer(_kat_als_text)),
                          ("imp", SimpleImputer(strategy="constant",
                                                fill_value="MISSING")),
                          ("oh", _ohe)]), preop_cat),
        ("bin", SimpleImputer(strategy="constant", fill_value=0), preop_bin),
    ])


res_time = feat["res_time"].astype(float).values
res_event = feat["res_event"].astype(int).values
n = len(feat)
HORIZON = 730.0 - LANDMARK
print(f"[stage4cv] POD {LANDMARK} cohort: n={n}  events={int(res_event.sum())}"
      f"  horizon(res)={HORIZON:.0f} d")


# ── Out-of-fold GBSA 24-month event probabilities (5-fold stratified CV) ──
def _gbsa_params_pod180():
    import json, statistics
    pfad = Path(__file__).resolve().parent / "stage3_pod180_hyperparams.json"
    rueckfall = dict(n_estimators=100, learning_rate=0.073, max_depth=2)
    if not pfad.exists():
        print(f"[stage4cv] WARN: {pfad.name} fehlt, nutze {rueckfall}")
        return rueckfall
    with pfad.open(encoding="utf-8") as f:
        folds = json.load(f).get("GBSA", [])
    if not folds:
        print(f"[stage4cv] WARN: kein GBSA-Eintrag, nutze {rueckfall}")
        return rueckfall
    med = {}
    for k in ("n_estimators", "learning_rate", "max_depth"):
        werte = [f[k] for f in folds if k in f]
        if not werte:
            return rueckfall
        m = statistics.median(werte)
        med[k] = int(round(m)) if k != "learning_rate" else float(m)
    print(f"[stage4cv] GBSA-Hyperparameter aus {len(folds)} POD-180-Falten: {med}")
    return med


GBSA_PARAMS = _gbsa_params_pod180()


def rsf_new():
    return GradientBoostingSurvivalAnalysis(random_state=SEED, **GBSA_PARAMS)


_cache_npz = CACHE_DIR / f"stage3_pod{LANDMARK}.npz"
oof_p = np.full(n, np.nan)
_geladen = False
if _cache_npz.exists():
    _c = np.load(_cache_npz, allow_pickle=True)
    if "p24_GBSA" in _c.files:
        _pos = {p: i for i, p in enumerate(_c["pseudonyms"])}
        _fehlt = 0
        for i, p in enumerate(feat["Pseudonym"].values):
            j = _pos.get(p)
            if j is None:
                _fehlt += 1
            else:
                oof_p[i] = _c["p24_GBSA"][j]
        _geladen = np.isfinite(oof_p).sum() > 0
        print(f"[stage4cv] Wahrscheinlichkeiten aus {_cache_npz.name} geladen "
              f"({int(np.isfinite(oof_p).sum())} von {n}, {_fehlt} ohne "
              f"Entsprechung)")
    else:
        print(f"[stage4cv] WARN: {_cache_npz.name} enthaelt kein p24_GBSA")
else:
    print(f"[stage4cv] WARN: {_cache_npz.name} fehlt")

if not _geladen:
    print("[stage4cv] RUECKFALL: eigener Fit, nicht deckungsgleich mit den "
          "uebrigen Auswertungen")
    skf = StratifiedKFold(n_splits=N_SPLITS, shuffle=True, random_state=SEED)
    for fold, (tr, te) in enumerate(skf.split(np.zeros(n), res_event)):
        pre = build_pre()
        X_tr = pre.fit_transform(X_df.iloc[tr])
        X_te = pre.transform(X_df.iloc[te])
        y_tr = Surv.from_arrays(event=res_event[tr].astype(bool),
                                time=res_time[tr])
        rsf = rsf_new()
        rsf.fit(X_tr, y_tr)
        sfs = rsf.predict_survival_function(X_te, return_array=False)
        oof_p[te] = np.array([1.0 - sf(HORIZON) for sf in sfs])
        print(f"[stage4cv]   fold {fold + 1}/{N_SPLITS}: "
              f"test n={len(te)}  pred range "
              f"[{oof_p[te].min():.3f}, {oof_p[te].max():.3f}]")


from sksurv.metrics import concordance_index_censored as _cic
_ok = ~np.isnan(oof_p)
_c_here = _cic(res_event[_ok].astype(bool), res_time[_ok], oof_p[_ok])[0]
print(f"[stage4cv] C-Index der hier erzeugten OOF-Vorhersagen: {_c_here:.3f}")


# ── Kaplan-Meier helper: observed event probability at the horizon ──
def km_event_prob(time, event, horizon):
    """1 - S(horizon) via Kaplan-Meier; censoring-aware."""
    if len(time) == 0:
        return np.nan
    t, s = kaplan_meier_estimator(event.astype(bool), time)
    idx = np.searchsorted(t, horizon, side="right") - 1
    surv = s[idx] if idx >= 0 else 1.0
    return 1.0 - surv


# ── Abb 3.13 — Calibration (OOF predicted vs KM-observed, 8 quantile bins) ──
def calibration_bins(pred, time, event, horizon, n_bins=8, min_n=8):
    keep = ~np.isnan(pred)
    p = pred[keep]
    tt = time[keep]
    ee = event[keep]
    qs = np.quantile(p, np.linspace(0, 1, n_bins + 1))
    qs[0] = -np.inf
    qs[-1] = np.inf
    rows = []
    for k in range(n_bins):
        m = (p >= qs[k]) & (p < qs[k + 1])
        if m.sum() < min_n:
            continue
        rows.append((p[m].mean(),
                     km_event_prob(tt[m], ee[m], horizon),
                     int(m.sum())))
    return pd.DataFrame(rows, columns=["pred", "obs", "n"])


cb = calibration_bins(oof_p, res_time, res_event, HORIZON, n_bins=8)
print(f"[stage4cv] calibration bins: {len(cb)}")
for _i, _r in cb.iterrows():
    print(f"[stage4cv]   bin {_i + 1}: pred={_r.pred:.3f}  obs={_r.obs:.3f}  "
          f"n={int(_r.n)}")
print(f"[stage4cv] Spannweite vorhergesagt {cb.pred.min():.3f}-{cb.pred.max():.3f}"
      f"  beobachtet {cb.obs.min():.3f}-{cb.obs.max():.3f}")

COL_GRUEN = "#00441B"
COL_GRAU = "#555555"
fig, ax = plt.subplots(figsize=(4.6, 4.3))
ax.plot([0, 1], [0, 1], linestyle="--", color="grey", linewidth=0.8,
        label="Perfekte Kalibrierung", zorder=1)
ax.scatter(cb["pred"], cb["obs"], s=cb["n"] * 8, color="grey", alpha=0.35,
           linewidths=0, zorder=2)
ax.plot(cb["pred"], cb["obs"], marker="o", markersize=4, color="black",
        linewidth=1.4, label="GBSA (POD 180, kreuzvalidiert)", zorder=3)
if len(cb) == 8:
    _lage = ["oben", "unten", "oben", "untenlinks", "rechts", "oben", "unten",
             "oben"]
else:
    _lage = ["oben"] * len(cb)
    print(f"[stage4cv] WARN: {len(cb)} statt 8 Gruppen, Zahlen alle ueber "
          f"dem Punkt")
_box = dict(boxstyle="square,pad=0.08", facecolor="white", edgecolor="none")
for _x, _y, _wo in zip(cb["pred"], cb["obs"], _lage):
    if _wo == "oben":
        _pos, _ha = [(0, 19), (0, 10)], "center"
    elif _wo == "unten":
        _pos, _ha = [(0, -10), (0, -19)], "center"
    elif _wo == "untenlinks":
        _pos, _ha = [(-5, -10), (-5, -19)], "right"
    else:
        _pos, _ha = [(9, 5), (9, -4)], "left"
    for (_dx, _dy), _farbe, _txt in zip(_pos, (COL_GRUEN, COL_GRAU),
                                         (f"{_x:.3f}", f"{_y:.3f}")):
        ax.annotate(_txt, (_x, _y), xytext=(_dx, _dy),
                    textcoords="offset points", color=_farbe, fontsize=7.5,
                    ha=_ha, va="center", bbox=_box, zorder=4)
lim = max(0.6, float(cb["pred"].max()) * 1.1, float(cb["obs"].max()) * 1.1)
ax.set_xlim(0, lim)
ax.set_ylim(0, lim)
ax.set_xlabel("Vorhergesagte Rezidivwahrscheinlichkeit\nbis 24 Monate nach OP",
              color=COL_GRUEN, fontsize=9)
ax.set_ylabel("Beobachtete Rezidivrate bis 24 Monate\nnach OP (Kaplan-Meier)",
              color=COL_GRAU, fontsize=9)
ax.tick_params(labelsize=8)
ax.legend(loc="upper left", fontsize=8, frameon=False)
ax.grid(alpha=0.2)
plt.tight_layout()
cal_path = OUT_DIR / "Abb_3_13_Calibration_24mo.png"
plt.savefig(cal_path, dpi=200, facecolor="white")
plt.close()
print(f"[stage4cv]   wrote {cal_path.name}")


# ── Abb 3.14 — Decision Curve Analysis (survival net benefit, Vickers 2008) ──
thresholds = np.linspace(0.02, 0.50, 49)


def km_net_benefit(pred, time, event, horizon, thresholds, min_pos=8):
    keep = ~np.isnan(pred)
    p = pred[keep]
    tt = time[keep]
    ee = event[keep]
    N = len(p)
    nb = []
    for t in thresholds:
        pos = p >= t
        if pos.sum() < min_pos:
            nb.append(np.nan)
            continue
        p_pos = pos.sum() / N
        risk_pos = km_event_prob(tt[pos], ee[pos], horizon)
        tp = risk_pos * p_pos
        fp = (1.0 - risk_pos) * p_pos
        nb.append(tp - fp * (t / (1.0 - t)))
    return np.array(nb)


nb_rsf = km_net_benefit(oof_p, res_time, res_event, HORIZON, thresholds)
prev = km_event_prob(res_time, res_event, HORIZON)
nb_all = prev - (1.0 - prev) * thresholds / (1.0 - thresholds)
nb_none = np.zeros_like(thresholds)
print(f"[stage4cv] overall 24-Mo KM recurrence (Treat-all anchor): {prev:.3f}")

above = (nb_rsf > nb_all) & np.isfinite(nb_rsf)
cross = float(thresholds[np.argmax(above)]) if above.any() else np.nan
dauerhaft = np.nan
for _k in range(len(thresholds)):
    if above[_k:].all():
        dauerhaft = float(thresholds[_k])
        break
print(f"[stage4cv] Modell erstmals ueber Alles-Strategie bei t = {cross:.3f}")
print(f"[stage4cv] Modell DURCHGEHEND ueber Alles-Strategie ab t = {dauerhaft:.3f}")
_maxgap = float(np.nanmax(nb_rsf - nb_all))
_at = float(thresholds[int(np.nanargmax(nb_rsf - nb_all))])
print(f"[stage4cv] groesster Abstand {_maxgap:+.4f} bei t = {_at:.3f}")
_all_neg = thresholds[nb_all < 0]
_mod_pos = thresholds[np.isfinite(nb_rsf) & (nb_rsf > 0)]
print(f"[stage4cv] Alles-Strategie faellt unter null ab t = "
      f"{_all_neg.min():.3f}" if len(_all_neg) else "[stage4cv] Alles-Strategie bleibt positiv")
print(f"[stage4cv] Modell bleibt positiv bis t = "
      f"{_mod_pos.max():.3f}" if len(_mod_pos) else "[stage4cv] Modell nirgends positiv")

fig, ax = plt.subplots(figsize=(4.8, 3.6))
ax.plot(thresholds, nb_none, linestyle="--", color="black", linewidth=0.8,
        label="Keine intensivierte Nachsorge")
ax.plot(thresholds, nb_all, color="black", linewidth=1.4,
        label="Alle intensiviert nachsorgen")
ax.plot(thresholds, nb_rsf, color=COL_GRUEN, linewidth=1.4,
        label="GBSA (POD 180, kreuzvalidiert)")
ax.set_xlabel("Schwellenwahrscheinlichkeit", fontsize=9)
ax.set_ylabel("Netto-Nutzen", fontsize=9)
ax.tick_params(labelsize=8)
ax.set_ylim(-0.05, max(0.3, float(np.nanmax(nb_all)) + 0.05))
ax.legend(loc="upper right", fontsize=8, frameon=False)
ax.grid(alpha=0.2)
plt.tight_layout()
dca_path = OUT_DIR / "Abb_3_14_DCA.png"
plt.savefig(dca_path, dpi=200, facecolor="white")
plt.close()
print(f"[stage4cv]   wrote {dca_path.name}")

if LIVE_DIR.exists():
    for f in (cal_path, dca_path):
        shutil.copyfile(f, LIVE_DIR / f.name)
    print(f"[stage4cv]   copied live -> {LIVE_DIR}")
print("[stage4cv] done.")
