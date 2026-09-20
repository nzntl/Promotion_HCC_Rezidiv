"""Rezidivrisiko in festen Zeitfenstern (Fruehrezidiv): Rezidivrate und Fensterrisiko je Zeitfenster,
Kaplan-Meier-basiert mit Bootstrap-Konfidenzintervallen. Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx,
data_dynamic_surv.xlsx; schreibt Tab_3_19_Fruehrezidiv_Buckets_Deskriptiv_Landscape.tex.
"""

from __future__ import annotations

# ── Smoke-run constants ──
OPTUNA_N_TRIALS = 50
BOOTSTRAP_B     = 5000
OUTER_K         = 5
INNER_K         = 5
SEED            = 42

import os
import re
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
from matplotlib.patches import Patch

from sklearn.compose import ColumnTransformer
from sklearn.exceptions import ConvergenceWarning
from sklearn.impute import SimpleImputer
from sklearn.inspection import PartialDependenceDisplay, permutation_importance
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score, confusion_matrix, f1_score, roc_auc_score, roc_curve,
)
from sklearn.model_selection import StratifiedKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler

import optuna
optuna.logging.set_verbosity(optuna.logging.WARNING)

warnings.filterwarnings("ignore", category=ConvergenceWarning)
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=DeprecationWarning)
warnings.filterwarnings("ignore", category=UserWarning)
np.random.seed(SEED)

ROOT = Path(__file__).resolve().parents[2]
DATA_DIR = ROOT / "01_Daten"
OUT_DIR  = ROOT / "03_Tables_Figures"
OUT_DIR.mkdir(parents=True, exist_ok=True)

print(f"[stage6] SMOKE? OPTUNA_N_TRIALS={OPTUNA_N_TRIALS} "
      f"BOOTSTRAP_B={BOOTSTRAP_B}")


# ── Load ──
static = pd.read_excel(DATA_DIR / "data_static.xlsx")
dynamic = pd.read_excel(DATA_DIR / "data_dynamic.xlsx")
surv = pd.read_excel(DATA_DIR / "data_dynamic_surv.xlsx")

mask = ~(static["Rezidivtumor"].astype(str).str.lower() == "ja")
df = static.loc[mask].merge(surv, on="Pseudonym", how="inner") \
                   .dropna(subset=["time", "event"]).reset_index(drop=True)
for _c in ["Tumordurchmesser", "FLV", "TLV_preop",
           "d_TLV_rel_FLV_base_first_postop", "ChildPugh_Punkte",
           "ND_count", "BMI", "KGkg", "Alter"]:
    if _c in df.columns:
        df[_c] = pd.to_numeric(df[_c], errors="coerce")
print(f"[stage6] cohort n={len(df)} events={int(df['event'].sum())}")


# ── Helpers ──
def tex_escape(s):
    s = str(s)
    s = s.replace("\\", r"\textbackslash{}")
    for ch in "&%$#_{}":
        s = s.replace(ch, "\\" + ch)
    return s


# ── Lesbare deutsche Feature-Labels für Abbildungen (keine Rohnamen) ──
_NUM_LABELS = {
    "Alter": "Alter",
    "BMI": "BMI",
    "KGkg": "Körpergewicht",
    "Tumordurchmesser": "Tumordurchmesser",
    "ChildPugh_Punkte": "Child-Pugh-Punkte",
    "ND_count": "Nebendiagnosen (Anzahl)",
    "FLV": "FLV (präop)",
    "TLV_preop": "TLV (präop)",
    "d_TLV_rel_FLV_base_first_postop": "Δ TLV vs. FLV (1. postop)",
    "TTLVR_preop": "TTLVR (präop)",
    "Bili_preop_val": "Bilirubin (präop)",
    "Albumin_preop": "Albumin (präop)",
    "Quick_preop": "Quick (präop)",
    "deritis_preop": "De-Ritis-Quotient (präop)",
    "astalb_preop": "AST/Albumin-Ratio (präop)",
    "APRI_preop": "APRI (präop)",
    "ALBI_score": "ALBI-Score",
    "TLV_LM": "TLV (Landmark)",
    "dTLV_FLV_LM": "Δ TLV vs. FLV (Landmark)",
}
_BIN_LABELS = {
    "D_OP_seg_1": "Resektion Segment 1",
    "D_OP_seg_4": "Resektion Segment 4",
    "D_OP_seg_2_3": "Resektion Segment 2/3",
    "D_major": "Major-Hepatektomie",
}
_CAT_PREFIX = {
    "lesion_multi_bin": "Tumoranzahl multipel",
    "Leberzirrhose": "Zirrhose",
    "Symptom_cat": "Symptomatik",
    "ALBI_grade": "ALBI-Grad",
    "DM": "Diabetes mell.",
    "ND_Nikotin": "Nikotinkonsum",
    "T": "T-Stadium",
    "N": "N-Stadium",
    "M": "M-Stadium",
}
_ANALYTE = {
    "GOT": "GOT", "GPT": "GPT", "CRP": "CRP", "Albumin": "Albumin",
    "Quick": "Quick", "Bilirubin": "Bilirubin", "Bili": "Bilirubin",
    "INR": "INR", "GGT": "γ-GT", "AP": "AP", "Krea": "Kreatinin",
    "Leuko": "Leukozyten", "Thrombo": "Thrombozyten",
    "Leukos": "Leukozyten", "Thrombos": "Thrombozyten",
}
_ND_CAT_LABELS = {
    "substance": "Substanzkonsum",
}


def pretty_feature(name):
    """Map a transformed feature name (num__/cat__/bin__ prefixed) to a short German display label."""
    n = str(name)
    for pre in ("num__", "cat__", "bin__"):
        if n.startswith(pre):
            n = n[len(pre):]
            break
    if n in _NUM_LABELS:
        return _NUM_LABELS[n]
    if n in _BIN_LABELS:
        return _BIN_LABELS[n]
    if n.startswith("slope_"):
        body = n[len("slope_"):]
        analyte_raw = re.split(r"_?POD", body)[0].strip("_")
        analyte = _ANALYTE.get(analyte_raw, analyte_raw.replace("_", " "))
        m = re.search(r"POD\s*0*(\d+)[_–-]0*(\d+)", body)
        if m:
            return f"{analyte}-Slope POD {m.group(1)}–{m.group(2)}"
        return f"{analyte}-Slope"
    if n.startswith("D_ND_cat_"):
        cat = n[len("D_ND_cat_"):]
        return "ND: " + _ND_CAT_LABELS.get(cat, cat.replace("_", " "))
    for col, lab in _CAT_PREFIX.items():
        if n == col or n.startswith(col + "_"):
            cat = n[len(col):].lstrip("_").replace("_", " ")
            return f"{lab}: {cat}" if cat else lab
    return n.replace("_", " ")


def write_table(rows, path, caption, special_cols=None):
    """special_cols: Spaltennamen, auf die tex_escape nicht angewendet wird."""
    special_cols = set(special_cols or [])
    cols = list(rows[0].keys())
    align = "l" + "r" * (len(cols) - 1)
    with path.open("w", encoding="utf-8") as f:
        f.write("% " + caption + "\n")
        f.write("\\begin{tabular}{" + align + "}\n\\toprule\n")
        f.write(" & ".join(cols) + " \\\\\n\\midrule\n")
        for r in rows:
            cells = []
            for c in cols:
                v = r[c]
                cells.append(str(v) if c in special_cols else tex_escape(v))
            f.write(" & ".join(cells) + " \\\\\n")
        f.write("\\bottomrule\n\\end{tabular}\n")
    print(f"[stage6] wrote {path.name}")


# ── D1: Bucket-deskriptiv 0-3 / 3-6 / 6-12 / 12-24 / >24 Mo ──
buckets = [
    ("0–3",   0,   90,     True),
    ("3–6",   90,  180,    False),
    ("6–12",  180, 365,    False),
    ("12–24", 365, 730,    False),
    ("> 24",  730, 999999, False),
]


def get_first_loc(p):
    rows = dynamic[(dynamic["Pseudonym"] == p)
                   & (dynamic["datasource"] == "rezidiv")]
    if rows.empty:
        return None
    return rows.sort_values("date_obs").iloc[0]["D_rezidiv_cat"]


def km_surv(time, event):
    """Produkt-Limit-Schaetzer als Treppe."""
    t = np.asarray(time, dtype=float)
    e = np.asarray(event).astype(bool)
    ord_ = np.argsort(t, kind="mergesort")
    t, e = t[ord_], e[ord_]
    zeiten, s, ueberlebend, n = [], [], 1.0, len(t)
    i = 0
    while i < n:
        tj = t[i]
        gleich = i
        while gleich < n and t[gleich] == tj:
            gleich += 1
        d = int(e[i:gleich].sum())
        at_risk = n - i
        if d > 0:
            ueberlebend *= (1.0 - d / at_risk)
        zeiten.append(tj)
        s.append(ueberlebend)
        i = gleich
    return np.asarray(zeiten), np.asarray(s)


def km_at(zeiten, s, T):
    """Stufenauswertung: Wert der letzten Stufe bei oder vor T."""
    if len(zeiten) == 0:
        return 1.0
    k = int(np.searchsorted(zeiten, T, side="right")) - 1
    return 1.0 if k < 0 else float(s[k])


def bedingtes_risiko(time, event, lo, hi):
    """P(Ereignis in (lo, hi] | zu lo noch ereignisfrei), Kaplan-Meier."""
    maske = np.asarray(time, dtype=float) > lo
    if maske.sum() < 2:
        return np.nan
    zeiten, s = km_surv(np.asarray(time, dtype=float)[maske],
                        np.asarray(event)[maske])
    s_lo = km_at(zeiten, s, lo)
    if s_lo <= 0:
        return np.nan
    return 1.0 - km_at(zeiten, s, hi) / s_lo


def bedingtes_risiko_ci(time, event, lo, hi, B, seed):
    """Perzentil-Konfidenzintervall ueber Ziehen mit Zuruecklegen auf Personenebene."""
    rng = np.random.default_rng(seed)
    t = np.asarray(time, dtype=float)
    e = np.asarray(event)
    n = len(t)
    werte = []
    for _ in range(B):
        idx = rng.choice(n, size=n, replace=True)
        q = bedingtes_risiko(t[idx], e[idx], lo, hi)
        if np.isfinite(q):
            werte.append(q)
    if len(werte) < 20:
        return (np.nan, np.nan)
    return (float(np.percentile(werte, 2.5)), float(np.percentile(werte, 97.5)))


rows_d1 = []
events_total = int(df["event"].sum())
for label, lo, hi, _residual in buckets:
    at_risk_mask = df["time"] > lo
    n_at_risk = int(at_risk_mask.sum())
    mask_b = (df["event"] == 1) & (df["time"] > lo) & (df["time"] <= hi)
    sub = df[mask_b]
    n_ev = len(sub)
    q = bedingtes_risiko(df["time"].values, df["event"].values, lo, hi)
    ci_lo, ci_hi = bedingtes_risiko_ci(df["time"].values,
                                       df["event"].values, lo, hi,
                                       BOOTSTRAP_B, SEED)
    pt_days = (df.loc[at_risk_mask, "time"].clip(upper=hi) - lo)
    pt_months = float(pt_days.sum()) / 30.0
    rate_100pm = n_ev / pt_months * 100 if pt_months > 0 else np.nan

    intra = 0
    for ps in sub["Pseudonym"]:
        loc = get_first_loc(ps)
        if loc is None:
            continue
        s_loc = str(loc).lower()
        if "extra" in s_loc:
            continue
        if "intra" in s_loc or "leber" in s_loc or "hep" in s_loc:
            intra += 1
    intra_pct = f"{intra/n_ev*100:.1f}" if n_ev else "--"

    t_val = sub["T"].astype(str).str.replace("T", "", regex=False)
    mvi = (t_val.str.contains("3", na=False)
           | t_val.str.contains("4", na=False)).sum()
    med_tumor = df["Tumordurchmesser"].median()
    big_tumor = (sub["Tumordurchmesser"] > med_tumor).sum()
    cirr = (sub["Leberzirrhose"].astype(str).str.lower() == "ja").sum()

    rows_d1.append({
        "\\shortstack[l]{Zeitfenster\\\\ (Monate)}": label,
        "\\shortstack[r]{Rezidive /\\\\ unter Risiko}": f"{n_ev}/{n_at_risk}",
        "\\shortstack[r]{Risiko im\\\\ Fenster (\\%)}":
            f"{q*100:.1f} ({ci_lo*100:.1f}–{ci_hi*100:.1f})",
        "Rate":
            f"{rate_100pm:.2f}" if np.isfinite(rate_100pm) else "--",
        "\\shortstack[r]{Isoliert intra-\\\\hepatisches\\\\ Rezidiv (\\%)}": intra_pct,
        "\\shortstack[r]{T-Stadium\\\\ $\\geq$ 3 (\\%)}":
            f"{mvi/n_ev*100:.1f}" if n_ev else "--",
        "\\shortstack[r]{Tumor >\\\\ Median (\\%)}":
            f"{big_tumor/n_ev*100:.1f}" if n_ev else "--",
        "Zirrhose (\\%)":
            f"{cirr/n_ev*100:.1f}" if n_ev else "--",
    })

write_table(rows_d1,
            OUT_DIR / "Tab_3_19_Fruehrezidiv_Buckets_Deskriptiv_Landscape.tex",
            f"Rezidivrisiko je Zeitfenster. Nenner = Risikoset zu "
            f"Fensterbeginn; Risiko = bedingte Kaplan-Meier-Wahrscheinlichkeit im Fenster mit Bootstrap-KI; Rate = "
            f"Rezidive pro 100 Patient*innen-Monate Personenzeit im Fenster. "
            f"Merkmalsspalten beziehen sich auf die Rezidive im Fenster und sind "
            f"deskriptiv, nicht kausal. Analyse-Kohorte n={len(df)}, "
            f"Events gesamt {events_total}.")

print(f"[stage6_fruerezidiv] Tab_3_19 (Risikoset + Personenzeit) geschrieben.")
print(f"[stage6_fruerezidiv] ML-Teile D2-D6 "
      f"(Tab_3_20*, Tab_3_21*, Abb_3_21*) entfallen — Fruehrezidiv-ML machte "
      f"eine vierte Forschungsfrage auf; F2 ist deskriptiv-zeitlich.")
