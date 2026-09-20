"""Paarweise Differenz des C-Index gegenueber Cox A mit gepaartem Bootstrap; ergaenzt Tab_3_15_Consolidated_Comparison.tex.

Liest _cache/stage4_risks.npz und _cache/stage4_comparison_rows.csv.
"""

from __future__ import annotations

OPTUNA_N_TRIALS = 50
BOOTSTRAP_B     = 5000
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

from sksurv.metrics import concordance_index_censored
from sksurv.util import Surv

warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)
np.random.seed(SEED)

ROOT = Path(__file__).resolve().parents[2]
OUT_DIR   = ROOT / "03_Tables_Figures"
CACHE_DIR = ROOT / "02_Code" / "py" / "_cache"
OUT_DIR.mkdir(parents=True, exist_ok=True)

print(f"[stage4dm] BOOTSTRAP_B={BOOTSTRAP_B}")

cache_path = CACHE_DIR / "stage4_risks.npz"
if not cache_path.exists():
    raise SystemExit(f"[stage4dm] missing {cache_path}; run "
                     f"stage4_model_comparison.py first")

cache = np.load(cache_path, allow_pickle=True)
model_names = list(cache["model_names"])
time = cache["time"].astype(float)
event = cache["event"].astype(int)
n_all = len(time)
y = Surv.from_arrays(event=event.astype(bool), time=time)
print(f"[stage4dm] models loaded: {model_names}")

risks = {name: cache[name.replace(" ", "_")] for name in model_names}

ref_key = next((n for n in model_names if "Cox A" in n), model_names[0])
ref_risk = risks[ref_key]
print(f"[stage4dm] reference = {ref_key}")


def harrell(risk, idx=None):
    if idx is None:
        idx = np.arange(n_all)
    keep = idx[~np.isnan(risk[idx])]
    if len(keep) < 5:
        return np.nan
    return concordance_index_censored(event[keep].astype(bool),
                                      time[keep], risk[keep])[0]


n_dropped = 0


def boot_diff(metric_fn, point, n, rng):
    """Percentile-bootstrap CI around the deterministic full-sample point estimate; invalid replicates (NaN/error) are counted…"""
    global n_dropped
    vs = []
    for _ in range(BOOTSTRAP_B):
        idx = rng.randint(0, n, n)
        try:
            v = metric_fn(idx)
        except Exception:
            n_dropped += 1
            continue
        if v is None or (isinstance(v, float) and np.isnan(v)):
            n_dropped += 1
            continue
        vs.append(v)
    if not vs:
        return (float(point), np.nan, np.nan)
    return (float(point), float(np.percentile(vs, 2.5)),
            float(np.percentile(vs, 97.5)))


def fmt(p, lo, hi, ndigits=3):
    if np.isnan(p):
        return "--"
    return f"{p:+.{ndigits}f} ({lo:+.{ndigits}f}; {hi:+.{ndigits}f})"


rng = np.random.RandomState(SEED)
n = n_all

delta_by_model = {}
eigen_by_model = {}
for name in model_names:
    _eigen = ~np.isnan(risks[name])
    eigen_by_model[name] = (int(_eigen.sum()), int(event[_eigen].sum()))
gem_by_model = {}
for name in model_names:
    if name == ref_key:
        continue
    alt = risks[name]

    def f_dh(idx, alt=alt):
        beide = idx[~np.isnan(alt[idx]) & ~np.isnan(ref_risk[idx])]
        if len(beide) < 5:
            return np.nan
        ev = event[beide].astype(bool)
        return (concordance_index_censored(ev, time[beide], alt[beide])[0]
                - concordance_index_censored(ev, time[beide], ref_risk[beide])[0])

    _alle = np.arange(n_all)
    _gem = _alle[~np.isnan(alt[_alle]) & ~np.isnan(ref_risk[_alle])]
    dh = boot_diff(f_dh, f_dh(_alle), n, rng)
    delta_by_model[name] = fmt(*dh)
    gem_by_model[name] = (len(_gem), int(event[_gem].sum()))
    print(f"[stage4dm]   {name:35s} ΔC-Index={dh[0]:+.3f}  "
          f"(gemeinsame Menge n={len(_gem)})")


# ── Tab 3.15 neu schreiben: Basisspalten + ΔC-Index ──
rows_csv = CACHE_DIR / "stage4_comparison_rows.csv"
if not rows_csv.exists():
    raise SystemExit(f"[stage4dm] missing {rows_csv}; run "
                     f"stage4_model_comparison.py first")
base = pd.read_csv(rows_csv, encoding="utf-8")

tab_path = OUT_DIR / "Tab_3_15_Consolidated_Comparison.tex"
with tab_path.open("w", encoding="utf-8") as f:
    f.write("% Konsolidierter Modellvergleich. N/Rezidive und die beiden "
            "Guetemasse je Modell auf der Patientenmenge, fuer die es "
            "Vorhersagen liefert. Die Differenz in der letzten Spalte wird auf "
            f"der Schnittmenge mit {ref_key} berechnet, weil nur dort dieselben "
            "Personen verglichen werden; sie entspricht deshalb nicht der "
            "Differenz der Spaltenwerte. Gemeinsame Mengen in der Legende. "
            f"Gepaarter Bootstrap, B={BOOTSTRAP_B}. Basiszeilen aus "
            "stage4_model_comparison.py, Differenzen hier ergaenzt.\n")
    f.write("\\begin{tabular}{lrrrr}\n\\toprule\n")
    f.write("Modell & N/Rezidive & \\makecell[r]{C-Index\\\\(95\\% KI)} & "
            "\\makecell[r]{AUC 24 Monate\\\\(95\\% KI)} & "
            "\\makecell[r]{$\\Delta$C-Index zu Cox A\\\\(95\\% KI)} "
            "\\\\\n\\midrule\n")
    for _, r in base.iterrows():
        m = r["Modell"]
        n_e, ev_e = eigen_by_model[m]
        delta = "" if m == ref_key else delta_by_model.get(m, "--")
        f.write(f"{m.split(' (')[0]} & {n_e}/{ev_e} & {r['C-Index']} & "
                f"{r['AUC 24 Mo']} & {delta} \\\\\n")
    f.write("\\bottomrule\n\\end{tabular}\n")
print(f"[stage4dm] wrote {tab_path.name}  (einteilig, 5 Spalten)")
for _m in model_names:
    _g = gem_by_model.get(_m)
    print(f"[stage4dm]   {_m:35s} eigen N={eigen_by_model[_m][0]} "
          f"Rezidive={eigen_by_model[_m][1]}"
          + (f"  gemeinsam N={_g[0]} Rezidive={_g[1]}" if _g else ""))
print(f"[stage4dm] dropped bootstrap replicates (NaN/error): {n_dropped}")
print("[stage4dm] done.")
