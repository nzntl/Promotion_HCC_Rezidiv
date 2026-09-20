"""Kaplan-Meier-Kurven nach Risikodritteln des Landmark-Modells POD 180.

Liest _cache/stage3_pod180.npz; schreibt Abb_KM_RiskScore_Tertiles.png.
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
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

from lifelines import KaplanMeierFitter
from lifelines.statistics import multivariate_logrank_test

warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=PendingDeprecationWarning)
warnings.filterwarnings("ignore", category=UserWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)
np.random.seed(SEED)

ROOT = Path(__file__).resolve().parents[2]
OUT_DIR   = ROOT / "03_Tables_Figures"
CACHE_DIR = ROOT / "02_Code" / "py" / "_cache"

print(f"[stage4km] start")

cache_path = CACHE_DIR / "stage3_pod180.npz"
if not cache_path.exists():
    raise SystemExit(f"[stage4km] missing {cache_path}")

cache = np.load(cache_path, allow_pickle=True)
if "GBSA" not in cache.files:
    raise SystemExit(f"[stage4km] no GBSA risk in cache")

risk = cache["GBSA"]
res_time = cache["res_time"].astype(float) / 365.25
res_event = cache["res_event"].astype(int)

keep = ~np.isnan(risk)
risk = risk[keep]
res_time = res_time[keep]
res_event = res_event[keep]
print(f"[stage4km] cohort at-risk bei POD180: n={len(risk)} "
      f"events={int(res_event.sum())}")

q33 = np.quantile(risk, 1/3)
q67 = np.quantile(risk, 2/3)
grp = np.where(risk <= q33, "low",
        np.where(risk <= q67, "mid", "high"))

lr = multivariate_logrank_test(res_time, grp, res_event)
print(f"[stage4km] log-rank p = {lr.p_value:.4g}")

colors = {"low": "#00441B", "mid": "#777777", "high": "#D95F02"}
labels_de = {"low": "Niedrig: bis 33. Perzentil",
             "mid": "Mittel: 33. bis 67. Perzentil",
             "high": "Hoch: über 67. Perzentil"}
labels_kurz = {"low": "Niedrig", "mid": "Mittel", "high": "Hoch"}
X_MAX = 8

fig, (ax, ax_rt) = plt.subplots(2, 1, figsize=(8.0, 6.5), sharex=True,
                                gridspec_kw={"height_ratios": [3, 1]})

kmfs = {}
medians = {}
for g in ["low", "mid", "high"]:
    m = grp == g
    kmf = KaplanMeierFitter()
    kmf.fit(res_time[m], event_observed=res_event[m],
            label=f"{labels_de[g]} (n = {int(m.sum())})")
    kmf.plot_survival_function(ax=ax, ci_show=True, color=colors[g],
                                linewidth=1.8)
    medians[g] = kmf.median_survival_time_
    kmfs[g] = kmf

for g in ["low", "mid", "high"]:
    _m = medians[g]
    _txt = ("nicht erreicht" if _m is None or np.isnan(_m)
            else f"{_m:.1f} Jahre ({_m * 12:.1f} Mo)")
    print(f"[stage4km] Median Residualzeit {labels_kurz[g]:<8s} = {_txt} "
          f"(n={int((grp == g).sum())})")

ax.set_xlim(0, X_MAX)
ax.set_xticks(range(0, X_MAX + 1))
ax.tick_params(labelbottom=True)
ax.set_xlabel("Jahre ab Landmark POD 180")
ax.xaxis.label.set_visible(True)
ax.set_ylabel("Patient*innen ohne Rezidiv")
ax.set_ylim(0, 1.0)
ax.grid(alpha=0.25)
ax.legend(loc="upper right", frameon=True)
median_txt = ", ".join(
    f"{labels_kurz[g].lower()} "
    + ("nicht erreicht" if medians[g] is None or np.isnan(medians[g])
       else f"{medians[g]:.1f}")
    for g in ["low", "mid", "high"])
ax.text(0.02, 0.05,
        f"Log-Rank-Test: p = {lr.p_value:.3f}\nMediane: {median_txt} Jahre",
        transform=ax.transAxes, fontsize=9,
        bbox=dict(boxstyle="round", fc="white", alpha=0.85, edgecolor="none"))

times = np.arange(0, X_MAX + 1)
for _s in ax_rt.spines.values():
    _s.set_visible(False)
ax_rt.set_yticks([])
ax_rt.set_ylim(-0.5, 3.2)
ax_rt.tick_params(axis="x", length=0, labelbottom=False)
for k, g in enumerate(["low", "mid", "high"]):
    y = 2 - k
    at_risk_vals = [int(((grp == g) & (res_time >= t)).sum()) for t in times]
    ax_rt.text(-0.02, y, labels_kurz[g], transform=ax_rt.get_yaxis_transform(),
               fontsize=9, color=colors[g], fontweight="bold", ha="right",
               va="center")
    for t, v in zip(times, at_risk_vals):
        ax_rt.text(t, y, str(v), fontsize=8.5, ha="center", va="center",
                   color=colors[g])
ax_rt.text(0.5, 3.0, "Patient*innen unter Risiko", fontsize=9, ha="left",
           va="center")

plt.tight_layout()
fig_path = OUT_DIR / "Abb_KM_RiskScore_Tertiles.png"
plt.savefig(fig_path, dpi=200, facecolor="white", bbox_inches="tight")
plt.close()
print(f"[stage4km] wrote {fig_path.name}")
print("[stage4km] done.")
