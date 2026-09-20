"""Risikoadaptierte Nachsorgeintervalle: Hazard je Aetiologie, empfohlene Intervalle je Phase, Vergleich von
Nachsorgeschemata. Liest 01_Daten/data_static.xlsx, data_dynamic_surv.xlsx und _cache/stage2_oof_risk.npz;
schreibt Tab_3_26_*.tex bis Tab_3_28_*.tex und Abb_3_26_Tradeoff_Curve.png.
"""
from __future__ import annotations
import os, sys, warnings
from pathlib import Path
os.environ.setdefault("PYTHONIOENCODING", "utf-8")
try: sys.stdout.reconfigure(encoding="utf-8")
except Exception: pass

import numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg"); import matplotlib.pyplot as plt
from sksurv.nonparametric import kaplan_meier_estimator
from sksurv.metrics import concordance_index_censored

warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)
SEED = 42; np.random.seed(SEED)

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "01_Daten"; OUT = ROOT / "03_Tables_Figures"
print(f"[stage6] root={ROOT}")

static = pd.read_excel(DATA / "data_static.xlsx")
surv   = pd.read_excel(DATA / "data_dynamic_surv.xlsx")

mask = ~(static["Rezidivtumor"].astype(str).str.lower() == "ja")
s = static.loc[mask].copy()
ana_pseu = set(s["Pseudonym"])
sv = surv[surv["Pseudonym"].isin(ana_pseu)].copy()
joined = s.merge(sv[["Pseudonym","time","event"]], on="Pseudonym", how="inner")
print(f"[stage6] analysis cohort with survival: {len(joined)}")
print(f"[stage6] ätiologie distribution:")
print(joined["äthiologie_cat"].value_counts(dropna=False).to_string())

# ── OOF risk scores (Stufe 2) for the risk-adapted scheme ──
CACHE = ROOT / "02_Code" / "py" / "_cache" / "stage2_oof_risk.npz"
if not CACHE.exists():
    sys.exit(f"[stage6] FEHLER: OOF-Risiko-Cache fehlt: {CACHE}\n"
             "         Zuerst stage2_nested_cv.py ausführen. "
             "Kein Proxy-Fallback vorgesehen.")
cache2 = np.load(CACHE, allow_pickle=True)
risk_keys = [k for k in cache2.files
             if k not in ("pseudonyms", "time", "event")]
c_time = cache2["time"].astype(float)
c_event = cache2["event"].astype(bool)
RISK_MODEL, best_c = None, -np.inf
for k in risk_keys:
    if k == "Ensemble":
        continue
    v = cache2[k].astype(float)
    ok = ~np.isnan(v)
    if ok.sum() < 10:
        continue
    c = concordance_index_censored(c_event[ok], c_time[ok], v[ok])[0]
    print(f"[stage6]   Ausgangs-OOF {k:10s} C={c:.3f}")
    if c > best_c:
        best_c, RISK_MODEL = c, k
if RISK_MODEL is None:
    sys.exit("[stage6] FEHLER: Kein verwertbares Risikomodell im OOF-Cache.")
risk_map = dict(zip(cache2["pseudonyms"], cache2[RISK_MODEL].astype(float)))
joined["oof_risk"] = joined["Pseudonym"].map(risk_map)
n_scored = int(joined["oof_risk"].notna().sum())
print(f"[stage6] OOF risk model: {RISK_MODEL}, scored {n_scored}/{len(joined)}")
if n_scored < 10:
    sys.exit("[stage6] FEHLER: Zu wenige Patient*innen mit OOF-Risikoscore.")

# ── Parameters ──
THRESHOLD_PRIMARY = 0.05
THRESHOLD_SENS    = (0.03, 0.10)
INTERVALS_WEEKS   = [4, 6, 8, 12, 16, 24]
INTERVALS_DAYS    = [w * 7 for w in INTERVALS_WEEKS]
PHASES = [("Phase 1: 0-12 Mo",  0,    365),
          ("Phase 2: 12-24 Mo", 365,  730),
          ("Phase 3: 24-60 Mo", 730, 1825)]
AETIO_CLASSES = ["HBV","HCV","Ethyltox","MASLD","Andere","Unbekannt"]
ANZEIGE_AE = {"Ethyltox": "Ethyltoxisch"}


# ── Tab_3_26: Rezidive je Zeitfenster und Ätiologie (Zählungen, keine Raten) ──
BUCKETS = [("0–3",   0,   90),
           ("3–6",   90,  180),
           ("6–12",  180, 365),
           ("12–24", 365, 730),
           ("> 24",  730, 99999)]

def n_events_at(df, t_lo, t_hi):
    n_at_risk = ((df["time"] > t_lo)).sum()
    n_events  = ((df["event"] == 1) & (df["time"] > t_lo) &
                 (df["time"] <= t_hi)).sum()
    return int(n_events), int(n_at_risk)

rows326 = []
for bname, lo, hi in BUCKETS:
    row = {"Zeitfenster": bname}
    n_total = 0; n_event_total = 0
    for ae in AETIO_CLASSES + ["Gesamt"]:
        if ae == "Gesamt":
            sub = joined
        else:
            sub = joined[joined["äthiologie_cat"] == ae]
        n_ev, n_ar = n_events_at(sub, lo, hi)
        row[ae] = f"{n_ev}/{n_ar}" if n_ar else "—"
        if ae != "Gesamt":
            n_total += n_ar; n_event_total += n_ev
    rows326.append(row)

def esc(s):
    s = str(s)
    s = s.replace("\\", r"\textbackslash{}")
    for ch in "&%$#_{}": s = s.replace(ch,"\\"+ch)
    return s

with (OUT / "Tab_3_26_Hazard_by_Aetio_Landscape.tex").open("w", encoding="utf-8") as f:
    f.write("% Rezidive je Zeitfenster und Ätiologie. Zelle: Rezidive im "
            "Fenster / zu Fensterbeginn rezidivfrei unter Beobachtung.\n")
    cols = ["Zeitfenster"] + AETIO_CLASSES + ["Gesamt"]
    kopf = {"Zeitfenster": "Zeitfenster (Monate)", **ANZEIGE_AE}
    f.write("\\begin{tabular}{l" + "r"*(len(cols)-1) + "}\n\\toprule\n")
    f.write(" & ".join(esc(kopf.get(c, c)) for c in cols) + " \\\\\n\\midrule\n")
    for r in rows326:
        f.write(" & ".join(esc(r[c]) for c in cols) + " \\\\\n")
    f.write("\\bottomrule\n\\end{tabular}\n")
print("[stage6] wrote Tab_3_26_Hazard_by_Aetio_Landscape.tex")


if False:
    # ── Abb 3.24 — Hazard-Rate-Kurven pro Ätiologie (kernelgeglättet) ──
    plt.figure(figsize=(8, 5.5))
    ae_colors = {"HBV":"#C7261B","HCV":"#7B3F99","Ethyltox":"#3B6FAB",
                 "MASLD":"#E1A11A","Andere":"#777777","Unbekannt":"#B0B0B0"}

    max_cum_inc = 0.0
    for ae in AETIO_CLASSES:
        sub = joined[joined["äthiologie_cat"] == ae]
        if len(sub) < 5: continue
        t, p = kaplan_meier_estimator(sub["event"].astype(bool).values,
                                      sub["time"].astype(float).values)
        cum_inc = (1 - p) * 100
        if len(cum_inc):
            max_cum_inc = max(max_cum_inc, float(np.max(cum_inc)))
        plt.step(t / 30.4375, cum_inc, where="post",
                 color=ae_colors.get(ae,"black"), linewidth=1.6,
                 label=f"{ae} (n={len(sub)})")
    ylim_upper = max(80.0, (np.floor(max_cum_inc / 10) + 1) * 10 + 5)
    plt.xlim(0, 60); plt.ylim(0, ylim_upper)
    plt.xlabel("Monate nach OP")
    plt.ylabel("Kumulative Rezidiv-Inzidenz (%)")
    plt.legend(loc="lower right", fontsize=9, frameon=False)
    plt.grid(alpha=0.25)
    plt.tight_layout()
    plt.savefig(OUT / "Abb_3_24_Hazard_by_Aetiologie.png", dpi=200, facecolor="white")
    plt.close()
    print("[stage6] wrote Abb_3_24_Hazard_by_Aetiologie.png")


# ── Tab 3.27 — Empfohlene Intervalle (Schwellen-Verfahren) ──
def conditional_recurrence_prob(df, t0, dt):
    """P(event in (t0, t0+dt] | at risk at t0)."""
    at_risk = df[df["time"] > t0]
    if len(at_risk) < 5: return np.nan
    t, p = kaplan_meier_estimator(at_risk["event"].astype(bool).values,
                                  at_risk["time"].astype(float).values)
    if len(t) == 0: return 0.0
    def s_at(T):
        if len(t) == 0:
            return 1.0
        k = int(np.searchsorted(t, T, side="right")) - 1
        return 1.0 if k < 0 else float(p[k])
    s0 = s_at(t0); s1 = s_at(t0 + dt)
    if s0 <= 0: return np.nan
    return 1 - s1 / s0


def recommend_interval(df, phase_t0, phase_t1, threshold):
    """Largest INTERVALS_DAYS value whose conditional recurrence prob (over the phase) stays <= threshold."""
    best = None
    for I in INTERVALS_DAYS:
        p = conditional_recurrence_prob(df, phase_t0, I)
        if np.isnan(p):
            continue
        if p <= threshold + 1e-9:
            best = I
    return best


MIN_AT_RISK = 25
BOOT_B = int(os.environ.get("HCC_SMOKE_B", "0")) or 1000
rng = np.random.default_rng(SEED)

def n_at_risk_at(df, t0):
    return int((df["time"] > t0).sum())

def boot_interval_ci(df, t0, t1, threshold, B):
    """Bootstrap (Patient*innen-Resampling) der Intervallempfehlung."""
    vals = []
    n = len(df)
    if n == 0:
        return (np.nan, np.nan)
    idx_all = np.arange(n)
    for _ in range(B):
        idx = rng.choice(idx_all, size=n, replace=True)
        rec = recommend_interval(df.iloc[idx], t0, t1, threshold)
        vals.append((rec if rec is not None
                     else min(INTERVALS_DAYS)) / 7.0)
    if not vals:
        return (np.nan, np.nan)
    return (float(np.percentile(vals, 2.5)), float(np.percentile(vals, 97.5)))


rows327 = []
for ae in AETIO_CLASSES + ["Gesamt"]:
    if ae == "Gesamt":
        sub = joined
    else:
        sub = joined[joined["äthiologie_cat"] == ae]
    row = {"Ätiologie": ANZEIGE_AE.get(ae, ae)}
    for pname, t0, t1 in PHASES:
        nar = n_at_risk_at(sub, t0)
        row[(pname, "N")] = str(nar)
        if nar < MIN_AT_RISK:
            row[(pname, "V")] = "--"
            continue
        rec = recommend_interval(sub, t0, t1, THRESHOLD_PRIMARY)
        lo, hi = boot_interval_ci(sub, t0, t1, THRESHOLD_PRIMARY, BOOT_B)
        ci_txt = (f" [{lo:.0f}–{hi:.0f}]" if np.isfinite(lo) else "")
        if rec is None:
            row[(pname, "V")] = f"< 4{ci_txt}"
        else:
            row[(pname, "V")] = f"{rec // 7}{ci_txt}"
    rows327.append(row)

KOPF_PHASE = {"Phase 1: 0-12 Mo":  "Phase 1: 0--12 Monate",
              "Phase 2: 12-24 Mo": "Phase 2: 12--24 Monate",
              "Phase 3: 24-60 Mo": "Phase 3: 24--60 Monate"}

with (OUT / "Tab_3_27_Intervals_Recommended_Landscape.tex").open(
        "w", encoding="utf-8") as f:
    f.write("% Intervall-Vorschlaege (Schwelle 5%) je Ätiologie und Phase, "
            "Spanne aus " + str(BOOT_B) + " Bootstrap-Ziehungen "
            "(Patient*innen-Resampling), N = unter Beobachtung zu "
            "Phasenbeginn. Zellen mit N < " + str(MIN_AT_RISK) + " ohne "
            "Vorschlag (--). Explorativ und hypothesengenerierend, keine "
            "Empfehlung.\n")
    f.write("\\begin{tabular}{l" + "rr" * len(PHASES) + "}\n\\toprule\n")
    f.write(" & " + " & ".join("\\multicolumn{2}{c}{" + KOPF_PHASE[p[0]] + "}"
                               for p in PHASES) + " \\\\\n")
    f.write(" ".join(f"\\cmidrule(lr){{{2 + 2 * j}-{3 + 2 * j}}}"
                     for j in range(len(PHASES))) + "\n")
    f.write("Ätiologie" + " & N & Vorschlag (Wochen)" * len(PHASES)
            + " \\\\\n\\midrule\n")
    for r in rows327:
        zellen = [esc(r["Ätiologie"])]
        for pname, _t0, _t1 in PHASES:
            zellen += [esc(r[(pname, "N")]), esc(r[(pname, "V")])]
        f.write(" & ".join(zellen) + " \\\\\n")
    f.write("\\bottomrule\n\\end{tabular}\n")
print(f"[stage6] wrote Tab_3_27_Intervals_Recommended_Landscape.tex "
      f"(Bootstrap B={BOOT_B}, MIN_AT_RISK={MIN_AT_RISK})")


# ── Schwellen-Sensitivitaet ──
def _rec_txt(sub, t0, t1, thr):
    rec = recommend_interval(sub, t0, t1, thr)
    return "< 4" if rec is None else f"{rec // 7}"


THRESHOLDS_ALL = (THRESHOLD_SENS[0], THRESHOLD_PRIMARY, THRESHOLD_SENS[1])
rows327b = []
n_zellen327b = 0
for ae in AETIO_CLASSES + ["Gesamt"]:
    sub = joined if ae == "Gesamt" else joined[joined["äthiologie_cat"] == ae]
    row = {"Ätiologie": ANZEIGE_AE.get(ae, ae)}
    for pname, t0, t1 in PHASES:
        if n_at_risk_at(sub, t0) < MIN_AT_RISK:
            for thr in THRESHOLDS_ALL:
                row[(pname, thr)] = "--"
            continue
        n_zellen327b += 1
        for thr in THRESHOLDS_ALL:
            row[(pname, thr)] = _rec_txt(sub, t0, t1, thr)
    rows327b.append(row)

if n_zellen327b:
    k = len(THRESHOLDS_ALL)
    spalten = k * len(PHASES)
    with (OUT / "Tab_3_27b_Intervals_Threshold_Sensitivity.tex").open(
            "w", encoding="utf-8") as f:
        f.write("% Intervall-Vorschlaege bei drei Risikoschwellen. Die Spalten "
                "zu 5% entsprechen Tab_3_27_Intervals_Recommended. Zellen mit "
                "N < " + str(MIN_AT_RISK) + " ohne Vorschlag (--). Explorativ "
                "und hypothesengenerierend.\n")
        f.write("\\begin{tabular}{l"
                + ">{\\raggedleft\\arraybackslash}p{0.9cm}" * spalten
                + "}\n\\toprule\n")
        f.write(" & \\multicolumn{" + str(spalten)
                + "}{c}{Vorschlag (Wochen) je Schwelle} \\\\\n")
        f.write("\\cmidrule(lr){2-" + str(spalten + 1) + "}\n")
        f.write(" & " + " & ".join("\\multicolumn{" + str(k) + "}{c}{"
                                   + KOPF_PHASE[p[0]] + "}" for p in PHASES)
                + " \\\\\n")
        f.write(" ".join(f"\\cmidrule(lr){{{2 + k * j}-{1 + k * (j + 1)}}}"
                         for j in range(len(PHASES))) + "\n")
        schwellen = " & ".join(f"{round(thr * 100)}\\,\\%"
                               for thr in THRESHOLDS_ALL)
        f.write("Ätiologie" + (" & " + schwellen) * len(PHASES)
                + " \\\\\n\\midrule\n")
        for r in rows327b:
            zellen = [esc(r["Ätiologie"])]
            for pname, _t0, _t1 in PHASES:
                zellen += [esc(r[(pname, thr)]) for thr in THRESHOLDS_ALL]
            f.write(" & ".join(zellen) + " \\\\\n")
        f.write("\\bottomrule\n\\end{tabular}\n")
    print(f"[stage6] wrote Tab_3_27b_Intervals_Threshold_Sensitivity.tex "
          f"({n_zellen327b} Zellen, Schwellen "
          f"{', '.join(f'{t:.0%}' for t in THRESHOLDS_ALL)})")


intv_matrix = np.full((len(AETIO_CLASSES), len(PHASES)), np.nan)
for i, ae in enumerate(AETIO_CLASSES):
    sub = joined[joined["äthiologie_cat"] == ae]
    for j, (_pn, t0, t1) in enumerate(PHASES):
        if n_at_risk_at(sub, t0) < MIN_AT_RISK:
            continue
        rec = recommend_interval(sub, t0, t1, THRESHOLD_PRIMARY)
        intv_matrix[i, j] = (rec if rec is not None
                             else min(INTERVALS_DAYS)) // 7


# ── Tab 3.28 — Counterfactual schemes evaluation ──
recurrers = joined[joined["event"] == 1].copy()
print(f"[stage6] recurrers for counterfactual: {len(recurrers)}")

HORIZON_DAYS = PHASES[-1][2]

def phase_index(t):
    for j, (_pn, t0, t1) in enumerate(PHASES):
        if t0 <= t < t1:
            return j
    return len(PHASES) - 1

def scan_calendar(intervals_by_phase, horizon=HORIZON_DAYS):
    """Scan-Termine ab OP: naechster Termin = aktueller + Intervall der Phase, in der der aktuelle Termin liegt."""
    times = []
    t = 0.0
    while True:
        iv = intervals_by_phase[phase_index(t)]
        t = t + iv
        if t > horizon:
            break
        times.append(t)
    return np.array(times)

def delay_under_calendar(t_event, cal):
    """Verzoegerung >= 0 garantiert: Rezidive nach dem 5-Jahres-Kalender (time > 1825 d kommt bei einzelnen Spaetrezidiven…"""
    later = cal[cal >= t_event]
    if len(later) == 0:
        iv = (cal[-1] - cal[-2]) if len(cal) > 1 else 84.0
        t = float(cal[-1])
        while t < t_event:
            t += iv
        return t - t_event
    return float(later[0] - t_event)

def scans_per_year(cal):
    return len(cal) / (HORIZON_DAYS / 365.0)

r_q1, r_q2 = np.nanquantile(joined["oof_risk"].astype(float), [1/3, 2/3])
_rg = pd.cut(joined["oof_risk"].astype(float),
             [-np.inf, r_q1, r_q2, np.inf],
             labels=["niedrig", "mittel", "hoch"])
for _lab in ["niedrig", "mittel", "hoch"]:
    _m = (_rg == _lab).to_numpy()
    if _m.sum() == 0:
        continue
    _t_km, _s_km = kaplan_meier_estimator(
        joined.loc[_m, "event"].astype(bool).to_numpy(),
        joined.loc[_m, "time"].astype(float).to_numpy())
    _below = _t_km[_s_km <= 0.5]
    _med = "nicht erreicht" if len(_below) == 0 else f"{_below[0] / 30.4375:.1f} Mo"
    print(f"[stage6] Median TTR Risikodrittel {_lab:<8s} = {_med} "
          f"(n={int(_m.sum())}, Ereignisse={int(joined.loc[_m, 'event'].sum())})")

def risk_interval(r):
    if pd.isna(r): return 12*7
    if r <= r_q1: return 16*7
    if r <= r_q2: return 12*7
    return 8*7

def aetio_intervals(ae_label):
    """Phasen-Intervalle (Tage) des Ätio-Schemas; NaN-Zellen -> 12 Wo."""
    if pd.isna(ae_label) or ae_label not in AETIO_CLASSES:
        ae_label = "Unbekannt"
    i_ae = AETIO_CLASSES.index(ae_label)
    out = []
    for j in range(len(PHASES)):
        v = intv_matrix[i_ae, j]
        out.append(int(v) * 7 if np.isfinite(v) else 12 * 7)
    return out

SCHEME_ORDER = ["S3-Standard (12/24/24 Wo)", "Flach 12 Wo", "Flach 24 Wo",
                "Risiko-adaptiert", "Ätio-adaptiert", "Kombiniert"]

def calendars_for_patient(row):
    ae = row["äthiologie_cat"]
    riv = risk_interval(row["oof_risk"])
    aiv = aetio_intervals(ae)
    return {
        "S3-Standard (12/24/24 Wo)": scan_calendar([84, 168, 168]),
        "Flach 12 Wo":               scan_calendar([84, 84, 84]),
        "Flach 24 Wo":               scan_calendar([168, 168, 168]),
        "Risiko-adaptiert":          scan_calendar([riv] * len(PHASES)),
        "Ätio-adaptiert":            scan_calendar(aiv),
        "Kombiniert":                scan_calendar(
            [min(riv, a) for a in aiv]),
    }

delays_all = {sch: [] for sch in SCHEME_ORDER}
for _, row in recurrers.iterrows():
    cals = calendars_for_patient(row)
    t = float(row["time"])
    for sch in SCHEME_ORDER:
        delays_all[sch].append(delay_under_calendar(t, cals[sch]))

scans_all = {sch: [] for sch in SCHEME_ORDER}
for _, row in joined.iterrows():
    cals = calendars_for_patient(row)
    for sch in SCHEME_ORDER:
        scans_all[sch].append(scans_per_year(cals[sch]))
scan_count_map = {sch: float(np.mean(v)) for sch, v in scans_all.items()}

for sch in SCHEME_ORDER:
    assert (np.array(delays_all[sch]) >= 0).all(), \
        f"negative Detektionsverzoegerung in Schema {sch}"

ANZEIGE_SCHEMA = {"S3-Standard (12/24/24 Wo)": "S3-Standard (12/24/24)",
                  "Flach 12 Wo":               "Flach (12)",
                  "Flach 24 Wo":               "Flach (24)",
                  "Risiko-adaptiert":          "Risikoadaptiert (16/12/8)",
                  "Ätio-adaptiert":            "Ätiologieadaptiert",
                  "Kombiniert":                "Kombiniert"}
rows328 = []
for sch in SCHEME_ORDER:
    dl = np.array(delays_all[sch])
    rows328.append({
        "Schema": ANZEIGE_SCHEMA[sch],
        "Median": f"{np.median(dl) / 7:.1f}",
        "IQR":    f"({np.quantile(dl,0.25)/7:.1f}--"
                  f"{np.quantile(dl,0.75)/7:.1f})",
        "Last":   f"{scan_count_map[sch]:.1f}",
    })

with (OUT / "Tab_3_28_Counterfactual_Schemes_Landscape.tex").open(
        "w", encoding="utf-8") as f:
    f.write("% Kontrafaktische Simulation: Verzoegerung bis zum naechsten "
            "Termin je Nachsorgeschema, alle Schemata ueber denselben "
            "phasenweisen Terminkalender ab OP. S3-Standard 12 Wochen im "
            "ersten Jahr, 24 Wochen ab dem zweiten (Wechsel zum Ultraschall "
            "nach dem zweiten Jahr nicht abgebildet). Flach 12 und 24 Wochen "
            "als Grenzfaelle. Risikoadaptiert: Drittel der OOF-Scores des "
            "Ausgangsmodells. Annahme: dokumentiertes Rezidivdatum = "
            "fruehestmoegliches Entdeckungsdatum. Bildgebungen pro Jahr "
            "ueber fuenf Jahre, ganze Analysekohorte, ohne Abbruch bei "
            "Rezidiv.\n")
    f.write("\\begin{tabular}{lrrr}\n\\toprule\n")
    f.write(" & \\multicolumn{2}{c}{Verzögerung (Wochen)} & \\\\\n")
    f.write("\\cmidrule(lr){2-3}\n")
    f.write("Schema (Wochen) & Median & IQR & Bildgebungen pro Jahr "
            "\\\\\n\\midrule\n")
    for r in rows328:
        f.write(" & ".join(esc(r[c]) for c in ("Schema", "Median", "IQR", "Last"))
                + " \\\\\n")
    f.write("\\bottomrule\n\\end{tabular}\n")
print("[stage6] wrote Tab_3_28_Counterfactual_Schemes_Landscape.tex")


# ── Abb 3.26 — Trade-off curve: scans/year vs median Δt ──
fig, ax = plt.subplots(figsize=(7.5, 5.5))
colors_sch = {"S3-Standard (12/24/24 Wo)": "#00441B",
              "Flach 12 Wo":               "#3B6FAB",
              "Flach 24 Wo":               "#7B3F99",
              "Risiko-adaptiert":          "#E1A11A",
              "Ätio-adaptiert":            "#555555",
              "Kombiniert":                "#C7261B"}
LAGE_SCH = {"S3-Standard (12/24/24 Wo)": "unten",
            "Flach 12 Wo":               "links",
            "Flach 24 Wo":               "rechts",
            "Risiko-adaptiert":          "rechts",
            "Ätio-adaptiert":            "unten",
            "Kombiniert":                "rechts"}
for sch in SCHEME_ORDER:
    dl = np.array(delays_all[sch]) / 7
    x = scan_count_map[sch]
    y = np.median(dl)
    q1, q3 = np.quantile(dl, 0.25), np.quantile(dl, 0.75)
    ax.errorbar(x, y, yerr=[[y - q1], [q3 - y]], fmt="none",
                ecolor=colors_sch[sch], elinewidth=1.8, capsize=4, zorder=2)
    ax.scatter(x, y, s=120, color=colors_sch[sch],
               edgecolor="black", zorder=3, label=ANZEIGE_SCHEMA[sch])
    lage = LAGE_SCH[sch]
    if lage == "unten":
        ax.annotate(ANZEIGE_SCHEMA[sch], (x, q1), xytext=(0, -7),
                    textcoords="offset points", ha="center", va="top",
                    fontsize=10)
    else:
        ax.annotate(ANZEIGE_SCHEMA[sch], (x, y),
                    xytext=(10 if lage == "rechts" else -10, 0),
                    textcoords="offset points",
                    ha="left" if lage == "rechts" else "right",
                    va="center", fontsize=10)

ax.set_xlim(1.5, 7.4)
ax.set_ylim(0, 20)
ax.set_xlabel("Bildgebungen pro Jahr", fontsize=11)
ax.set_ylabel("Detektionsverzögerung (Wochen)", fontsize=11)
ax.tick_params(labelsize=10)
ax.grid(alpha=0.25)
plt.tight_layout()
plt.savefig(OUT / "Abb_3_26_Tradeoff_Curve.png", dpi=200, facecolor="white")
plt.close()
print("[stage6] wrote Abb_3_26_Tradeoff_Curve.png")

print("[stage6] done.")
