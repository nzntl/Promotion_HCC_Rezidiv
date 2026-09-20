"""Hyperparameter-Tabelle des Anhangs aus den Tuning-Protokollen von Stufe 2 und 3.

Liest stage2_hyperparams_final.json und stage3_pod180_hyperparams.json; schreibt Tab_HPT_Grids.tex.
"""

from __future__ import annotations

import json
import statistics
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PY_DIR = ROOT / "02_Code" / "py"
OUT_DIR = ROOT / "03_Tables_Figures"
OUT_DIR.mkdir(parents=True, exist_ok=True)

SEARCH_SPACE = {
    "Cox-LASSO": [("l1_ratio", "[0.1; 1.0]"),
                  ("alpha_min_ratio", "[0.01; 0.2], log")],
    "RSF":       [("n_estimators", "{100, 200, 300, 400}"),
                  ("max_depth", "[3; 8]"),
                  ("min_samples_leaf", "[4; 20]")],
    "GBSA":      [("n_estimators", "{100, 200, 300, 400}"),
                  ("learning_rate", "[0.02; 0.2], log"),
                  ("max_depth", "[2; 5]")],
}
PARAM_DE = {
    "l1_ratio":         "L1-Anteil",
    "alpha_min_ratio":  "Strafpfad-Untergrenze",
    "n_estimators":     "Anzahl der Bäume",
    "max_depth":        "Maximale Baumtiefe",
    "min_samples_leaf": "Mindestgröße der Endknoten",
    "learning_rate":    "Lernrate",
}
MODEL_ORDER = ["Cox-LASSO", "RSF", "GBSA"]

STRAENGE = [
    ("Ausgangsmodell", PY_DIR / "stage2_hyperparams_final.json"),
    ("Landmark-Modell (POD 180)", PY_DIR / "stage3_pod180_hyperparams.json"),
]


def tex_escape(s: str) -> str:
    return (str(s).replace("\\", r"\textbackslash{}").replace("_", r"\_")
            .replace("{", r"\{").replace("}", r"\}").replace("%", r"\%")
            .replace("&", r"\&").replace("#", r"\#"))


def fmt(v: float) -> str:
    if float(v).is_integer():
        return str(int(v))
    return f"{v:.3f}"


def folds_of(entry):
    """Tuning-Protokoll je Modell: Liste der pro Outer-Fold gewaehlten Werte."""
    if isinstance(entry, list):
        return [f for f in entry if isinstance(f, dict)]
    if isinstance(entry, dict):
        vals = list(entry.values())
        if vals and all(isinstance(v, dict) for v in vals):
            return vals
        return [entry]
    return []


rows = []
n_folds_seen = set()
for strang, path in STRAENGE:
    if not path.exists():
        sys.exit(f"[stage3d] fehlt: {path}")
    log = json.loads(path.read_text(encoding="utf-8"))
    block = []
    for model in MODEL_ORDER:
        if model not in log:
            continue
        folds = folds_of(log[model])
        n_folds_seen.add(len(folds))
        for i, (param, space) in enumerate(SEARCH_SPACE[model]):
            vals = [f[param] for f in folds if param in f]
            if not vals:
                sys.exit(f"[stage3d] {strang}/{model}: Parameter {param} fehlt im Protokoll")
            med = statistics.median(vals)
            block.append((model if i == 0 else "",
                          PARAM_DE[param], space, fmt(med)))
    rows.append((strang, block))

n_folds = sorted(n_folds_seen)
n_folds_txt = "/".join(str(n) for n in n_folds)
header = ("% Hyperparameter-Suchraeume (Optuna) und Median der pro-Fold gewaehlten "
          f"Werte ueber die {n_folds_txt} aeusseren CV-Folds. Strang Ausgangsmodell = "
          "praeoperativer Snapshot; Strang Landmark-Modell = Landmark POD 180. "
          "Aus stage2_hyperparams_final.json und stage3_pod180_hyperparams.json.")
lines = [header, "\\begin{tabular}{lllr}", "\\toprule",
         "Modell & Hyperparameter & Suchraum & Median \\\\",
         "\\midrule"]
for b, (strang, block) in enumerate(rows):
    if b > 0:
        lines.append("\\addlinespace")
    lines.append(f"\\multicolumn{{4}}{{@{{}}l}}{{\\textit{{{strang}}}}} \\\\[2pt]")
    for model, param, space, med in block:
        lines.append(" & ".join([model, tex_escape(param),
                                 tex_escape(space), med]) + " \\\\")
lines += ["\\bottomrule", "\\end{tabular}", ""]

out = OUT_DIR / "Tab_HPT_Grids.tex"
out.write_text("\n".join(lines), encoding="utf-8", newline="\n")
print(f"[stage3d] wrote {out.name}: "
      f"{sum(len(b) for _s, b in rows)} Zeilen, Folds {n_folds_txt}")
