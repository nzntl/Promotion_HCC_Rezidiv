# Hyperparameter des Referenzlaufs

Hier liegen die drei Dateien des Referenzlaufs vom 9. September 2026 (Windows), auf dem die Tabellen der Arbeit beruhen:

- `stage2_hyperparams_final.json`
- `stage3_pod90_hyperparams.json`
- `stage3_pod180_hyperparams.json`

Sie entstehen bei jedem Lauf in `02_Code/py/` und enthalten je äußerer Falte nur die gewählten Einstellungen, keine Patientendaten.
Mit `HCC_HP_REPLAY=1` lesen `stage2_nested_cv.py` und `stage3_nested_cv.py` sie von hier und überspringen die Optuna-Suche.

Am 20. September 2026 wurde der Referenzlauf auf derselben Plattform wiederholt. Die drei Dateien des
Wiederholungslaufs sind mit diesen bytegleich, und die drei ML-Tabellen der Arbeit entstanden erneut zeichengleich.
