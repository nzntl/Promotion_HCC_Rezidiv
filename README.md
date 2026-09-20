# Analysecode: Leberregeneration und Rezidivrisiko nach HCC-Resektion

Auswertungscode einer retrospektiven Kohortenstudie zum hepatozellulären
Karzinom. Untersucht wird, ob die postoperative Regeneration der Leber, gemessen
volumetrisch und funktionell, das Rezidivrisiko über die klinischen
Ausgangsmerkmale hinaus vorhersagt.

## Daten

**Die Originaldaten sind nicht Teil dieses Repositoriums.** Sie enthalten
personenbezogene Verlaufsdaten aus der klinischen Routine und werden nicht
veröffentlicht. Eine begründete Anfrage an die Chirurgische Klinik des
Universitätsklinikums Heidelberg ist möglich, eine Weitergabe ist unter dem
geltenden Ethikvotum aber unwahrscheinlich. Welche Dateien und Blätter der Code
erwartet, steht in `01_Daten/PLATZHALTER.md`.

**Für einen lauffähigen Test gibt es einen synthetischen Datensatz.** Er ist
nicht Teil dieses Repositoriums, wird aber auf Anfrage weitergegeben. Keine
Zeile darin beschreibt eine reale Person. Aufbau, Spalten, Wertebereiche und
Fehlmuster entsprechen den Originaldaten, sodass der komplette Code darauf
läuft. Die Zahlen, die dabei entstehen, sind nicht die Zahlen der Arbeit und
dürfen nicht als solche zitiert werden.

Die beiden Mappen des synthetischen Datensatzes tragen bereits die Dateinamen
und Blattnamen, die der Code fest erwartet. Umbenannt werden muss nichts, sie
gehören nur nach `01_Daten/`. Danach genügt aus dem Projektstamm:

```
Rscript 02_Code/00_Master.R
```

**Weil die Dateinamen dieselben sind wie bei den Originaldaten, überschreibt
das Einspielen vorhandene echte Daten.** Diesen Schritt nie in einem
Arbeitsordner ausführen, in dem echte Daten liegen.

## Reproduzierbarkeit

Auf derselben Plattform ist der Lauf bitgenau wiederholbar. Über Plattformen
hinweg gilt das für alle Verfahren außer der GBSA. Deren Hyperparameter-Suche
nimmt auf anderer Hardware in einzelnen Falten einen anderen Weg, und die
Konkordanzindizes weichen dann um **bis zu 0,012** ab. Die Schlussfolgerungen
ändern sich dadurch nicht. Wer die Werte der Arbeit exakt nachrechnen will,
setzt `HCC_HP_REPLAY=1` und übernimmt damit die Hyperparameter des
Referenzlaufs aus `02_Code/py/referenz/`; Einzelheiten weiter unten.

Belegt ist das durch zwei Läufe. Der Referenzlauf vom 9. September 2026 unter
Windows trägt die Tabellen der Arbeit. Eine Nachrechnung unter Linux ergab für
Cox-LASSO und Random Survival Forest dieselben Werte bis zur letzten Stelle,
für die GBSA die genannte Abweichung. Ein Wiederholungslauf am 20. September
2026 auf der Referenzplattform erzeugte alle Ergebnistabellen erneut
zeichengleich, bei bytegleichen Hyperparameter-Dateien.

## Aufbau

Die Auswertung läuft in einem R-Strang und einem Python-Strang. Gestartet wird
alles über ein Skript, aufgerufen aus dem Projektstamm:

```r
source("./02_Code/00_Master.R")
```

Der Master leert zu Beginn die Ausgabeverzeichnisse `03_Tables_Figures` und
`05_Target_Structure/01_figs_tabs`, ruft die R-Skripte in fester Reihenfolge
auf und startet dann die Python-Skripte. Danach laufen noch fünf R-Skripte:
Softwaremanifest, Referenztabellen, Variablenübersicht, Sammeldatei und
Spiegelung in den Manuskriptbaum. Bricht eines der Python-Skripte ab, endet der
Master mit Fehlerstatus.

Das Verzeichnis `logs/` legt der Master nicht selbst an. Wer die Ausgabe
umlenken will, erzeugt es vorher.

### R-Strang

| Skript | Aufgabe |
|---|---|
| `01_Preparation_Selection.R` | liest die klinische Rohmappe: Eignungsfilter, Freitext-Aufbereitung, abgeleitete Größen, Ausschlusskette; schreibt die aufbereiteten Datensätze, von denen alle weiteren Stufen leben |
| `02_Meta_and_Static_Analysis.R`, `02b_Correlation_Heatmap.R` | Übersichtstabellen, Kollinearitätsdiagnostik |
| `03b_Hazard_Plots.R` | kumulative Inzidenz unter Konkurrenzrisiko, geglätteter Hazard-Verlauf |
| `04_Descriptive_Tables.R`, `05_Descriptive_Figures.R` | Kohortenbeschreibung, Fehlendprofil, Überlebenskurven, Verlaufsabbildungen |
| `06_Cox_Analysis.R`, `06b_Cox_Chan_Postop.R` | univariate und multivariate Cox-Modelle A bis D, Annahmeprüfung, Splines, Interaktionen |
| `06c_FineGray_CompetingRisk.R` | Konkurrenzrisiko-Analyse |
| `06d_Cox_LASSO_Main.R` | penalisierte Cox-Regression als Sensitivität |
| `06e_TimingRestricted_PostopVol.R` | Zeitfenster-Sensitivität der postoperativen Volumetrie |
| `04b_Joint_Model_VarianteA.R` | gemeinsames Verlaufs- und Ereigniszeitmodell |
| `07`, `09`, `10`, `15` | Variablenlegende, Berichtsstandard nach TRIPOD+AI, Softwaremanifest, Variablenübersicht |
| `13_Imaging_Density_Audit.R` | Dichte der Bildgebung; liest neben den aufbereiteten Daten auch die Rohmappe mit den Bildgebungsterminen |
| `16_Diskussion_Zusatzrechnungen.R` | Zusatzrechnungen zu Abschnitt 4.3.3; liest ebenfalls die Terminmappe und schreibt nur ins Protokoll, Zeilen mit `[textdiag-4.3.3]` |
| `14_Static_Reference_Tables.R` | derzeit stillgelegt, erzeugt keine Ausgabe; läuft nur mit, um eine veraltete Datei zu entfernen |
| `08_Aggregate_View.R`, `00b_Mirror_Figs.R` | Sammeldatei aller Tabellenfragmente, Spiegelung der Ausgaben in den Manuskriptbaum |
| `05b_Regenerationsverlauf.R` | beschreibende Prüfung des Volumenverlaufs; eigenständiges Skript, nicht Teil des Masterlaufs |

### Python-Strang

Vorhersagemodelle mit verschachtelter Kreuzvalidierung. Die Skripte lesen
ausschließlich die von Stufe 1 erzeugten Zwischenstände und schreiben ihre
Ergebnisse als LaTeX-Fragmente und Abbildungen zurück.

| Skript | Aufgabe |
|---|---|
| `stage2_nested_cv.py` | Ausgangsmodell |
| `stage3_nested_cv.py` | Landmark-Modelle POD 90 und POD 180 |
| `stage3c_nested_cv_ablation.py` | Blockvergleich über kumulative Merkmalsblöcke |
| `stage3d_hpt_table.py` | Hyperparameter-Tabelle aus den Tuning-Protokollen |
| `stage4_*.py` | Modellvergleich, Differenzmaße, Kalibrierung, Entscheidungskurve, Risikodrittel |
| `stage6_fruerezidiv.py` | Rezidivrisiko in festen Zeitfenstern |
| `stage6_surveillance.py` | Ableitung von Nachsorgeintervallen, Vergleich von Nachsorgeschemata |
| `stage7_sensitivity.py` | Sensitivitätsrechnungen zum Landmark-Modell |

Zwischenstände, die der Lauf selbst erzeugt und die nicht versioniert sind:
die aufbereiteten Datensätze in `01_Daten/`, der Zwischenspeicher
`02_Code/py/_cache/` und die Hyperparameter-Protokolle `02_Code/py/*.json`.

Der Zwischenspeicher enthält je Pseudonym Beobachtungszeit, Ereignis und
Risikoscore, also personenbezogene Daten. Er wird wie die Daten selbst
behandelt und nie veröffentlicht.

### Hyperparameter des Referenzlaufs und Wiedergabe-Modus

Die je Falte gewählten Hyperparameter des Referenzlaufs liegen versioniert in
`02_Code/py/referenz/` (drei JSON-Dateien, ohne Patientendaten). Mit

```
HCC_HP_REPLAY=1
```

übernehmen `stage2_nested_cv.py` und `stage3_nested_cv.py` diese Werte und
überspringen die Optuna-Suche. Beide melden das im Protokoll mit einer
`[replay]`-Zeile. Der Python-Strang läuft dann in Minuten statt Stunden. Ohne
die Variable läuft die Suche unverändert.

Auch mit festen Hyperparametern bleiben bei der GBSA Abweichungen in der
dritten Nachkommastelle möglich.

### Zuordnung der Ausgabedateien zu den Tabellen der Arbeit

Die Dateinamen tragen die Nummern eines früheren Manuskriptstands.

| Tabelle der Arbeit | Datei in `03_Tables_Figures/` |
|---|---|
| 2.1 | `Tab_Exclusion_Steps` |
| 3.1, 3.2 | `Tab_3_1_Charakteristika`, `Tab_3_2_Missing_Profil` |
| 3.3, 3.4, 3.5 | `Tab_Volumetrie_Verfuegbarkeit`, `Tab_3_3_Rezidiv_Todesfall_Uebersicht`, `Tab_3_4_Volumetrie_Eckwerte` |
| 3.6, 3.7 | `Tab_3_22_Rezidivort_Verteilung`, `Tab_3_23_Therapie_Erstrezidiv` |
| 3.8, 3.9, 3.10 | `Tab_3_5_Univariate_Cox`, `Tab_3_6_Multivariate_Cox_ABC`, `Tab_GOF_Cox_A_D` |
| 3.11, 3.12 | `Tab_EPV_Diagnostics`, `Tab_3_8_Cox_Chan_Postop` |
| 3.13, 3.14 | `Tab_RCS_Linearity`, `Tab_Interactions_ModelA` |
| 3.15, 3.16a, 3.16b | `Tab_3_10_Stage2_ML_Performance`, `Tab_3_11_Stage3_Landmark_POD90`, `Tab_3_12_Stage3_Landmark_POD180` |
| 3.17, 3.18, 3.20 | `Tab_3_15_Consolidated_Comparison`, `Tab_3_17_Ablation`, `Tab_3_18_Sensitivity_Analysis` |
| 3.19 | `Tab_3_14_JointModel_Posterior` |
| 3.21, 3.22 | `Tab_3_19_Fruehrezidiv_Buckets_Deskriptiv`, `Tab_3_26_Hazard_by_Aetio` |
| 3.23, 3.24, 3.25 | `Tab_FineGray_Competing`, `Tab_Sensitivity_90Tage`, `Tab_3_28b_Imaging_Density_Audit` |
| 3.26, 3.27, 3.28 | `Tab_3_27_Intervals_Recommended`, `Tab_3_27b_Intervals_Threshold_Sensitivity`, `Tab_3_28_Counterfactual_Schemes` |
| A.1, A.2 | `Tab_TRIPOD_AI_Conformity`, `Tab_HPT_Grids` |

## Voraussetzungen

R 4.6 mit den Paketen, die `00_Install_Packages.R` nachinstalliert. Python 3.12
mit den in `02_Code/requirements.txt` festgelegten Versionen, insbesondere
scikit-survival, scikit-learn, optuna und lifelines. Die vollständige
Softwareumgebung des Referenzlaufs steht in `02_Code/Software_Environment_full.txt`.

Einrichtung des Python-Strangs:

```
python -m venv <zielpfad>
<zielpfad>/bin/python -m pip install -r 02_Code/requirements.txt
```

Unter Windows liegt der Interpreter unter `<zielpfad>\Scripts\python.exe`.

Der Python-Interpreter wird portabel aufgelöst. `00_Master.R` liest die
Umgebungsvariable `HCC_PYTHON`; ist sie nicht gesetzt, sucht er `python`,
`python3` und `py` im Suchpfad. Fehlt ein Interpreter, wird der Python-Strang
übersprungen.

Die R-Skripte verwenden Bezeichner mit Umlauten. Unter Linux und macOS ist
deshalb eine UTF-8-Locale nötig, etwa `LC_ALL=C.UTF-8`; unter Windows ab R 4.2
ist nichts zu tun.

Laufzeit, gemessen am 20. September 2026 auf zwanzig Kernen: R-Strang gut zwei
Minuten, gemeinsames Modell siebenunddreißig Minuten, Python-Strang knapp
dreißig Minuten, zusammen etwa eine Stunde. Auf weniger Kernen dauert vor allem
das gemeinsame Modell deutlich länger, bis zu mehreren Stunden. Mit
`HCC_HP_REPLAY=1` verkürzt sich der Python-Strang auf wenige Minuten.

## Hinweise zum Lesen des Codes

Die Skripte tragen je einen Kurzheader und Abschnittsmarker, sonst kaum
Kommentare. Die methodischen Begründungen stehen im Methodenteil der Arbeit.

Die Variablennamen folgen der deutschen Datenerhebung. Einige sind leicht zu
verwechseln. `m` ist die Körpergröße in Metern und nicht das Geschlecht, das in
`mwd` steht. `Quick` ist der Quick-Wert in Prozent und nicht die INR. `GOT`
entspricht der AST, `GPT` der ALT.

## Zitieren

Zental N (2026). Analysecode zur Dissertation zur Leberregeneration und zum
Rezidivrisiko nach HCC-Resektion. Medizinische Fakultät Heidelberg.
https://github.com/nzntl/Promotion_HCC_Rezidiv

## Lizenz

Der Code steht unter der MIT-Lizenz, siehe `LICENSE`.

Die Lizenz gilt ausschließlich für den Code. Weder die Originaldaten noch der
synthetische Datensatz sind Teil dieses Repositoriums, beide sind nicht
mitlizenziert.
