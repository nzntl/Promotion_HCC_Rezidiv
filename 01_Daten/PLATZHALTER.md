# Datenverzeichnis

Dieses Verzeichnis bleibt im Repository leer. Die Auswertung liest hier zwei
Arbeitsmappen ein, die personenbezogene Verlaufsdaten enthalten und deshalb
nicht veroeffentlicht werden:

- `20260527_hcc_combined_final.xlsx`, Blatt `combined df`
- `20260817_bildgebungstermine_alle.xlsx`, Blatt `Termine_lang`

## Ohne die Originaldaten

Es gibt einen synthetischen Ersatzdatensatz. Er ist nicht Teil dieses
Repositoriums, wird aber auf Anfrage weitergegeben. Keine Zeile darin
beschreibt eine reale Person. Beide Mappen tragen bereits die oben genannten
Dateinamen und Blattnamen, sie muessen also nur hierher gelegt werden, nicht
umbenannt. Danach laeuft die gesamte Auswertung durch.

Die Zahlen aus einem solchen Lauf sind nicht die Zahlen der Arbeit und duerfen
nicht als solche zitiert werden. Eine Beschreibung von Herstellung und Grenzen
liegt dem Datensatz bei.

**Weil die Dateinamen identisch sind, ueberschreibt das Einspielen vorhandene
Originaldaten.** Diesen Schritt nie in einem Arbeitsordner ausfuehren, in dem
echte Daten liegen.

## Was der Lauf hier selbst ablegt

Die Aufbereitung (`01_Preparation_Selection.R`) schreibt ihre Zwischenstaende
`data_static.xlsx`, `data_dynamic.xlsx` und `data_dynamic_surv.xlsx` in
dasselbe Verzeichnis. Das gemeinsame Modell legt hier ausserdem seine
Fit-Objekte (`jm_fit_*.rds`) ab. Beides ist durch `.gitignore` ausgeschlossen
und gehoert nicht ins Repository.
