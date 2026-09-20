# 09_TRIPOD_AI_Table.R -- Konformitaetstabelle nach TRIPOD+AI (Collins et al. 2024).
# Schreibt Tab_TRIPOD_AI_Conformity.tex.

suppressPackageStartupMessages({ library(dplyr) })

tex_escape_cell <- function(x) {
  x <- as.character(x); x[is.na(x)] <- "--"
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x); x
}

ref_cell <- function(x) {
  x <- as.character(x)
  labs <- regmatches(x, gregexpr("@@[^@]+@@", x))
  out <- x
  for (i in seq_along(x)) {
    for (j in seq_along(labs[[i]]))
      out[i] <- sub(labs[[i]][j], sprintf("REFTOK%d", j), out[i], fixed = TRUE)
  }
  out <- tex_escape_cell(out)
  for (i in seq_along(x)) {
    for (j in seq_along(labs[[i]]))
      out[i] <- sub(sprintf("REFTOK%d", j),
                    sprintf("\\ref{%s}", gsub("@@", "", labs[[i]][j])),
                    out[i], fixed = TRUE)
  }
  out
}

write_table_tex <- function(tbl, path, caption = NULL, align) {
  header <- paste(tex_escape_cell(names(tbl)), collapse = " & ")
  body <- apply(tbl, 1, function(row) paste(tex_escape_cell(row), collapse = " & "))
  body <- paste(body, collapse = " \\\\\n")
  out <- c(if (!is.null(caption)) sprintf("%% %s", caption),
           sprintf("\\begin{tabular}{%s}", align), "\\toprule",
           paste0(header, " \\\\"), "\\midrule",
           paste0(body, " \\\\"), "\\bottomrule",
           "\\end{tabular}")
  con <- file(path, "w", encoding = "UTF-8"); writeLines(out, con); close(con)
}

tripod <- data.frame(
  Item = c("1","2","3a","3b","3c","4","5a","5b","6a","6b","6c","7",
           "8a","8b","8c","9a","9b","9c","10","11","12a","12b","12c","12d","12e","12f","12g",
           "13","14","15","16","17","18a","18b","18c","18d","18e","18f","19",
           "20a","20b","20c","21","22","23a","23b","24","25","26","27a","27b","27c"),
  Bereich = c(
    "Titel","Zusammenfassung","Hintergrund und Kontext",
    "Zielpopulation, Zweck, Anwender*innen",
    "Gesundheitliche Ungleichheiten","Zielsetzung",
    "Datenquelle, Begründung, Repräsentativität","Datenzeitraum und Nachbeobachtung",
    "Versorgungsrahmen, Zentren","Ein- und Ausschlusskriterien","Behandlungen",
    "Datenaufbereitung und Qualitätsprüfung",
    "Definition des Endpunkts, Zeithorizont","Beurteilende des Endpunkts (subjektiv)",
    "Verblindung der Endpunktbeurteilung",
    "Auswahl der Prädiktoren","Definition und Messung der Prädiktoren",
    "Beurteilende der Prädiktoren (subjektiv)",
    "Stichprobengröße","Fehlende Werte",
    "Datennutzung und Aufteilung","Umgang mit Prädiktoren",
    "Modelltyp, Modellbildung, Hyperparameter, interne Validierung",
    "Heterogenität zwischen Zentren",
    "Gütemaße und Grafiken","Modellanpassung","Berechnung der Vorhersagen",
    "Klassenungleichgewicht","Fairness","Modellausgabe",
    "Entwicklungs- und Evaluationsdaten","Ethikvotum",
    "Förderung","Interessenkonflikte","Studienprotokoll","Registrierung",
    "Datenverfügbarkeit","Verfügbarkeit des Codes",
    "Beteiligung von Patient*innen und Öffentlichkeit",
    "Fluss der Teilnehmenden","Merkmale der Teilnehmenden",
    "Vergleich mit den Entwicklungsdaten",
    "Teilnehmende und Ereignisse je Analyse","Modellspezifikation",
    "Modellgüte mit KI, auch in Untergruppen","Heterogenität zwischen Zentren",
    "Ergebnisse der Modellanpassung",
    "Interpretation","Limitationen",
    "Mangelhafte oder fehlende Eingabedaten",
    "Bedienung durch Anwender*innen, nötige Expertise","Künftige Forschung"),
  Status = c(
    "Erfüllt","Erfüllt","Erfüllt","Teilweise erfüllt","Teilweise erfüllt","Erfüllt",
    "Erfüllt","Teilweise erfüllt","Erfüllt","Erfüllt","Teilweise erfüllt","Teilweise erfüllt",
    "Erfüllt","Teilweise erfüllt","Erfüllt","Erfüllt","Erfüllt","Erfüllt","Teilweise erfüllt","Erfüllt",
    "Erfüllt","Erfüllt","Erfüllt","n. z.","Erfüllt","n. z.","n. z.",
    "n. z.","Teilweise erfüllt","Erfüllt","n. z.","Teilweise erfüllt",
    "Erfüllt","Erfüllt","Erfüllt","Erfüllt","Erfüllt","Erfüllt","Erfüllt",
    "Erfüllt","Erfüllt","n. z.","Erfüllt","Teilweise erfüllt","Teilweise erfüllt","n. z.","n. z.",
    "Erfüllt","Erfüllt","Teilweise erfüllt","Teilweise erfüllt","Erfüllt"),
  Beleg = c(
    "Titelblatt","Kap. @@chap:summ@@","Kap. @@sec:intro:hcc@@, @@sec:intro:chirurgie@@",
    "Kap. @@sec:intro:hcc@@, @@sec:intro:chirurgie@@ (Anwender*innen nur implizit)",
    "Kap. @@sec:disc:limit:kohorte@@ (Geschlechterverhältnis)",
    "Kap. @@sec:intro:ziele@@ (F1--F3)",
    "Kap. @@sec:data:design@@",
    "Kap. @@sec:data:design@@ (Einschluss 2001--2018); Ende der Nachbeobachtung nicht genannt",
    "monozentrisch, UKHD (Kap. @@sec:data:design@@)","Tab. @@tab:method:exclusion@@",
    "Resektionsausmaß (Kap. @@sec:data:freitext:op@@); Rezidivtherapie (Tab. @@tab:results_desc:therapy@@)",
    "Kap. @@sec:data:acquisition@@, @@sec:data:freitext@@, @@sec:methods:ml:fe@@; Untergruppen nicht geprüft",
    "Kap. @@sec:data:endpoints@@ (TTR)","Rezidiv aus Routine- und Bildbefunden",
    "keine Maßnahmen zur Verblindung, Rezidiv aus der klinischen Routine",
    "Kap. @@sec:methods:f1:axes@@, @@sec:methods:cox:mv@@, @@sec:methods:ml:fe@@",
    "Tab. @@tab:method:vars@@; Kap. @@sec:data:struktur@@, @@sec:data:freitext@@",
    "Kap. @@sec:data:volumetrie@@ (Volumetrie durch eine Person)",
    "Tab. @@tab:results_cox:epv@@; Kap. @@sec:disc:methods:epv@@ (Ereignisse je Variable, Teststärke); keine Fallzahlplanung vorab",
    "Tab. @@tab:results_desc:missing@@; Kap. @@sec:data:struktur@@, @@sec:methods:ml:split@@",
    "Kap. @@sec:methods:ml:split@@ (verschachtelte Kreuzvalidierung)",
    "Kap. @@sec:methods:ml:fe@@, @@sec:methods:ml:models@@ (Standardisierung)",
    "Kap. @@sec:methods:cox:mv@@, @@sec:methods:ml:models@@, @@sec:methods:ml:hpt@@; Tab. @@tab:appendix:hpt_grids@@ (Anhang @@sec:app:a4@@)",
    "monozentrisch","Kap. @@sec:methods:ml:eval@@","keine externe Evaluation",
    "keine externe Evaluation",
    "keine Verfahren gegen Klassenungleichgewicht; Aufteilung nach Rezidiv stratifiziert (Kap. @@sec:methods:ml:split@@)",
    "Kap. @@sec:disc:limit:kohorte@@ (diskutiert, nicht quantifiziert)",
    "Risikoscores; Schwellen der Entscheidungskurvenanalyse und Tertile (Kap. @@sec:methods:ml:eval@@)",
    "keine externe Evaluationskohorte",
    "Kap. @@sec:data:design@@ (S-429/2021); Einwilligung nicht beschrieben",
    "keine externe Förderung","keine Interessenkonflikte","kein Studienprotokoll erstellt","nicht registriert",
    "Patient*innendaten aus Datenschutzgründen nur eingeschränkt verfügbar",
    "github.com/nzntl/Promotion_HCC_Rezidiv; Kap. @@sec:methods:ml:repro@@; Tab. @@tab:app:software@@",
    "keine Patient*innenbeteiligung",
    "Tab. @@tab:method:exclusion@@","Tab. @@tab:results_desc:char@@","keine externe Evaluation",
    "Kap. @@sec:results_f1@@; Tab. @@tab:results_cox:epv@@",
    "Tab. @@tab:results_cox:mv@@, @@tab:results_cox:postop@@ (Koeffizienten); RSF und GBSA über den Code",
    "Bootstrap-KI (Tab. @@tab:results_ml:s2_perf@@, @@tab:results_ml:s3_landmark@@); keine Untergruppen",
    "monozentrisch","keine Modellanpassung",
    "Kap. @@sec:disc:main@@, @@sec:disc:f1@@","Kap. @@sec:disc:limit@@",
    "Kap. @@sec:disc:limit:merkmale@@ (Ersatzgrößen für AFP und MVI)",
    "Kap. @@sec:disc:f3@@, @@sec:disc:outlook@@","Kap. @@sec:disc:outlook@@ (Ausblick)"),
  check.names = FALSE, stringsAsFactors = FALSE
)

n_done    <- sum(tripod$Status == "Erfüllt")
n_partial <- sum(tripod$Status == "Teilweise erfüllt")
n_na      <- sum(tripod$Status == "n. z.")
n_total   <- nrow(tripod)
n_appl    <- n_total - n_na

rows <- vapply(seq_len(nrow(tripod)), function(i) paste(
  c(tex_escape_cell(tripod$Item[i]), tex_escape_cell(tripod$Bereich[i]),
    tex_escape_cell(tripod$Status[i]), ref_cell(tripod$Beleg[i])),
  collapse = " & "), character(1))
rows <- paste0(rows, " \\\\")
lt <- c(
  sprintf("%% TRIPOD+AI-Konformität (Collins et al. 2024): 27 Hauptitems / %d Sub-Items. %d Erfüllt, %d Teilweise erfüllt, %d n.z.",
          n_total, n_done, n_partial, n_na),
  "\\begin{longtable}{l>{\\raggedright\\arraybackslash}p{4cm}l>{\\raggedright\\arraybackslash}p{6cm}}",
  "\\caption[TRIPOD+AI-Konformität]{Status pro TRIPOD+AI-Item nach \\citet{collins_tripod+ai_2024}.}\\label{tab:app:tripod}\\\\",
  "\\toprule", "Item & Bereich & Status & Beleg \\\\", "\\midrule", "\\endfirsthead",
  "\\multicolumn{4}{l}{\\footnotesize\\itshape Fortsetzung TRIPOD+AI-Konformität} \\\\",
  "\\toprule", "Item & Bereich & Status & Beleg \\\\", "\\midrule", "\\endhead",
  "\\midrule",
  "\\multicolumn{4}{r}{\\footnotesize\\itshape Fortsetzung nächste Seite} \\\\",
  "\\endfoot", "\\bottomrule", "\\endlastfoot",
  rows,
  "\\end{longtable}")
con <- file("./03_Tables_Figures/Tab_TRIPOD_AI_Conformity.tex", "w", encoding = "UTF-8")
writeLines(lt, con); close(con)

message(sprintf(
  paste0("Stage 9 done. TRIPOD+AI (27 Hauptitems / 52 Sub-Items): ",
         "%d Erfüllt, %d Teilweise erfüllt, %d n.z.; ",
         "%d anwendbar -> %.0f%% Erfüllt, %.0f%% Teilweise erfüllt."),
  n_done, n_partial, n_na, n_appl,
  100 * n_done / n_appl, 100 * n_partial / n_appl))
