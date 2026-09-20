# 15_Variable_Overview.R -- Uebersicht der erhobenen Variablen und ihrer Verwendung.
# Schreibt Tab_2_2_Variablen_Uebersicht.tex.

suppressPackageStartupMessages({library(dplyr); library(tibble)})

esc <- function(s) {
  s <- gsub("([&%#_])", "\\\\\\1", s)
  s
}

ND <- "Präoperativ: Komorbidität und Ätiologie"
VOL <- "Postoperativ: Bildgebung und Volumetrie"
NAC <- "Postoperativ: Nachsorge und Endpunkt"
LABV <- "Präoperativ: Leberfunktion und Labor (POD $-$1)"
LABN <- "Postoperativ: Labor (POD 1 bis 7)"

v <- tribble(
  ~block, ~variable, ~bezug, ~asso, ~vorh,

  "Präoperativ: Person und Anamnese",
  "Geburtsdatum", "Alter bei Operation", "A", "A",
  "Präoperativ: Person und Anamnese",
  "Geschlecht", "--", "A", "A",
  "Präoperativ: Person und Anamnese",
  "Körpergröße, Körpergewicht", "BMI", "--", "A",
  "Präoperativ: Person und Anamnese",
  "Symptome bei Erstdiagnose", "Symptom-Klasse (Zufallsbefund, Überwachung, symptomatisch)", "--", "A",
  "Präoperativ: Person und Anamnese",
  "B-Symptomatik", "--", "--", "--",
  "Präoperativ: Person und Anamnese",
  "Vorbehandlung", "--", "--", "--",
  "Präoperativ: Person und Anamnese",
  "Voroperationen", "--", "--", "--",
  "Präoperativ: Person und Anamnese",
  "Onkologische Vorerkrankungen", "--", "--", "--",
  "Präoperativ: Person und Anamnese",
  "Antidiabetische Medikation", "--", "--", "--",
  "Präoperativ: Person und Anamnese",
  "Datum einer Lebertransplantation", "--", "--", "--",

  ND, "Diabetes mellitus", "--", "A", "A",
  ND, "Nikotinkonsum", "--", "A", "A",
  ND, "Pack-Years", "--", "--", "--",
  ND, "Ätiologie", "Ätiologie-Klassen (MASLD, viral, ethyltoxisch, Rest)", "A", "A",
  ND, "kardiovaskulär", "arterielle Hypertonie, KHK, Vorhofflimmern, pAVK", "--", "A",
  ND, "metabolisch", "NASH, Hyperlipidämie, Adipositas, Hyperurikämie", "--", "A",
  ND, "endokrin", "Hypothyreose, Hyperthyreose, Struma", "--", "A",
  ND, "Substanzkonsum", "Alkohol, Drogen", "--", "A",
  ND, "hepatisch", "Ösophagusvarizen, portale Hypertension, Splenomegalie", "--", "--",
  ND, "pulmonal", "COPD, Asthma, Schlafapnoe", "--", "--",
  ND, "infektiologisch", "Hepatitis A, B, C und E, HIV, Tuberkulose", "--", "--",
  ND, "renal", "chronische Niereninsuffizienz, Nephrolithiasis", "--", "--",
  ND, "gastrointestinal", "Gastritis, Refluxkrankheit, Divertikulose", "--", "--",
  ND, "neuropsychiatrisch", "Apoplex, Epilepsie, Depression", "--", "--",
  ND, "hämatologisch", "Thrombopenie, Anämie, Thrombophilie", "--", "--",
  ND, "muskuloskelettal", "Arthrose, Osteoporose, Frakturen", "--", "--",
  ND, "chirurgisch", "Cholezystektomie, Kolonresektion, Splenektomie", "--", "--",
  ND, "Zahl der Nebendiagnosen", "Krankheitslast", "--", "A",

  LABV,
  "Leberzirrhose", "--", "A", "A",
  LABV,
  "Child-Pugh-Punkte", "--", "--", "A",
  LABV,
  "Child-Pugh-Stadium", "--", "--", "--",

  LABV, "Bilirubin", "ALBI-Score und -Grad", "(B)", "B",
  LABV, "Albumin", "ALBI-Score und -Grad, AST/Albumin-Ratio", "(B)", "B",
  LABV, "Quick-Wert", "Quick präop (Einzelwert)", "B", "B",
  LABV, "AST (GOT)", "De-Ritis-Quotient, AST/Albumin-Ratio, APRI", "--", "(B)",
  LABV, "ALT (GPT)", "De-Ritis-Quotient", "--", "(B)",
  LABV, "Thrombozyten", "APRI", "--", "B",
  LABV, "Leukozyten", "--", "--", "B",
  LABV, "CRP", "--", "--", "B",
  LABV, "Glucose", "--", "--", "--",
  LABV, "Serum-Magnesium", "--", "--", "--",

  "Präoperativ: Volumetrie",
  "TLV", "FLV/TLV-Ratio, TTLVR, tumorfreies Parenchym", "(C)", "C",
  "Präoperativ: Volumetrie",
  "Tumorvolumen", "TTLVR, tumorfreies Parenchym", "(C)", "(C)",
  "Präoperativ: Volumetrie",
  "FLV", "FLV/TLV-Ratio, LVR-Index, $\\Delta$ TLV", "(C)", "C",

  "Intraoperativ",
  "Resezierte Segmente", "Zahl der Segmente, Major-Hepatektomie", "--", "A",
  "Intraoperativ",
  "Operativer Zugang", "--", "--", "--",
  "Intraoperativ",
  "Zeitgleiche Cholezystektomie", "--", "--", "--",
  "Intraoperativ",
  "Sonstige Begleiteingriffe", "--", "--", "--",

  "Postoperativ: histopathologischer Befund",
  "Tumordurchmesser", "--", "A", "A",
  "Postoperativ: histopathologischer Befund",
  "Läsionenzahl", "Tumoranzahl (multipel vs. solitär)", "A", "A",
  "Postoperativ: histopathologischer Befund",
  "T-, N-, M-Stadium", "--", "--", "A",

  LABN, "Albumin", "Albumin-Steigung", "D", "D",
  LABN, "Quick-Wert", "Quick-Steigung", "D", "D",
  LABN, "AST (GOT)", "GOT-Steigung", "D", "D",
  LABN, "ALT (GPT)", "GPT-Steigung", "D", "D",
  LABN, "Leukozyten", "Leukozyten-Steigung", "--", "D",
  LABN, "CRP", "CRP-Steigung", "D", "D",
  LABN, "Thrombozyten", "Thrombozyten-Steigung", "D", "D",

  VOL, "TLV", "LVR-Index, $\\Delta$ TLV gegen FLV, Wachstumsrate", "(D)", "E",
  VOL, "Tumorvolumen", "Rezidivbeleg und Rezidivdatum, tumorfreies Parenchym", "Endpunkt", "Endpunkt",
  VOL, "Bildgebungstermine", "Nachsorgedichte, Intervall-Simulation", "--", "--",
  VOL, "Modalität (CT oder MRT)", "--", "--", "--",
  VOL, "Dichtewerte Leber und Milz", "--", "--", "--",

  NAC, "Rezidivdatum und -lokalisation", "Zielgröße TTR, Rezidivort", "Endpunkt", "Endpunkt",
  NAC, "Follow-up-Status und -Datum", "Zielgröße OS, Zensierung", "Endpunkt", "Endpunkt",
  NAC, "Clavien-Dindo-Grad", "--", "--", "--",
  NAC, "Rezidivtherapie", "Therapiekategorie", "--", "--"
)

kopf <- "Variable & Bezug zu & Assoziation & Vorhersage \\\\"

out <- c(
  "% Uebersicht der erhobenen Variablen. Quelle: 02_Code/15_Variable_Overview.R",
  # ── Tabelle bleibt im Satzspiegel ──
  "\\setlength{\\LTleft}{0pt}",
  "\\setlength{\\LTright}{0pt}",
  "\\renewcommand{\\arraystretch}{1.08}",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\setlength{\\LTpre}{20pt}",
  "\\setlength{\\LTpost}{0pt}",
  paste0("\\begin{longtable}{@{}",
         ">{\\raggedright\\arraybackslash}p{4.3cm}",
         ">{\\raggedright\\arraybackslash}p{6.85cm}",
         ">{\\raggedright\\arraybackslash}p{1.65cm}",
         ">{\\raggedright\\arraybackslash}p{1.55cm}@{}}"),
  paste0("\\caption[Übersicht der erhobenen Variablen]{Übersicht der erhobenen ",
         "Variablen.}\\label{tab:method:vars}\\\\"),
  "\\toprule", kopf, "\\midrule", "\\endfirsthead",
  "\\toprule", kopf, "\\midrule", "\\endhead",
  "\\bottomrule", "\\insertTableNotes", "\\endlastfoot"
)

for (b in unique(v$block)) {
  out <- c(out, sprintf("\\multicolumn{4}{@{}l}{\\textit{%s}} \\\\[3pt]", esc(b)))
  s <- v |> filter(block == b)
  for (i in seq_len(nrow(s))) {
    out <- c(out, sprintf("%s & %s & %s & %s \\\\",
                          esc(s$variable[i]), esc(s$bezug[i]),
                          esc(s$asso[i]), esc(s$vorh[i])))
  }
  out <- c(out, "\\addlinespace")
}
out <- c(out, "\\end{longtable}")

dir.create("03_Tables_Figures", showWarnings = FALSE)
p <- "03_Tables_Figures/Tab_2_2_Variablen_Uebersicht.tex"
writeLines(out, p, useBytes = TRUE)
message("Wrote: ", p, " (", nrow(v), " Variablenzeilen)")
