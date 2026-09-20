# 07_Variable_Legend.R -- Legende der Analysevariablen. Schreibt 01_Daten/variables_legend.xlsx
# und kein Objekt fuer das Manuskript.

pkgs <- c("readxl", "writexl", "dplyr", "tidyr", "stringr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
library(readxl); library(writexl); library(dplyr); library(tidyr); library(stringr)

L <- tribble(
  ~Variable, ~Label, ~Beschreibung, ~Typ, ~Phase,

  # ── Identitäten / Meta ──
  "Pseudonym",         "Pseudonym (ID)",            "Eindeutige Patienten-ID",                                              "Statisch",  "Meta",
  "Studienausschluss", "Studienausschluss",         "Grund für Studienausschluss (no preop img / no postop img / no OP / no HCC)", "Statisch",  "Meta",

  # ── Datum / Zeit ──
  "date_Geburt",       "Geburtsdatum",              "Geburtsdatum",                                                         "Statisch",  "Meta",
  "date_OP",           "OP-Datum",                  "Datum der Operation",                                                  "Statisch",  "präop",
  "date_obs",          "Beobachtungsdatum",         "Datum der Beobachtungs-Zeile (Long-Format)",                           "Dynamisch", "beide",
  "date_FU",           "Datum letztes FU",          "Datum des letzten Follow-up",                                          "Statisch",  "postop",
  "FU_duration_days",  "FU-Dauer (Tage)",           "Tage zwischen OP und letztem Follow-up",                               "Abgeleitet", "postop",
  "lab_date_POD",      "POD (Tag nach OP)",         "Postoperativer Tag der Beobachtungs-Zeile",                            "Dynamisch", "beide",
  "lab_date_POD_imp",  "POD (imputiert)",           "POD mit linearer Imputation pro Patient",                              "Dynamisch", "beide",
  "datasource",        "Datenquelle (Event-Typ)",   "konstant / volumetry / labor / rezidiv / Rezidiv_Therapie / FU",       "Dynamisch", "beide",

  # ── Demographie / Anthropometrie ──
  "Alter",             "Alter (Jahre)",             "Alter bei OP in Jahren",                                               "Statisch",  "präop",
  "m",                 "Körpergröße (m)",           "Körpergröße in m",                                                     "Statisch",  "präop",
  "mwd",               "Geschlecht (m/w)",          "Geschlecht (m/w)",                                                     "Statisch",  "präop",
  "KGkg",              "Körpergewicht (kg)",        "Körpergewicht in kg",                                                  "Statisch",  "präop",
  "BMI",               "BMI (kg/m²)",               "Body-Mass-Index",                                                      "Statisch",  "präop",

  # ── Diagnose / Tumor ──
  "Diagnose",          "Diagnose",                  "Klinische Hauptdiagnose (HCC)",                                        "Statisch",  "präop",
  "Äthiologie",        "Ätiologie HCC",             "Ätiologie des HCC (multifaktoriell, komma-getrennt)",                  "Statisch",  "präop",
  "äthiologie_cat",    "Ätiologie-Klasse (6)",      "Ätiologie 6-Klassen (HBV / HCV / Ethyltox / MASLD / Andere / Unbekannt)","Abgeleitet", "präop",
  "aetio_model",       "Ätiologie (4 Klassen)",     "Ätiologie (4 Klassen: MASLD, viral, ethyltoxisch, Rest). Aus äthiologie_cat abgeleitet, Referenzklasse Rest", "Abgeleitet", "präop",
  "masld_bin",         "MASLD-Ätiologie",      "Binäres Flag für Ätiologie-Klasse MASLD (aus äthiologie_cat). Verwendung nur im DM/Metformin-Submodell", "Abgeleitet", "präop",
  "Vorbehandlung",     "Vorbehandlung",             "Frühere Behandlung vor OP",                                            "Statisch",  "präop",
  "T",                 "TNM T-Stadium",             "TNM T-Stadium",                                                        "Statisch",  "präop",
  "N",                 "TNM N-Stadium",             "TNM N-Stadium",                                                        "Statisch",  "präop",
  "M",                 "TNM M-Stadium",             "TNM M-Stadium",                                                        "Statisch",  "präop",
  "Tumordurchmesser",  "Tumordurchmesser (cm)",     "Tumordurchmesser in cm",                                              "Statisch",  "präop",
  "Läsionenzahl_post", "Läsionenzahl (histo)",      "Anzahl der Läsionen laut histopathologischem Bericht (numerisch oder Textwert multipel/disseminiert)", "Statisch",  "postop",
  "lesion_multi_bin",  "Tumoranzahl: multipel (vs. solitär)", "Binär aus Läsionenzahl_post (Histopathologie). Multipel = mehr als eine Läsion oder Eintrag multipel/disseminiert", "Abgeleitet", "postop",
  "Rezidivtumor",      "Vor-Rezidiv vor OP",        "Vor-Rezidiv vor der analysierten OP (ja/nein)",                        "Statisch",  "präop",

  # ── Leberfunktion / Komplikation ──
  "Leberzirrhose",     "Leberzirrhose",             "Leberzirrhose ja/nein",                                                "Statisch",  "präop",
  "ChildPugh_Punkte",  "Child-Pugh Punkte",         "Child-Pugh-Score (Punkte)",                                            "Statisch",  "präop",
  "ChildPugh_Stadium", "Child-Pugh Stadium",        "Child-Pugh-Stadium (A/B/C)",                                           "Statisch",  "präop",
  "ClavienDindo",      "Clavien-Dindo (0-5)",       "Clavien-Dindo Komplikationsgrad",                                      "Statisch",  "postop",
  "FUStatus",          "FU-Status (NED/AWD/...)",   "Follow-up-Status (NED/AWD/DOD/DOC/COD unknown)",                       "Statisch",  "postop",

  # ── Nebendiagnosen / Komorbidität ──
  "ND_Nikotin",        "Nikotinkonsum (binär)",     "Nikotin-Konsum dokumentiert (binär)",                                  "Abgeleitet", "präop",
  "ND_Nikotin_py",     "Pack-Years",                "Pack-Years bei Nikotin-Konsum",                                        "Statisch",  "präop",
  "ND_count",          "Anzahl Nebendiagnosen",     "Anzahl dokumentierter Nebendiagnosen",                                 "Abgeleitet", "präop",
  "DM",                "Diabetes mellitus",         "Diabetes mellitus ja/nein",                                            "Statisch",  "präop",
  "dm_bin",            "Diabetes mellitus (binär)", "Diabetes mellitus als 0/1-Kodierung von DM (fehlende Werte bleiben NA)","Abgeleitet", "präop",
  "D_ND_cat_*",        "Komorbiditäts-Kategorien",  "Binäre Komorbiditäts-Flags je Organsystem (13 Kategorien: kardiovaskulär, metabolisch, endokrin, hepatisch, renal, pulmonal, gastrointestinal, neuropsychiatrisch, hämatologisch, muskuloskelettal, infektiös, chirurgisch, Substanzabusus). Davon 3 als ML-Features (kardiovaskulär, metabolisch, endokrin). Eine Diagnose zählt bei der Person nicht mit, bei der sie die Ätiologie benennt", "Abgeleitet", "präop",

  # ── Symptomatik ──
  "Symptom_cat",       "Symptom-Klasse",            "Symptomatisch / Asymptomatisch (Zufall) / Überwachung",                "Abgeleitet", "präop",

  # ── OP ──
  "OP_Name",           "OP-Bezeichnung",            "Klartext-OP-Bezeichnung",                                              "Statisch",  "präop",
  "OP_num",            "OP-Segmente (Couinaud)",    "Couinaud-Segment-Nummerierung der Resektion",                          "Statisch",  "präop",
  "OP_laparoskop",     "Zugangsweg",                "Zugangsweg der Resektion (offen vs. laparoskopisch)",                                          "Statisch",  "präop",
  "OP_CCE",            "Zeitgleiche CCE",           "Cholezystektomie im selben Eingriff (ja vs. nein)",                                          "Statisch",  "präop",
  "D_OP_seg_1",        "OP betraf Segment 1",       "Segment 1 betroffen (prognostisch relevant)",                          "Abgeleitet", "präop",
  "D_OP_seg_4",        "OP betraf Segment 4",       "Segment 4 betroffen",                                                  "Abgeleitet", "präop",
  "D_OP_seg_2_3",      "OP betraf Segmente 2 & 3",  "Segmente 2 und 3 betroffen",                                           "Abgeleitet", "präop",
  "n_op_segments",     "Resezierte Segmente",       "Anzahl verschiedener resezierter Couinaud-Segmente (aus OP_num; 4a/4b zählen als Segment 4)", "Abgeleitet", "präop",
  "D_major",           "Major-Hepatektomie",        "Major-Hepatektomie: mindestens drei resezierte Segmente",              "Abgeleitet", "präop",

  # ── Volumetrie ──
  "FLV",               "FLV präop (cm³)",           "Future Liver Volume (präop geplant)",                                  "Statisch",  "präop",
  "TLV",               "TLV (cm³, longitudinal)",   "Total Liver Volume (longitudinal)",                                    "Dynamisch", "beide",
  "TLV_preop",         "TLV präop (cm³)",           "TLV bei erster (präop) Volumetrie",                                    "Abgeleitet", "präop",
  "TLV_first_postop",  "TLV 1. postop (cm³)",       "TLV bei erster postoperativer Volumetrie",                             "Abgeleitet", "postop",
  "tumor_volume",      "Tumorvolumen (cm³)",        "Tumorvolumen",                                                         "Dynamisch", "beide",
  "HU_liver",          "HU Leberparenchym",         "Hounsfield-Wert Leberparenchym (erhoben, aber nicht in die Analyse-Datensätze übernommen)", "Dynamisch", "beide",
  "HU_spleen",         "HU Milzparenchym",          "Hounsfield-Wert Milzparenchym (erhoben, aber nicht in die Analyse-Datensätze übernommen)",  "Dynamisch", "beide",
  "d_TLV_abs",         "Δ TLV absolut",             "Absolute TLV-Differenz zur ersten Volumetrie",                         "Dynamisch", "postop",
  "d_TLV_rel",         "Δ TLV (%)",                 "Relative TLV-Differenz zur ersten Volumetrie",                         "Dynamisch", "postop",
  "d_TLV_rel_FLV_base","Δ TLV vs. FLV (%)","(TLV - FLV) / FLV × 100 — Regeneration gegenüber präop-Plan",          "Abgeleitet", "postop",
  "d_TLV_rel_first_postop", "Δ TLV vs. 1. Volumetrie (1. postop)", "d_TLV_rel zur 1. postop Volumetrie",                   "Abgeleitet", "postop",
  "d_TLV_rel_FLV_base_first_postop", "Δ TLV vs. FLV (1. postop)", "d_TLV_rel_FLV_base zur 1. postop Volumetrie",   "Abgeleitet", "postop",
  "POD_first_postop_volumetry", "POD 1. postop Volumetrie", "Postoperativer Tag der 1. postop Volumetrie",                  "Abgeleitet", "postop",
  "LVR_nam",           "LVR-Index POD 91-180",      "TLV (1. Messung POD 91-180) / FLV präop — Plateaufenster nach Ku 2026", "Abgeleitet", "postop",
  "TLV_nam_91_180",    "TLV POD 91-180",            "TLV der 1. Volumetrie im Fenster POD 91-180",                          "Abgeleitet", "postop",
  "POD_nam_91_180",    "POD der Fenster-Volumetrie","Postoperativer Tag der 1. Volumetrie im Fenster POD 91-180",           "Abgeleitet", "postop",
  "TLV_growth_rate",   "TLV-Log-Wachstumsrate",     "OLS-Steigung von log(TLV_parenchym) über POD (postop ≤ 180, vor Ereignis, ≥ 2 Messungen)", "Abgeleitet", "postop",
  "TLV_parenchym",     "Parenchymvolumen (cm³)",    "TLV − tumor_volume: tumorfreies Lebervolumen der Volumetrie-Zeile",    "Abgeleitet", "postop",
  "n_postop_volumetrien", "Anzahl postop Volumetrien", "Anzahl postoperativer Volumetrien pro Patient",                    "Abgeleitet", "postop",
  "TTLVR_preop",       "TTLVR (Tumor/TLV)",         "Tumor-to-Total-Liver-Volume-Ratio präop (nach Lian)",                  "Abgeleitet", "präop",

  # ── Labor (longitudinal) ──
  "Bili_preop",        "Bilirubin präop",           "Bilirubin (nur präoperativ verfügbar; siehe Limitation)",              "Dynamisch", "präop",
  "nü_Gluc",           "Glucose",          "Glucose",                                                     "Dynamisch", "beide",
  "GOT",               "GOT/AST",                   "Glutamat-Oxalacetat-Transaminase (AST)",                               "Dynamisch", "beide",
  "GPT",               "GPT/ALT",                   "Glutamat-Pyruvat-Transaminase (ALT)",                                  "Dynamisch", "beide",
  "Albumin",           "Serum-Albumin",             "Serum-Albumin",                                                        "Dynamisch", "beide",
  "CRP",               "CRP",                       "C-reaktives Protein",                                                  "Dynamisch", "beide",
  "Leukos",            "Leukozyten",                "Leukozyten",                                                           "Dynamisch", "beide",
  "Thrombos",          "Thrombozyten",              "Thrombozyten",                                                         "Dynamisch", "beide",
  "Quick",             "Quick-Wert",                "Quick-Wert (Prothrombinzeit in %)",                                    "Dynamisch", "beide",
  "Magnesium",         "Serum-Magnesium",           "Serum-Magnesium",                                                      "Dynamisch", "beide",

  # ── Labor ──
  "AST_preop",         "AST präop",                 "AST/GOT präop (erster Laborwert)",                                     "Abgeleitet", "präop",
  "Albumin_preop",     "Albumin präop",             "Albumin präop (erster Laborwert)",                                     "Abgeleitet", "präop",
  "Thrombos_preop",    "Thrombozyten präop",        "Thrombozyten präop (erster Laborwert)",                                "Abgeleitet", "präop",
  "Leukos_preop",      "Leukozyten präop",          "Leukozyten präop (erster Laborwert)",                                  "Abgeleitet", "präop",
  "CRP_preop",         "CRP präop",                 "CRP präop (erster Laborwert)",                                         "Abgeleitet", "präop",
  "Quick_preop",       "Quick präop",               "Quick-Wert präop (erster Laborwert)",                                  "Abgeleitet", "präop",
  "Bili_preop_val",    "Bilirubin präop (num.)",    "Bilirubin präop numerisch (erster Laborwert)",                         "Abgeleitet", "präop",
  "deritis_preop",     "De-Ritis-Quotient",         "AST/ALT-Ratio präop; Marker für Hepatozytenschaden und Fibrose",       "Abgeleitet", "präop",
  "astalb_preop",      "AST/Albumin-Ratio",         "AST/Albumin präop (nach Peng 2022); mischt Zellschaden und Reserve",   "Abgeleitet", "präop",
  "APRI_preop",        "APRI",                      "AST-to-Platelet-Ratio-Index präop ((AST/ULN)/Thrombos × 100)",         "Abgeleitet", "präop",
  "ALBI_score",        "ALBI-Score",                "ALBI-Score präop (0,66 x log10 Bili[umol] - 0,085 x Albumin)",         "Abgeleitet", "präop",
  "ALBI_grade",        "ALBI-Grad",                "ALBI-Grad (1/2/3) aus ALBI-Score",                                    "Abgeleitet", "präop",

  # ── Funktionelle Regeneration ──
  "slope_Quick_POD1_7",   "Quick-Slope POD 1-7",    "Quick-Wert-Steigung POD 1-7 (%/Tag)",                                  "Abgeleitet", "postop",
  "slope_GOT_POD1_7",     "GOT-Slope POD 1-7",      "GOT-Steigung POD 1-7 (U/L/Tag)",                                       "Abgeleitet", "postop",
  "slope_GPT_POD1_7",     "GPT-Slope POD 1-7",      "GPT-Steigung POD 1-7 (U/L/Tag)",                                       "Abgeleitet", "postop",
  "slope_Albumin_POD1_7", "Albumin-Slope POD 1-7",  "Albumin-Steigung POD 1-7 (g/L/Tag)",                                   "Abgeleitet", "postop",
  "slope_CRP_POD1_7",     "CRP-Slope POD 1-7",      "CRP-Steigung POD 1-7 (mg/L/Tag)",                                      "Abgeleitet", "postop",
  "slope_Leukos_POD1_7",  "Leukozyten-Slope POD 1-7",   "Leukozyten-Steigung POD 1-7 (/nL/Tag)",                            "Abgeleitet", "postop",
  "slope_Thrombos_POD1_7","Thrombozyten-Slope POD 1-7", "Thrombozyten-Steigung POD 1-7 (/nL/Tag)",                          "Abgeleitet", "postop",

  # ── Rezidiv ──
  "Rezidiv",           "Rezidiv-Flag (Zeile)",      "Rezidiv-Flag auf der Folge-Zeile (Ja/Nein)",                           "Dynamisch", "postop",
  "D_rezidiv_dyn",     "Rezidiv ab Ereignis (dyn.)","1 ab erstem Rezidiv-Ereignis dieser Zeile aufwärts",                   "Dynamisch", "postop",
  "D_rezidiv",         "Rezidiv-Flag (patient)",    "Patient-Level: irgendwann Rezidiv (1) oder nicht (0)",                 "Abgeleitet", "postop",
  "n_rezidiv",         "Anzahl Rezidive",           "Anzahl Rezidive aus datasource == rezidiv",                            "Abgeleitet", "postop",
  "date_rezidiv_first","Datum 1. Rezidiv",          "Datum des ersten Rezidivs",                                            "Abgeleitet", "postop",
  "time_to_first_rezidiv_days", "Tage OP bis 1. Rezidiv", "Tage von OP bis erstem Rezidiv",                                   "Abgeleitet", "postop",
  "D_rezidiv_cat",     "Rezidiv-Muster (Ereignis)", "Rezidiv-Muster pro Ereignis (intra-/extrahepatisch, multilokulär)",    "Abgeleitet", "postop",
  "D_rezidiv_cat_first","Erstes Rezidiv: Lokalisation","Rezidiv-Muster des ersten Rezidivs (patient-level)",                "Abgeleitet", "postop",
  "rezidiv_ort_cat",   "Rezidiv-Ort (Kategorie)",   "Per-Event-Kategorie: Intrahepatisch / Extrahepatisch",                 "Abgeleitet", "postop",
  "rezidiv_ort_cat_first", "Erstes Rezidiv: Ort",   "rezidiv_ort_cat des ersten Rezidivs (patient-level)",                  "Abgeleitet", "postop",
  "rezidiv_therapie_cat", "Rezidiv-Therapie (Kategorie)", "Therapie-Kategorie (Resektion / Ablation / Transarteriell / ...)","Abgeleitet", "postop",
  "rezidiv_therapie_cat_first", "Erstes Rezidiv: Therapie", "Erste dokumentierte Therapie-Kategorie",                       "Abgeleitet", "postop",
  "n_rezidiv_therapie", "Anzahl Therapie-Ereignisse","Anzahl Therapie-Ereignisse pro Patient",                              "Abgeleitet", "postop"
)

ok <- tryCatch({ write_xlsx(L, "./01_Daten/variables_legend.xlsx"); TRUE },
               error = function(e) FALSE)
if (ok) {
  message("Stage 7 done. variables_legend.xlsx written (", nrow(L), " rows).")
} else {
  message("Stage 7 skipped: variables_legend.xlsx locked (likely open in Excel). Continuing.")
}

# ── LaTeX appendix table fragment ──
fig_dir <- "./03_Tables_Figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

tex_escape_cell <- function(x) {
  x <- as.character(x); x[is.na(x)] <- "--"
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  gsub("([&%$#_{}])", "\\\\\\1", x)
}

L_tex <- L |>
  select(Label, Beschreibung, Typ, Phase) |>
  mutate(Phase = if_else(Phase == "beide", "prä & post", Phase))

hdr <- paste(tex_escape_cell(names(L_tex)), collapse = " & ")
body <- apply(L_tex, 1, function(r) paste(tex_escape_cell(r), collapse = " & "))
body <- paste0(body, " \\\\")
caption <- paste0(
  "% Variablenlexikon der Analyse-Kohorte: Label (im Dokument verwendete ",
  "Bezeichnung), Beschreibung, Typ (Statisch / Dynamisch / Abgeleitet), Phase ",
  sprintf("(praeop / postop / prae & post / Meta). N=%d Einträge, ", nrow(L_tex)),
  "Pipeline-Schritt 02_Code/07_Variable_Legend.R."
)

align <- "p{0.24\\linewidth} p{0.43\\linewidth} l l"
out <- c(
  caption,
  sprintf("\\begin{longtable}{%s}", align),
  paste0("\\caption{Variablenlexikon der Analyse-Kohorte (n = ", nrow(L_tex),
         " Einträge).}\\label{tab:app:varlexicon}\\\\"),
  "\\toprule",
  paste0(hdr, " \\\\"),
  "\\midrule",
  "\\endfirsthead",
  paste0("\\multicolumn{4}{l}{\\footnotesize\\itshape Fortsetzung Variablenlexikon} \\\\"),
  paste0("\\toprule\n", hdr, " \\\\\n\\midrule\n\\endhead"),
  "\\midrule",
  paste0("\\multicolumn{4}{r}{\\footnotesize\\itshape Fortsetzung nächste Seite} \\\\"),
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot",
  body,
  "\\end{longtable}"
)
if (FALSE) {
path <- file.path(fig_dir, "Appendix_Variable_Lexicon.tex")
con <- file(path, "w", encoding = "UTF-8")
writeLines(out, con); close(con)
message(sprintf("Stage 7 (J1) wrote %s (%d Eintraege)", path, nrow(L_tex)))
}
for (old in c("Appendix_Variable_Lexicon.tex",
              "Appendix_Variable_Lexicon_Landscape.tex",
              "Tab_Variable_Lexicon_Appendix_Landscape.tex")) {
  op <- file.path(fig_dir, old)
  if (file.exists(op)) file.remove(op)
}
