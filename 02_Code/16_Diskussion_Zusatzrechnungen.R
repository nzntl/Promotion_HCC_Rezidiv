# 16_Diskussion_Zusatzrechnungen.R -- Zusatzrechnungen zu Abschnitt 4.3.3 (Selektionskaskade, Messgroesse).
# Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx, data_dynamic_surv.xlsx und
# 01_Daten/20260817_bildgebungstermine_alle.xlsx (Blatt "Termine_lang").
# Schreibt 03_Tables_Figures/Zusatzrechnungen_4_3_3.txt und dieselben Zeilen auf die Konsole.
# Die hier berechneten Zahlen stehen im Text, aber in keiner Tabelle; die Datei macht sie nachlesbar.
# Voraussetzung: 06b_Cox_Chan_Postop.R lief in derselben Sitzung (Objekte fitD und sdat_D).
suppressPackageStartupMessages({ library(readxl); library(dplyr); library(survival) })

out_path <- file.path("03_Tables_Figures", "Zusatzrechnungen_4_3_3.txt")
if (!dir.exists(dirname(out_path))) dir.create(dirname(out_path), recursive = TRUE)
con_out <- file(out_path, open = "w", encoding = "UTF-8")
writeLines(c(
  "Zusatzrechnungen zu den Abschnitten 4.3.2 und 4.3.3 der Dissertation",
  "Quelle: 02_Code/16_Diskussion_Zusatzrechnungen.R",
  paste("Lauf:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "Jede Zeile entspricht einer Zahl, die im Text genannt wird und in keiner Tabelle steht.",
  ""), con_out)

td <- function(...) {
  zeile <- sprintf("[textdiag-4.3.3] %s", sprintf(...))
  cat(zeile, "\n", sep = "")
  writeLines(zeile, con_out)
}

stat <- read_xlsx("01_Daten/data_static.xlsx") |>
  filter(is.na(Rezidivtumor) | tolower(as.character(Rezidivtumor)) != "ja")
surv <- read_xlsx("01_Daten/data_dynamic_surv.xlsx")
dyn  <- read_xlsx("01_Daten/data_dynamic.xlsx")
an   <- inner_join(stat, surv, by = "Pseudonym")
pod1 <- an$POD_first_postop_volumetry
rec  <- an$event == 1

# 1) Erste postoperative Volumetrie relativ zum Tag der Rezidivdiagnose
td("Rezidive mit erster postop. Volumetrie am Diagnosetag: %d von %d; am oder nach dem Diagnosetag: %d",
   sum(rec & pod1 == an$time, na.rm = TRUE), sum(rec), sum(rec & pod1 >= an$time, na.rm = TRUE))

# 2) Zeitpunkt der ersten Volumetrie nach Rezidivstatus, mit und ohne fruehe Todesfaelle
wt <- function(d, lab) {
  a <- d$POD_first_postop_volumetry[d$event == 1]; b <- d$POD_first_postop_volumetry[d$event == 0]
  td("%s: Median POD %.0f (n=%d) vs. %.0f (n=%d), Differenz %.0f Tage, Wilcoxon p=%.3f", lab,
     median(a, na.rm = TRUE), sum(!is.na(a)), median(b, na.rm = TRUE), sum(!is.na(b)),
     median(a, na.rm = TRUE) - median(b, na.rm = TRUE), wilcox.test(a, b, exact = FALSE)$p.value)
}
wt(an, "Erste Volumetrie, alle")
wt(filter(an, death90 != 1), "Erste Volumetrie, ohne 90-Tage-Todesfaelle")
wt(filter(an, death90 != 1, !(event == 1 & POD_first_postop_volumetry >= time)),
   "Erste Volumetrie, ohne Todesfaelle und ohne Volumetrie am/nach Diagnosetag")

# 3) Komplikationen und Zeitpunkt der ersten Volumetrie
cd <- suppressWarnings(as.numeric(an$ClavienDindo))
td("Clavien-Dindo >= III: Median POD %.0f (n=%d); < III: Median POD %.0f (n=%d); Grad fehlt: n=%d",
   median(pod1[which(cd >= 3)], na.rm = TRUE), sum(cd >= 3, na.rm = TRUE),
   median(pod1[which(cd < 3)], na.rm = TRUE), sum(cd < 3, na.rm = TRUE), sum(is.na(cd)))
td("90-Tage-Todesfaelle mit erster Volumetrie bis POD 14: %d von %d",
   sum(an$death90 == 1 & pod1 <= 14, na.rm = TRUE), sum(an$death90 == 1))

# 4) Volumen am Landmark verfuegbar, nach spaeterem Rezidivstatus (Test ausdruecklich benannt)
vol <- dyn |> filter(!is.na(TLV), lab_date_POD_imp > 0, Pseudonym %in% an$Pseudonym)
for (lm in c(90, 180)) {
  ar  <- filter(an, time > lm)
  has <- ar$Pseudonym %in% vol$Pseudonym[vol$lab_date_POD_imp <= lm]
  tab <- table(factor(ar$event, 0:1), factor(has, c(FALSE, TRUE)))
  td(paste0("Landmark POD %d: Volumen vorhanden bei %d/%d (%.1f %%) mit vs. %d/%d (%.1f %%) ohne spaeteres Rezidiv; ",
            "Chi2 mit Yates-Korrektur p=%.3f, ohne Korrektur p=%.3f, Fisher exakt p=%.3f"), lm,
     tab["1", "TRUE"], sum(tab["1", ]), 100 * tab["1", "TRUE"] / sum(tab["1", ]),
     tab["0", "TRUE"], sum(tab["0", ]), 100 * tab["0", "TRUE"] / sum(tab["0", ]),
     chisq.test(tab)$p.value, chisq.test(tab, correct = FALSE)$p.value, fisher.test(tab)$p.value)
}

# 5) Anteil der postoperativen Volumetrien nach dem Rezidiv (Definition im Text angeben)
v <- inner_join(vol, select(an, Pseudonym, event, time), by = "Pseudonym")
td("Postop. Volumetrien n=%d: nach dem Diagnosetag %.1f %%, am oder nach dem Diagnosetag %.1f %%, Verlaufs-Flag D_rezidiv_dyn %.1f %%",
   nrow(v), 100 * mean(v$event == 1 & v$lab_date_POD_imp > v$time),
   100 * mean(v$event == 1 & v$lab_date_POD_imp >= v$time),
   100 * mean(suppressWarnings(as.numeric(v$D_rezidiv_dyn)) == 1, na.rm = TRUE))

# 6) Modell D ohne Faelle mit erster Volumetrie am oder nach dem Diagnosetag
if (exists("fitD") && exists("sdat_D")) {
  cc   <- sdat_D[complete.cases(sdat_D[, intersect(all.vars(formula(fitD)), names(sdat_D))]), ]
  raus <- cc$event == 1 & cc$POD_first_postop_volumetry >= cc$time
  fit2 <- coxph(formula(fitD), data = cc[!raus, ])
  ci   <- summary(fit2)$conf.int; pv <- summary(fit2)$coefficients[, "Pr(>|z|)"]
  td("Modell D ohne %d Faelle: n=%d, Rezidive=%d, Ereignisse je Parameter=%.2f",
     sum(raus), fit2$n, fit2$nevent, fit2$nevent / length(coef(fit2)))
  for (k in c("LVR_index_sd", "albumin_slope_sd", "FLV_TLV_ratio_sd"))
    td("  %s: HR %.2f (%.2f-%.2f), p=%.3f", k, ci[k, 1], ci[k, 3], ci[k, 4], pv[k])
} else td("Modell D nicht im Speicher -- zuerst 06_Cox_Analysis.R und 06b_Cox_Chan_Postop.R ausfuehren")

# 7) Bildgebungsmodalitaet laut Terminliste
tf <- "01_Daten/20260817_bildgebungstermine_alle.xlsx"
if (file.exists(tf)) {
  tm <- read_xlsx(tf, sheet = "Termine_lang") |> filter(Pseudonym %in% an$Pseudonym)
  anteil <- function(d, lab) { m <- toupper(as.character(d$Modalitaet)); ct <- sum(grepl("CT", m)); mr <- sum(grepl("MR", m))
    td("%s (n=%d): CT %.1f %%, MRT %.1f %%", lab, nrow(d), 100 * ct / (ct + mr), 100 * mr / (ct + mr)) }
  anteil(filter(tm, tolower(Messwerte_vorhanden) == "ja"), "Modalitaet, Termine mit Messwerten")
  anteil(tm |> filter(Tage_nach_OP <= 0) |> arrange(Pseudonym, Tage_nach_OP) |> group_by(Pseudonym) |> slice_tail(n = 1) |> ungroup(),
         "Modalitaet, praeoperative Basismessung je Person")
}

# 8) Teilgruppe mit LVR im Nam-Fenster POD 91-180 (Zahlen aus 4.3.2)
lvr_nam  <- suppressWarnings(as.numeric(an$LVR_nam))
lvr_snap <- suppressWarnings(as.numeric(an$TLV_first_postop)) / suppressWarnings(as.numeric(an$FLV))
inw <- !is.na(lvr_nam)
td("Gesamtkohorte: n=%d, Rezidive=%d, Ereignisrate %.1f %%", nrow(an), sum(rec), 100 * mean(rec))
td("Teilgruppe mit LVR im Fenster POD 91-180: n=%d, Rezidive=%d, Ereignisrate %.1f %%",
   sum(inw), sum(inw & rec), 100 * mean(rec[inw]))
td("Davon dieselbe Messung wie die Momentaufnahme (erste Volumetrie faellt selbst ins Fenster): %d",
   sum(inw & pod1 >= 91 & pod1 <= 180, na.rm = TRUE))
td("Spearman-Korrelation der beiden LVR-Fassungen: r=%.2f (n=%d)",
   cor(lvr_nam, lvr_snap, method = "spearman", use = "complete.obs"),
   sum(!is.na(lvr_nam) & !is.na(lvr_snap)))

close(con_out)
cat(sprintf("[textdiag-4.3.3] Ausgabe geschrieben nach %s\n", out_path))
