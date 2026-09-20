# 13_Imaging_Density_Audit.R -- Dichte der Routinebildgebung zwischen Operation und Erstrezidiv.
# Liest 01_Daten/data_static.xlsx und 01_Daten/20260817_bildgebungstermine_alle.xlsx
# (Blatt "Termine_lang"); schreibt Tab_3_28b_Imaging_Density_Audit.tex.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(lubridate)
})

setwd_here <- function() {
  if (!dir.exists("01_Daten")) {
    stop("01_Daten not found - run from project root.")
  }
}
setwd_here()

# ── 1. Datenladen ──
stat <- read_xlsx("01_Daten/data_static.xlsx") |>
  filter(is.na(Rezidivtumor) | tolower(as.character(Rezidivtumor)) != "ja")

recur_dates <- stat |>
  filter(D_rezidiv == 1, !is.na(date_rezidiv_first)) |>
  select(Pseudonym, date_rezidiv_first)

termine <- read_xlsx("01_Daten/20260817_bildgebungstermine_alle.xlsx",
                     sheet = "Termine_lang")

# ── 2. Alle postoperativen Bildgebungstermine pro Rezidiv-Patient:in ──
vol_rec <- termine |>
  filter(!is.na(Untersuchungsdatum), !is.na(OP_Datum)) |>
  rename(date_obs = Untersuchungsdatum, date_OP = OP_Datum) |>
  inner_join(recur_dates, by = "Pseudonym") |>
  filter(date_obs > date_OP, date_obs <= date_rezidiv_first) |>
  mutate(POD_days = as.numeric(difftime(date_obs, date_OP, units = "days"))) |>
  arrange(Pseudonym, POD_days) |>
  select(Pseudonym, date_OP, date_obs, POD_days, date_rezidiv_first)

intervals <- vol_rec |>
  group_by(Pseudonym) |>
  mutate(prev_POD = lag(POD_days)) |>
  filter(!is.na(prev_POD)) |>
  mutate(gap_days  = POD_days - prev_POD,
         gap_weeks = gap_days / 7,
         mid_POD   = (POD_days + prev_POD) / 2) |>
  ungroup()

# ── 3. Phasenzuordnung nach Mittelpunkt des Intervalls ──
intervals <- intervals |>
  mutate(phase = case_when(
    mid_POD <= 365                     ~ "0-12 Mo",
    mid_POD >  365 & mid_POD <= 730    ~ "12-24 Mo",
    mid_POD >  730 & mid_POD <= 1825   ~ "24-60 Mo",
    TRUE                                ~ NA_character_
  )) |>
  filter(!is.na(phase))

phase_levels <- c("0-12 Mo", "12-24 Mo", "24-60 Mo")
phase_anzeige <- c("0-12 Mo" = "0--12", "12-24 Mo" = "12--24",
                   "24-60 Mo" = "24--60", "Gesamt" = "Gesamt")
cat(sprintf("[13_audit] Personen mit Rezidiv=%d  mit Termin bis Rezidiv=%d  mit Abstand=%d\n",
            n_distinct(recur_dates$Pseudonym), n_distinct(vol_rec$Pseudonym),
            n_distinct(intervals$Pseudonym)))

# ── 4. Pro Phase: Median/IQR/95-Perzentil; Anteil Patient:innen ──
phase_summary <- intervals |>
  group_by(phase) |>
  summarise(
    n_pat       = n_distinct(Pseudonym),
    n_int       = dplyr::n(),
    med_w       = median(gap_weeks, na.rm = TRUE),
    q1_w        = quantile(gap_weeks, 0.25, na.rm = TRUE),
    q3_w        = quantile(gap_weeks, 0.75, na.rm = TRUE),
    p95_w       = quantile(gap_weeks, 0.95, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  mutate(phase = factor(phase, levels = phase_levels)) |>
  arrange(phase)

gap_share <- intervals |>
  group_by(phase, Pseudonym) |>
  summarise(any_gap_gt16 = any(gap_weeks > 16), .groups = "drop") |>
  group_by(phase) |>
  summarise(share_gap = mean(any_gap_gt16), .groups = "drop") |>
  mutate(phase = factor(phase, levels = phase_levels))

audit <- phase_summary |>
  left_join(gap_share, by = "phase")

total_row <- intervals |>
  summarise(
    phase = "Gesamt",
    n_pat = n_distinct(Pseudonym),
    n_int = dplyr::n(),
    med_w = median(gap_weeks, na.rm = TRUE),
    q1_w  = quantile(gap_weeks, 0.25, na.rm = TRUE),
    q3_w  = quantile(gap_weeks, 0.75, na.rm = TRUE),
    p95_w = quantile(gap_weeks, 0.95, na.rm = TRUE)
  ) |>
  mutate(share_gap = intervals |>
           group_by(Pseudonym) |>
           summarise(any_gap_gt16 = any(gap_weeks > 16)) |>
           pull(any_gap_gt16) |>
           mean())

audit <- bind_rows(
  audit |> mutate(phase = as.character(phase)),
  total_row
)

print(audit)

# ── 5. LaTeX-Fragment im threeparttable-Stil ──
fmt_num <- function(x, d = 1) formatC(x, format = "f", digits = d)
fmt_pct <- function(x) formatC(x * 100, format = "f", digits = 1)

out <- paste0(
  "% Imaging-Density-Audit: per-Patient Inter-Bildgebungs-Intervall zwischen OP und Erstrezidiv.\n",
  "% Quelle: 02_Code/13_Imaging_Density_Audit.R\n",
  "\\begin{tabular}{lrrrrrr}\n",
  "\\toprule\n",
  "\\shortstack[l]{Nachsorgephase\\\\ (Monate)} & N & Abst\\\"ande & \\shortstack[r]{Median\\\\ (Wochen)} & \\shortstack[r]{IQR\\\\ (Wochen)} & \\shortstack[r]{95. Perzentil\\\\ (Wochen)} & \\shortstack[r]{L\\\"ucke > 16\\\\ Wochen (\\%)} \\\\\n",
  "\\midrule\n",
  paste(
    apply(audit, 1, function(r) {
      sprintf(
        "%s & %s & %s & %s & (%s--%s) & %s & %s \\\\\n",
        phase_anzeige[[r["phase"]]], r["n_pat"], r["n_int"],
        fmt_num(as.numeric(r["med_w"]), 1),
        fmt_num(as.numeric(r["q1_w"]),  1),
        fmt_num(as.numeric(r["q3_w"]),  1),
        fmt_num(as.numeric(r["p95_w"]), 1),
        fmt_pct(as.numeric(r["share_gap"]))
      )
    }),
    collapse = ""
  ),
  "\\bottomrule\n",
  "\\end{tabular}\n"
)

out_path <- "03_Tables_Figures/Tab_3_28b_Imaging_Density_Audit.tex"
writeLines(out, out_path, useBytes = TRUE)
message("Wrote: ", out_path)

local({
  first_img <- termine |>
    filter(!is.na(Untersuchungsdatum), !is.na(OP_Datum)) |>
    mutate(POD_days = as.numeric(difftime(Untersuchungsdatum, OP_Datum,
                                          units = "days"))) |>
    filter(POD_days > 0) |>
    group_by(Pseudonym) |>
    summarise(first_img_POD = min(POD_days), .groups = "drop")
  cmp <- stat |>
    filter(!is.na(POD_first_postop_volumetry)) |>
    select(Pseudonym, D_rezidiv, POD_first_postop_volumetry) |>
    inner_join(first_img, by = "Pseudonym") |>
    mutate(diff_days = POD_first_postop_volumetry - first_img_POD)
  for (g in c(1, 0)) {
    gg <- cmp[cmp$D_rezidiv == g, ]
    cat(sprintf(paste0("[textdiag] Rezidiv=%d: erste Bildgebung zugleich ",
                       "volumetriert bei %.1f %% (n=%d), medianer Abstand ",
                       "%.0f Tage\n"),
                g, 100 * mean(gg$diff_days == 0, na.rm = TRUE), nrow(gg),
                median(gg$diff_days, na.rm = TRUE)))
  }
})
