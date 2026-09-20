# 06c_FineGray_CompetingRisk.R -- Konkurrenzrisiko-Analyse (Fine-Gray) und 90-Tage-
# Sensitivitaet zum Rezidivendpunkt. Liest 01_Daten/data_static.xlsx, data_dynamic_surv.xlsx.

suppressMessages({
  library(readxl); library(writexl); library(dplyr); library(survival)
})

if (!dir.exists("01_Daten")) {
  stop("01_Daten nicht gefunden - bitte aus dem Projektstamm ausfuehren.")
}

static <- read_excel("01_Daten/data_static.xlsx")
surv   <- read_excel("01_Daten/data_dynamic_surv.xlsx")

keep_static <- static |>
  select(Pseudonym, Rezidivtumor, FUStatus, FU_duration_days,
         Tumordurchmesser, T, lesion_multi_bin, Leberzirrhose, Alter,
         ALBI_grade)

df <- surv |>
  left_join(keep_static, by = "Pseudonym") |>
  filter(is.na(Rezidivtumor) | tolower(Rezidivtumor) != "ja")

df <- df |>
  mutate(
    cr_time = if_else(event == 1, as.numeric(time),
                      pmax(as.numeric(time),
                           as.numeric(coalesce(FU_duration_days, time)),
                           na.rm = TRUE)),
    tumdiam_sd = as.numeric(scale(suppressWarnings(as.numeric(Tumordurchmesser)))),
    age_sd     = as.numeric(scale(suppressWarnings(as.numeric(Alter)))),
    lesion_multi = as.integer(lesion_multi_bin),
    cirrhosis  = if_else(tolower(as.character(Leberzirrhose)) == "ja", 1L, 0L,
                         missing = NA_integer_),
    albi2plus  = if_else(as.character(ALBI_grade) %in% c("Grade 2", "Grade 3"),
                         1L, 0L, missing = 0L)
  ) |>
  filter(!is.na(cr_time), cr_time > 0,
         !is.na(tumdiam_sd), !is.na(age_sd))

cat(sprintf("[finegray] Datensatz n=%d  recurrences=%d  competing deaths=%d  censored=%d  (davon death90=%d)\n",
            nrow(df), sum(df$cr_status == 1), sum(df$cr_status == 2),
            sum(df$cr_status == 0), sum(df$death90 == 1, na.rm = TRUE)))

df$cr_f <- factor(df$cr_status, levels = c(0, 1, 2),
                  labels = c("censor", "recurrence", "death"))

covs <- c("tumdiam_sd", "age_sd", "lesion_multi", "cirrhosis", "albi2plus")
fml  <- as.formula(paste("Surv(cr_time, cr_f) ~", paste(covs, collapse = " + ")))

fg     <- finegray(fml, data = df, etype = "recurrence")
fit_fg <- coxph(as.formula(paste(
  "Surv(fgstart, fgstop, fgstatus) ~", paste(covs, collapse = " + "))),
  weight = fgwt, data = fg)

df$ev_rec <- as.integer(df$cr_status == 1)
fit_cs <- coxph(as.formula(paste("Surv(cr_time, ev_rec) ~",
                                 paste(covs, collapse = " + "))), data = df)

sfg <- summary(fit_fg); scs <- summary(fit_cs)

cat(sprintf("[finegray] Schaetzbasis n=%d  Ereignisse=%d  (%d Faelle ohne vollstaendige Kovariaten)\n",
            fit_cs$n, fit_cs$nevent, nrow(df) - fit_cs$n))
cc <- complete.cases(df[, covs])
stopifnot(sum(cc) == fit_cs$n)
cat(sprintf("[finegray] Schaetzbasis: Rezidive=%d  konkurrierende Todesfaelle=%d (davon death90=%d)  zensiert=%d\n",
            sum(df$cr_status[cc] == 1), sum(df$cr_status[cc] == 2),
            sum(df$death90[cc] == 1, na.rm = TRUE), sum(df$cr_status[cc] == 0)))
cat(sprintf("[finegray] Fehlwerte unter n=%d: Leberzirrhose=%d  Tumoranzahl=%d  beide=%d\n",
            nrow(df), sum(is.na(df$cirrhosis)), sum(is.na(df$lesion_multi)),
            sum(is.na(df$cirrhosis) & is.na(df$lesion_multi))))

lab <- c(tumdiam_sd   = "Tumordurchmesser (pro SD)",
         age_sd       = "Alter (pro SD)",
         lesion_multi = "Tumoranzahl: multipel (vs. solitär)",
         cirrhosis    = "Leberzirrhose",
         albi2plus    = "ALBI-Grad $\\geq$ 2 (vs. 1)")

fmt_hr <- function(hr, lo, hi) sprintf("%.2f (%.2f--%.2f)", hr, lo, hi)
fmt_p  <- function(p) if (p < 0.001) "<0.001" else sprintf("%.3f", p)

rows <- lapply(covs, function(v) {
  data.frame(
    Variable = lab[[v]],
    CS_HR = fmt_hr(scs$conf.int[v, "exp(coef)"], scs$conf.int[v, "lower .95"],
                   scs$conf.int[v, "upper .95"]),
    CS_p  = fmt_p(scs$coefficients[v, "Pr(>|z|)"]),
    FG_HR = fmt_hr(sfg$conf.int[v, "exp(coef)"], sfg$conf.int[v, "lower .95"],
                   sfg$conf.int[v, "upper .95"]),
    FG_p  = fmt_p(sfg$coefficients[v, "Pr(>|z|)"]),
    stringsAsFactors = FALSE)
}) |> bind_rows()

print(rows)

n_rec <- sum(df$cr_status == 1); n_death <- sum(df$cr_status == 2)
out <- "03_Tables_Figures/Tab_FineGray_Competing.tex"
con <- file(out, open = "w", encoding = "UTF-8")
writeLines(c(
  "% Ursachenspezifisches Cox-Modell und Subdistributions-Modell nach Fine und Gray, gleiche Kovariaten und Faelle (M1 sensitivity)",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Ursachenspezifisches Modell} & \\multicolumn{2}{c}{Subdistributions-Modell} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Variable & HR (95\\% KI) & p & sHR (95\\% KI) & p \\\\",
  "\\midrule"
), con)
for (i in seq_len(nrow(rows))) {
  writeLines(sprintf("%s & %s & %s & %s & %s \\\\", rows$Variable[i],
                     rows$CS_HR[i], rows$CS_p[i], rows$FG_HR[i], rows$FG_p[i]), con)
}
writeLines(c("\\bottomrule", "\\end{tabular}"), con)
close(con)
cat(sprintf("[finegray] wrote %s  (recurrences=%d, competing deaths=%d)\n",
            out, n_rec, n_death))

# ── 90-Tage-Mortalitaets-Sensitivitaet ──
df_s90 <- df |> filter(death90 != 1 | is.na(death90))
fit_cs_s90 <- coxph(as.formula(paste("Surv(cr_time, ev_rec) ~",
                                     paste(covs, collapse = " + "))),
                    data = df_s90)
scs90 <- summary(fit_cs_s90)
rows90 <- lapply(covs, function(v) {
  data.frame(
    Variable = lab[[v]],
    Haupt_HR = fmt_hr(scs$conf.int[v, "exp(coef)"], scs$conf.int[v, "lower .95"],
                      scs$conf.int[v, "upper .95"]),
    Haupt_p  = fmt_p(scs$coefficients[v, "Pr(>|z|)"]),
    Ohne_HR  = fmt_hr(scs90$conf.int[v, "exp(coef)"], scs90$conf.int[v, "lower .95"],
                      scs90$conf.int[v, "upper .95"]),
    Ohne_p   = fmt_p(scs90$coefficients[v, "Pr(>|z|)"]),
    stringsAsFactors = FALSE)
}) |> bind_rows()

out90 <- "03_Tables_Figures/Tab_Sensitivity_90Tage.tex"
con90 <- file(out90, open = "w", encoding = "UTF-8")
writeLines(c(
  sprintf(paste0("%% 90-Tage-Mortalitaets-Sensitivitaet: ursachenspezifisches ",
                 "Cox (Kernkovariaten) mit allen Faellen (n=%d) vs. ohne Tod ",
                 "ohne Rezidiv <= 90 d (n=%d; %d Faelle ausgeschlossen). ",
                 "Hauptanalyse behaelt die Faelle (vgl. Chan 2018)."),
          fit_cs$n, fit_cs_s90$n, fit_cs$n - fit_cs_s90$n),
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Alle Fälle} & \\multicolumn{2}{c}{Ohne perioperative Mortalität} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Variable & HR (95\\% KI) & p & HR (95\\% KI) & p \\\\",
  "\\midrule"
), con90)
for (i in seq_len(nrow(rows90))) {
  writeLines(sprintf("%s & %s & %s & %s & %s \\\\", rows90$Variable[i],
                     rows90$Haupt_HR[i], rows90$Haupt_p[i],
                     rows90$Ohne_HR[i], rows90$Ohne_p[i]), con90)
}
writeLines(c("\\bottomrule", "\\end{tabular}"), con90)
close(con90)
cat(sprintf("[finegray] wrote %s  (ausgeschlossen: %d death90-Faelle)\n",
            out90, nrow(df) - nrow(df_s90)))
cat("DONE_FINEGRAY\n")
