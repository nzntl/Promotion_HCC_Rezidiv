# 06e_TimingRestricted_PostopVol.R -- Zeitfenster-Sensitivitaet der postoperativen Volumetrie
# (alle ersten postoperativen Messungen gegen das Fenster POD 60-120); Ergebnisse nur im Protokoll.

suppressMessages({
  library(readxl); library(writexl); library(dplyr); library(survival)
})
if (!dir.exists("01_Daten")) {
  stop("01_Daten nicht gefunden - bitte aus dem Projektstamm ausfuehren.")
}

static <- read_excel("01_Daten/data_static.xlsx")
surv   <- read_excel("01_Daten/data_dynamic_surv.xlsx")

df <- surv |>
  left_join(static |> select(Pseudonym, Rezidivtumor, D_rezidiv,
                             POD_first_postop_volumetry, TLV_first_postop,
                             d_TLV_rel_FLV_base_first_postop),
            by = "Pseudonym") |>
  filter(is.na(Rezidivtumor) | tolower(Rezidivtumor) != "ja") |>
  mutate(POD = suppressWarnings(as.numeric(POD_first_postop_volumetry)))

WIN_LO <- 60; WIN_HI <- 120

feats <- c("TLV erste postop"            = "TLV_first_postop",
           "$\\Delta$\\,TLV vs.\\ FLV (1.\\ postop)" = "d_TLV_rel_FLV_base_first_postop")

fit_one <- function(d, col) {
  d2 <- d |> filter(!is.na(.data[[col]]), !is.na(time), time > 0)
  if (nrow(d2) < 20) return(sprintf("n.b.\\ (n=%d)", nrow(d2)))
  d2$z <- as.numeric(scale(d2[[col]]))
  s <- summary(coxph(Surv(time, event) ~ z, data = d2))
  sprintf("%.2f (%.2f--%.2f), p=%.3f, n=%d",
          s$conf.int["z", "exp(coef)"], s$conf.int["z", "lower .95"],
          s$conf.int["z", "upper .95"], s$coefficients["z", "Pr(>|z|)"],
          nrow(d2))
}

df_restr <- df |> filter(!is.na(POD), POD >= WIN_LO, POD <= WIN_HI)

EXTREM_HI <- 250
df_ohne <- df |>
  filter(is.na(d_TLV_rel_FLV_base_first_postop) |
           d_TLV_rel_FLV_base_first_postop <= EXTREM_HI)

rows <- lapply(names(feats), function(lab) {
  data.frame(Feature = lab,
             Full = fit_one(df, feats[[lab]]),
             Restricted = fit_one(df_restr, feats[[lab]]),
             OhneExtrem = fit_one(df_ohne, feats[[lab]]),
             stringsAsFactors = FALSE)
}) |> bind_rows()
print(rows)

pod_check <- function(d, label) {
  d <- d |> filter(!is.na(POD))
  rec <- d |> filter(D_rezidiv == 1) |> pull(POD)
  non <- d |> filter(D_rezidiv == 0) |> pull(POD)
  p <- tryCatch(wilcox.test(rec, non)$p.value, error = function(e) NA)
  cat(sprintf("[timing] %-18s median POD recur=%.0f (n=%d) vs non-recur=%.0f (n=%d), Wilcoxon p=%.4f\n",
              label, median(rec), length(rec), median(non), length(non), p))
}
pod_check(df, "full")
pod_check(df_restr, "restricted 60-120")

if (FALSE) {
out <- "03_Tables_Figures/Tab_TimingSensitivity_PostopVol.tex"
con <- file(out, "w", encoding = "UTF-8")
writeLines(c(
  paste("% Zeitfenster-Sensitivitaet der Postop-Volumetrie: alle ersten postoperativen",
        "Messungen gegen das Fenster POD 60--120. Die letzte Spalte schliesst die",
        "beiden staerksten Regenerationswerte aus (ueber 250 % gegenueber der",
        "FLV) und zeigt deren Einfluss auf den Zusammenhang."),
  "\\begin{tabular}{llll}", "\\toprule",
  "Postop-Volumetrie (HR pro SD) & Alle Zeitpunkte & POD 60--120 & Ohne Extremwerte \\\\",
  "\\midrule"), con)
for (i in seq_len(nrow(rows))) {
  writeLines(sprintf("%s & %s & %s & %s \\\\", rows$Feature[i], rows$Full[i],
                     rows$Restricted[i], rows$OhneExtrem[i]), con)
}
writeLines(c("\\bottomrule", "\\end{tabular}"), con)
close(con)
cat("[timing] wrote", out, "\n")
cat("DONE_TIMING\n")
}
