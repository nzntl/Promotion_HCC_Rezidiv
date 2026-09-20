# 05b_Regenerationsverlauf.R -- beschreibende Pruefung des Parenchymvolumenverlaufs
# (Plateau nach Resektion). Nicht Teil von 00_Master.R; Aufruf aus dem Projektstamm:
# Rscript 02_Code/05b_Regenerationsverlauf.R. Schreibt Tab_Regenerationsverlauf.tex.

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
})
if (!dir.exists("01_Daten")) stop("01_Daten nicht gefunden - aus 02_Empirical starten.")

TAGE_MONAT       <- 30.4375
MIN_ABSTAND_TAGE <- 28

as_num <- function(x) suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", ".")))
f1  <- function(x) ifelse(is.na(x), "–", formatC(x, format = "f", digits = 1, decimal.mark = ","))
f1v <- function(x) ifelse(is.na(x), "–", paste0(ifelse(x > 0, "+", ""), f1(x)))
iqr_txt <- function(q1, q3, vz = FALSE) {
  g <- if (vz) f1v else f1
  paste0(g(q1), "\\,–\\,", g(q3))
}
quant <- function(x, p) unname(quantile(x, p, na.rm = TRUE))

static  <- read_excel("01_Daten/data_static.xlsx")
dynamic <- read_excel("01_Daten/data_dynamic.xlsx")

analysis_pseu <- static |>
  filter(is.na(Rezidivtumor) | str_to_lower(Rezidivtumor) != "ja") |>
  pull(Pseudonym)
stat <- static  |> filter(Pseudonym %in% analysis_pseu)
dyn  <- dynamic |> filter(Pseudonym %in% analysis_pseu)
cat(sprintf("[05b] Analysekohorte: %d Patient*innen\n", length(analysis_pseu)))

vol <- dyn |>
  filter(datasource == "volumetry") |>
  mutate(pod = as_num(lab_date_POD_imp),
         tlv = as_num(TLV),
         pv  = as_num(TLV_parenchym))

pre <- vol |>
  filter(is.na(pod) | pod <= 0, !is.na(tlv)) |>
  group_by(Pseudonym) |>
  arrange(pod, .by_group = TRUE) |>
  summarise(pv_preop = first(pv), .groups = "drop") |>
  filter(!is.na(pv_preop), pv_preop > 0)
cat(sprintf("[05b] Praeop. Parenchymvolumen vorhanden: %d von %d\n",
            nrow(pre), length(analysis_pseu)))

pat <- stat |>
  transmute(Pseudonym,
            ev_tag = as_num(time_to_first_rezidiv_days),
            major  = factor(case_when(as_num(D_major) == 1 ~ "Major",
                                      as_num(D_major) == 0 ~ "Minor"),
                            levels = c("Minor", "Major")))

post <- vol |>
  filter(!is.na(pod), pod > 0, !is.na(pv), pv > 0) |>
  inner_join(pat, by = "Pseudonym") |>
  filter(is.na(ev_tag) | pod <= ev_tag) |>
  inner_join(pre, by = "Pseudonym") |>
  group_by(Pseudonym, major, pv_preop, pod) |>
  summarise(pv = mean(pv), .groups = "drop") |>
  mutate(monate = pod / TAGE_MONAT,
         pct    = 100 * pv / pv_preop) |>
  arrange(Pseudonym, pod)

n_pat_post <- n_distinct(post$Pseudonym)
cat(sprintf(paste0("[05b] Postop. Volumetrien bis zum ersten Rezidiv: %d bei %d Patient*innen ",
                   "(Minor %d, Major %d, Ausmass fehlt %d)\n"),
            nrow(post), n_pat_post,
            n_distinct(post$Pseudonym[post$major %in% "Minor"]),
            n_distinct(post$Pseudonym[post$major %in% "Major"]),
            n_distinct(post$Pseudonym[is.na(post$major)])))
erst <- post |> group_by(Pseudonym) |> summarise(t = first(pod), .groups = "drop") |> pull(t)
cat(sprintf("[05b] Erste postop. Volumetrie: Median POD %.0f (IQR %.0f-%.0f); bis POD 30: %d, bis POD 91: %d, bis POD 182: %d Patient*innen\n",
            median(erst), quant(erst, 0.25), quant(erst, 0.75),
            sum(erst <= 30), sum(erst <= 91), sum(erst <= 182)))
cat(sprintf("[05b] Werte unter 20 %% oder ueber 160 %% des praeop. Parenchymvolumens: %d\n",
            sum(post$pct < 20 | post$pct > 160)))

# ── A: Median je Zeitfenster, eine Zahl je Person ──
fenster_a <- c("0–3", "3–6", "6–12", "12–24", "24–36")
teil_a <- post |>
  filter(monate <= 36) |>
  mutate(fenster = cut(monate, breaks = c(0, 3, 6, 12, 24, 36),
                       labels = fenster_a, right = TRUE, include.lowest = TRUE)) |>
  group_by(Pseudonym, major, fenster) |>
  summarise(pct = median(pct), .groups = "drop")

sum_a <- function(d, gruppe) {
  d |>
    group_by(fenster, .drop = FALSE) |>
    summarise(n = n(),
              med = if (n() > 0) median(pct) else NA_real_,
              q1  = if (n() > 0) quant(pct, 0.25) else NA_real_,
              q3  = if (n() > 0) quant(pct, 0.75) else NA_real_,
              .groups = "drop") |>
    mutate(gruppe = gruppe)
}
a_tab <- bind_rows(sum_a(teil_a, "Alle"),
                   sum_a(filter(teil_a, major %in% "Minor"), "Minor"),
                   sum_a(filter(teil_a, major %in% "Major"), "Major"))
cat("[05b] A  Parenchymvolumen in % praeop. je Fenster (Monate), eine Zahl je Person:\n")
for (i in seq_len(nrow(a_tab)))
  cat(sprintf("[05b] A  %-5s %-6s n=%3d  Median %s  IQR %s\n",
              a_tab$gruppe[i], as.character(a_tab$fenster[i]), a_tab$n[i],
              f1(a_tab$med[i]), iqr_txt(a_tab$q1[i], a_tab$q3[i])))

# ── B: erste gegen naechste Volumetrie derselben Person ──
naechste <- function(pod) {
  j <- which(pod >= pod[1] + MIN_ABSTAND_TAGE)
  if (length(j) == 0) NA_integer_ else j[1]
}
teil_b <- post |>
  group_by(Pseudonym, major) |>
  summarise(n_mess = n(),
            j    = naechste(pod),
            t1   = pod[1], pct1 = pct[1], pv1 = pv[1],
            t2   = if (is.na(j)) NA_real_ else pod[j],
            pct2 = if (is.na(j)) NA_real_ else pct[j],
            pv2  = if (is.na(j)) NA_real_ else pv[j],
            .groups = "drop")
cat(sprintf(paste0("[05b] B  Personen mit >= 2 Volumetrien bis zum Rezidiv: %d; ",
                   "davon mit zweiter Messung >= %d Tage nach der ersten: %d\n"),
            sum(teil_b$n_mess >= 2), MIN_ABSTAND_TAGE, sum(!is.na(teil_b$t2))))

teil_b <- teil_b |>
  filter(!is.na(t2)) |>
  mutate(erste     = cut(t1 / TAGE_MONAT, breaks = c(0, 3, 6, Inf),
                         labels = c("bis 3", "3–6", "über 6"), right = TRUE),
         delta_pp  = pct2 - pct1,
         pro_monat = 100 * (log(pv2) - log(pv1)) / ((t2 - t1) / TAGE_MONAT))

sum_b <- function(d, gruppe) {
  d |>
    group_by(erste, .drop = FALSE) |>
    summarise(n       = n(),
              t1_med  = if (n() > 0) median(t1) / TAGE_MONAT else NA_real_,
              t2_med  = if (n() > 0) median(t2) / TAGE_MONAT else NA_real_,
              pct1    = if (n() > 0) median(pct1) else NA_real_,
              pct2    = if (n() > 0) median(pct2) else NA_real_,
              dpp     = if (n() > 0) median(delta_pp) else NA_real_,
              dpp_q1  = if (n() > 0) quant(delta_pp, 0.25) else NA_real_,
              dpp_q3  = if (n() > 0) quant(delta_pp, 0.75) else NA_real_,
              plus5   = if (n() > 0) 100 * mean(delta_pp >= 5) else NA_real_,
              pm      = if (n() > 0) median(pro_monat) else NA_real_,
              .groups = "drop") |>
    mutate(gruppe = gruppe)
}
b_tab <- bind_rows(sum_b(teil_b, "Alle"),
                   sum_b(filter(teil_b, major %in% "Minor"), "Minor"),
                   sum_b(filter(teil_b, major %in% "Major"), "Major"))
cat("[05b] B  Erste gegen naechste Volumetrie, nach Zeitpunkt der ersten (Monate):\n")
for (i in seq_len(nrow(b_tab)))
  cat(sprintf(paste0("[05b] B  %-5s erste %-6s n=%3d  Monat %s -> %s  Prozent %s -> %s  ",
                     "Veraenderung %s pp (IQR %s)  >= +5 pp: %s %%  je Monat %s %%\n"),
              b_tab$gruppe[i], as.character(b_tab$erste[i]), b_tab$n[i],
              f1(b_tab$t1_med[i]), f1(b_tab$t2_med[i]),
              f1(b_tab$pct1[i]), f1(b_tab$pct2[i]),
              f1v(b_tab$dpp[i]), iqr_txt(b_tab$dpp_q1[i], b_tab$dpp_q3[i], vz = TRUE),
              f1(b_tab$plus5[i]), f1v(b_tab$pm[i])))

# ── C: Zuwachs je Monat zwischen aufeinanderfolgenden Messungen ──
fenster_c <- c("0–3", "3–6", "6–12", "12–24", "über 24")
paare <- post |>
  group_by(Pseudonym) |>
  mutate(pod_vor = lag(pod), pv_vor = lag(pv)) |>
  ungroup() |>
  filter(!is.na(pod_vor), pod - pod_vor >= MIN_ABSTAND_TAGE) |>
  mutate(mitte     = (pod + pod_vor) / 2 / TAGE_MONAT,
         pro_monat = 100 * (log(pv) - log(pv_vor)) / ((pod - pod_vor) / TAGE_MONAT),
         fenster   = cut(mitte, breaks = c(0, 3, 6, 12, 24, Inf), labels = fenster_c))
cat(sprintf("[05b] C  Messpaare mit >= %d Tagen Abstand: %d aus %d Personen\n",
            MIN_ABSTAND_TAGE, nrow(paare), n_distinct(paare$Pseudonym)))
paare_person <- paare |>
  group_by(Pseudonym, major, fenster) |>
  summarise(pro_monat = median(pro_monat), .groups = "drop")

sum_c <- function(d, gruppe) {
  d |>
    group_by(fenster, .drop = FALSE) |>
    summarise(n   = n(),
              med = if (n() > 0) median(pro_monat) else NA_real_,
              q1  = if (n() > 0) quant(pro_monat, 0.25) else NA_real_,
              q3  = if (n() > 0) quant(pro_monat, 0.75) else NA_real_,
              .groups = "drop") |>
    mutate(gruppe = gruppe)
}
c_tab <- bind_rows(sum_c(paare_person, "Alle"),
                   sum_c(filter(paare_person, major %in% "Minor"), "Minor"),
                   sum_c(filter(paare_person, major %in% "Major"), "Major"))
cat("[05b] C  Zuwachs je Monat (%, log-basiert) zwischen aufeinanderfolgenden Messungen, nach Fenster der Paarmitte, eine Zahl je Person:\n")
for (i in seq_len(nrow(c_tab)))
  cat(sprintf("[05b] C  %-5s %-8s n=%3d  Median %s  IQR %s\n",
              c_tab$gruppe[i], as.character(c_tab$fenster[i]), c_tab$n[i],
              f1v(c_tab$med[i]), iqr_txt(c_tab$q1[i], c_tab$q3[i], vz = TRUE)))

# ── Belegtabelle ──
zelle <- function(tab, fe, g, vz) {
  x <- tab |> filter(fenster == fe, gruppe == g)
  if (nrow(x) == 0 || x$n == 0) return("0 & –")
  wert <- if (vz) f1v(x$med) else f1(x$med)
  sprintf("%d & %s (%s)", x$n, wert, iqr_txt(x$q1, x$q3, vz = vz))
}
zeile <- function(tab, fe, vz) {
  sprintf("%s & %s & %s & %s \\\\", fe,
          zelle(tab, fe, "Alle", vz), zelle(tab, fe, "Minor", vz), zelle(tab, fe, "Major", vz))
}
out <- c(
  "% Volumenverlauf der Analyse-Kohorte: Parenchymvolumen (TLV minus Tumor), Messungen bis zum ersten Rezidiv. Belegtabelle, in kein Kapitel eingebunden. Quelle: 02_Code/05b_Regenerationsverlauf.R",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Monate & n & Alle & n & Minor & n & Major \\\\",
  "\\midrule",
  "\\multicolumn{7}{@{}l}{\\textit{Zuwachs je Monat zwischen aufeinanderfolgenden Messungen (\\%), Median (IQR)}} \\\\[2pt]",
  vapply(fenster_c, function(fe) zeile(c_tab, fe, TRUE), character(1)),
  "\\addlinespace",
  "\\multicolumn{7}{@{}l}{\\textit{Parenchymvolumen (\\% präoperativ), Median (IQR)}} \\\\[2pt]",
  vapply(fenster_a, function(fe) zeile(a_tab, fe, FALSE), character(1)),
  "\\bottomrule",
  "\\end{tabular}"
)
out_path <- "03_Tables_Figures/Tab_Regenerationsverlauf.tex"
writeLines(out, out_path, useBytes = TRUE)
cat(sprintf("[05b] geschrieben: %s\n", out_path))
