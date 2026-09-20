# 02b_Correlation_Heatmap.R -- Kollinearitaetsdiagnostik der Praediktoren
# (Spearman-Korrelation, Cramers V); Ergebnisse nur im Protokoll. Liest 01_Daten/data_static.xlsx.

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(ggplot2); library(viridis); library(ggcorrplot)
})

fig_dir <- "./03_Tables_Figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

static <- read_excel("./01_Daten/data_static.xlsx")
analysis <- static |>
  filter(is.na(Rezidivtumor) | str_to_lower(Rezidivtumor) != "ja")

cat(sprintf("[02b_Correlation_Heatmap] Analyse-Kohorte n=%d\n", nrow(analysis)))

# ── Derived helper variables ──
analysis <- analysis |>
  mutate(
    FLV_TLV_ratio = ifelse(TLV_preop > 0, FLV / TLV_preop, NA_real_),
    LVR_index     = ifelse(FLV > 0, TLV_first_postop / FLV, NA_real_),
    cirrhosis_b   = as.integer(Leberzirrhose == "ja"),
    aetio_masld_b = as.integer(as.character(äthiologie_cat) == "MASLD"),
    aetio_viral_b = as.integer(as.character(äthiologie_cat) %in% c("HBV", "HCV")),
    aetio_ethyl_b = as.integer(as.character(äthiologie_cat) == "Ethyltox"),
    dm_b          = as.integer(str_to_lower(as.character(DM)) == "ja"),
    laparoskop_b  = as.integer(OP_laparoskop == "laparoskopisch"),
    major_hep_b   = suppressWarnings(as.integer(D_major))
  )

# ── (1) Spearman-Korrelations-Matrix ──
spearman_vars <- list(
  "Alter (Jahre)"           = "Alter",
  "BMI"                     = "BMI",
  "Leberzirrhose (0/1)"     = "cirrhosis_b",
  "Child-Pugh (Punkte)"     = "ChildPugh_Punkte",
  "Ätiologie MASLD (0/1)" = "aetio_masld_b",
  "Ätiologie viral (0/1)"      = "aetio_viral_b",
  "Ätiologie ethyltox. (0/1)"  = "aetio_ethyl_b",
  "Diabetes mellitus (0/1)" = "dm_b",
  "Major-Hepatektomie (0/1)" = "major_hep_b",
  "Tumordurchmesser (cm)"   = "Tumordurchmesser",
  "FLV (cm³)"               = "FLV",
  "TLV praeop (cm³)"        = "TLV_preop",
  "TLV 1. postop (cm³)"     = "TLV_first_postop",
  "FLV/TLV-Ratio"           = "FLV_TLV_ratio",
  "Delta TLV vs FLV (%)"    = "d_TLV_rel_FLV_base_first_postop",
  "LVR-Index (TLV/FLV)"     = "LVR_index",
  "LVR-Index POD 91-180"    = "LVR_nam",
  "TLV-Log-Wachstumsrate"   = "TLV_growth_rate",
  "Quick-Slope POD 1-7"     = "slope_Quick_POD1_7",
  "GOT-Slope POD 1-7"       = "slope_GOT_POD1_7",
  "GPT-Slope POD 1-7"       = "slope_GPT_POD1_7",
  "CRP-Slope POD 1-7"       = "slope_CRP_POD1_7",
  "Albumin-Slope POD 1-7"   = "slope_Albumin_POD1_7",
  "Leukozyten-Slope POD 1-7"   = "slope_Leukos_POD1_7",
  "Thrombozyten-Slope POD 1-7" = "slope_Thrombos_POD1_7"
)
have <- vapply(spearman_vars, function(v) v %in% names(analysis), logical(1))
if (any(!have)) {
  message("[02b_Correlation_Heatmap] dropping unavailable predictors: ",
          paste(unlist(spearman_vars)[!have], collapse = ", "))
  spearman_vars <- spearman_vars[have]
}

sp_df <- analysis |>
  select(all_of(unlist(spearman_vars))) |>
  mutate(across(everything(), as.numeric))
names(sp_df) <- names(spearman_vars)

corr_mat <- suppressWarnings(
  cor(sp_df, method = "spearman", use = "pairwise.complete.obs")
)
cat(sprintf("[02b_Correlation_Heatmap] Spearman-Matrix: %dx%d Variablen, ",
            ncol(corr_mat), nrow(corr_mat)))
cat(sprintf("median |r| = %.3f, max |r| (off-diag) = %.3f\n",
            median(abs(corr_mat[upper.tri(corr_mat)]), na.rm = TRUE),
            max(abs(corr_mat[upper.tri(corr_mat)]), na.rm = TRUE)))

p_spear <- ggcorrplot(
  corr_mat,
  method   = "square",
  type     = "lower",
  lab      = TRUE,
  lab_size = 2.4,
  digits   = 2,
  outline.color = "white",
  tl.cex   = 8,
  tl.srt   = 60
) +
  scale_fill_viridis_c(
    option = "viridis",
    limits = c(-1, 1),
    name   = "Spearman r"
  ) +
  theme(
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )

if (FALSE) ggsave(file.path(fig_dir, "Abb_Korrelations_Heatmap.png"),
       plot = p_spear, width = 9, height = 8, dpi = 220, bg = "white")
cat("[02b_Correlation_Heatmap] Spearman-Heatmap entfaellt als Objekt (Log-only).\n")

# ── (2) Cramér-V-Matrix fuer rein-kategoriale Variablen ──
cramers_v <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 5) return(NA_real_)
  tab <- table(x[ok], y[ok])
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))$statistic
  n   <- sum(tab)
  k   <- min(nrow(tab), ncol(tab))
  unname(sqrt(chi / (n * (k - 1))))
}

cat_vars <- list(
  "Geschlecht"        = "mwd",
  "Aethiologie"       = "äthiologie_cat",
  "Vorbehandlung"     = "Vorbehandlung",
  "Leberzirrhose"     = "Leberzirrhose",
  "Child-Pugh Stadium"= "ChildPugh_Stadium",
  "T-Stadium"         = "T",
  "N-Stadium"         = "N",
  "M-Stadium"         = "M",
  "DM"                = "DM",
  "OP laparoskop."    = "OP_laparoskop",
  "OP CCE"            = "OP_CCE",
  "Symptom-Kategorie" = "Symptom_cat"
)
have_cat <- vapply(cat_vars, function(v) v %in% names(analysis), logical(1))
if (any(!have_cat)) {
  message("[02b_Correlation_Heatmap] dropping unavailable categoricals: ",
          paste(unlist(cat_vars)[!have_cat], collapse = ", "))
  cat_vars <- cat_vars[have_cat]
}

cat_df <- analysis |>
  select(all_of(unlist(cat_vars))) |>
  mutate(across(everything(), as.character))
names(cat_df) <- names(cat_vars)

K <- ncol(cat_df)
cv_mat <- matrix(NA_real_, K, K, dimnames = list(names(cat_df), names(cat_df)))
for (i in seq_len(K)) {
  for (j in seq_len(K)) {
    cv_mat[i, j] <- if (i == j) 1 else cramers_v(cat_df[[i]], cat_df[[j]])
  }
}

p_cv <- ggcorrplot(
  cv_mat,
  method = "square",
  type   = "lower",
  lab    = TRUE,
  lab_size = 2.6,
  digits = 2,
  outline.color = "white",
  tl.cex = 9,
  tl.srt = 60
) +
  scale_fill_viridis_c(
    option = "viridis",
    limits = c(0, 1),
    name   = "Cramér-V"
  ) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )

if (FALSE) ggsave(file.path(fig_dir, "Abb_CramersV_Heatmap.png"),
       plot = p_cv, width = 8, height = 7, dpi = 220, bg = "white")
cat("[02b_Correlation_Heatmap] Cramer-V-Heatmap entfaellt als Objekt (Log-only).\n")

# ── Flag high-correlation pairs (|r| >= 0.7 Spearman, V >= 0.6 Cramér) ──
flag_rows <- list()

hi_spear <- which(abs(corr_mat) >= 0.7 & upper.tri(corr_mat), arr.ind = TRUE)
if (nrow(hi_spear) > 0) {
  cat("[02b_Correlation_Heatmap] hohe Spearman-Korrelationen (|r|>=0.7):\n")
  for (k in seq_len(nrow(hi_spear))) {
    i <- hi_spear[k, "row"]; j <- hi_spear[k, "col"]
    cat(sprintf("   %s  <->  %s :  r = %+.3f\n",
                rownames(corr_mat)[i], colnames(corr_mat)[j],
                corr_mat[i, j]))
    flag_rows[[length(flag_rows) + 1L]] <- data.frame(
      Metrik = "Spearman r",
      `Variable 1` = rownames(corr_mat)[i],
      `Variable 2` = colnames(corr_mat)[j],
      Wert = sprintf("%+.3f", corr_mat[i, j]),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
}

hi_cv <- which(cv_mat >= 0.6 & upper.tri(cv_mat), arr.ind = TRUE)
if (nrow(hi_cv) > 0) {
  cat("[02b_Correlation_Heatmap] hohe Cramér-V (>=0.6):\n")
  for (k in seq_len(nrow(hi_cv))) {
    i <- hi_cv[k, "row"]; j <- hi_cv[k, "col"]
    cat(sprintf("   %s  <->  %s :  V = %.3f\n",
                rownames(cv_mat)[i], colnames(cv_mat)[j], cv_mat[i, j]))
    flag_rows[[length(flag_rows) + 1L]] <- data.frame(
      Metrik = "Cramér-V",
      `Variable 1` = rownames(cv_mat)[i],
      `Variable 2` = colnames(cv_mat)[j],
      Wert = sprintf("%.3f", cv_mat[i, j]),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
}

if (length(flag_rows) > 0) {
  flag_tab <- do.call(rbind, flag_rows)
  esc <- function(x) {
    x <- as.character(x); x[is.na(x)] <- "--"
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    gsub("([&%$#_{}])", "\\\\\\1", x)
  }
  hdr <- paste(esc(names(flag_tab)), collapse = " & ")
  body <- apply(flag_tab, 1, function(r) paste(esc(r), collapse = " & "))
  body <- paste0(body, " \\\\")
  out <- c(
    paste0("% Multikollinearitaets-Flags (Spearman |r|>=0.7 fuer kontinuierlich/binaer; ",
           "Cramér-V>=0.6 fuer kategorial). Begleitet die Heatmaps Abb_Korrelations_",
           "Heatmap und Abb_CramersV_Heatmap; referenziert in §4.1.3 Methodische ",
           "Diskussion zur Erklärung breiter CIs im Forest-Plot."),
    "\\begin{tabular}{llll}",
    "\\toprule",
    paste0(hdr, " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  )
  cat("[02b_Correlation_Heatmap] Kollinearitaets-Flags (Log-only):\n")
  print(flag_tab, row.names = FALSE)
  if (FALSE) {
    path <- file.path(fig_dir, "Tab_Korrelations_Flags.tex")
    con <- file(path, "w", encoding = "UTF-8")
    writeLines(out, con); close(con)
    cat(sprintf("[02b_Correlation_Heatmap] wrote %s (%d Paare)\n",
                path, nrow(flag_tab)))
  }
}
