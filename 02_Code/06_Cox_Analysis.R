# 06_Cox_Analysis.R -- univariate Cox-Modelle, multivariate Modelle A bis C, Annahmepruefung,
# Forest-Plot, KM-Strata, Splines und Interaktionen.
# Liest 01_Daten/data_static.xlsx, data_dynamic_surv.xlsx.

pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "ggplot2",
          "wesanderson", "survival", "broom")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")

library(readxl); library(dplyr); library(tidyr); library(stringr)
library(ggplot2); library(wesanderson); library(survival); library(broom)

static  <- read_excel("./01_Daten/data_static.xlsx")
surv_df <- read_excel("./01_Daten/data_dynamic_surv.xlsx")

analysis_pseu <- static |>
  filter(is.na(Rezidivtumor) | str_to_lower(Rezidivtumor) != "ja") |>
  pull(Pseudonym)
static_an  <- static  |> filter(Pseudonym %in% analysis_pseu)
surv_an    <- surv_df |> filter(Pseudonym %in% analysis_pseu)

sdat <- surv_an |>
  left_join(static_an, by = "Pseudonym")

zscore <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

sdat <- sdat |>
  mutate(
    sex_m         = as.integer(str_to_lower(mwd) == "m"),
    cirrhosis     = as.integer(str_to_lower(Leberzirrhose) == "ja"),
    FLV_TLV_ratio = FLV / TLV_preop,
    LVR_index     = TLV_first_postop / FLV,
    age_c         = suppressWarnings(as.numeric(Alter)),
    bmi_c         = suppressWarnings(as.numeric(BMI)),
    cp_c          = suppressWarnings(as.numeric(ChildPugh_Punkte)),
    tum_d_c       = suppressWarnings(as.numeric(Tumordurchmesser)),
    flv_c         = suppressWarnings(as.numeric(FLV)),
    tlv_preop_c   = suppressWarnings(as.numeric(TLV_preop)),
    TLV_first_postop = suppressWarnings(as.numeric(TLV_first_postop)),
    d_TLV_rel_FLV_base_first_postop =
      suppressWarnings(as.numeric(d_TLV_rel_FLV_base_first_postop)),
    quick_slope    = suppressWarnings(as.numeric(slope_Quick_POD1_7)),
    got_slope      = suppressWarnings(as.numeric(slope_GOT_POD1_7)),
    gpt_slope      = suppressWarnings(as.numeric(slope_GPT_POD1_7)),
    crp_slope      = suppressWarnings(as.numeric(slope_CRP_POD1_7)),
    albumin_slope  = suppressWarnings(as.numeric(slope_Albumin_POD1_7)),
    thrombos_slope = suppressWarnings(as.numeric(slope_Thrombos_POD1_7)),
    aetio_model    = factor(as.character(aetio_model),
                            levels = c("Rest", "MASLD", "viral", "ethyltox")),
    dm_bin         = as.integer(str_to_lower(as.character(DM)) == "ja"),
    nikotin_bin    = as.integer(ND_Nikotin),
    # ── Block A: T-Stadium (deskriptiv) + Tumoranzahl ──
    T_stadium_ord = suppressWarnings(as.numeric(str_extract(as.character(T), "[0-9]+"))),
    lesion_multi_bin = as.integer(lesion_multi_bin),
    # ── Block B: TTLVR (Lian 2022) ──
    ttlvr_c        = suppressWarnings(as.numeric(TTLVR_preop)),
    # ── Neue Volumetrie-Fassungen, nur univariat ──
    lvr_nam_c      = suppressWarnings(as.numeric(LVR_nam)),
    tlv_growth_c   = suppressWarnings(as.numeric(TLV_growth_rate)),
    # ── Block C: präop Labor + Scores ──
    bili_preop_c   = suppressWarnings(as.numeric(Bili_preop_val)),
    alb_preop_c    = suppressWarnings(as.numeric(Albumin_preop)),
    quick_preop_c  = suppressWarnings(as.numeric(Quick_preop)),
    deritis_c      = suppressWarnings(as.numeric(deritis_preop)),
    astalb_c       = suppressWarnings(as.numeric(astalb_preop)),
    apri_c         = suppressWarnings(as.numeric(APRI_preop)),
    leukos_preop_c   = suppressWarnings(as.numeric(Leukos_preop)),
    crp_preop_c      = suppressWarnings(as.numeric(CRP_preop)),
    thrombos_preop_c = suppressWarnings(as.numeric(Thrombos_preop)),
    nd_count_c       = suppressWarnings(as.numeric(ND_count)),
    n_seg_c          = suppressWarnings(as.numeric(n_op_segments)),
    major_bin        = suppressWarnings(as.integer(D_major)),
    albi_grade_f   = factor(as.character(ALBI_grade),
                            levels = c("Grade 1", "Grade 2", "Grade 3")),
    symptom_f      = factor(as.character(Symptom_cat),
                            levels = c("Asymptomatisch_Zufall",
                                       "Asymptomatisch_Ueberwachung",
                                       "Symptomatisch")),
    flv_sd               = zscore(flv_c),
    tlv_preop_sd         = zscore(tlv_preop_c),
    tlv_first_postop_sd  = zscore(TLV_first_postop),
    d_TLV_rel_FLV_sd     = zscore(d_TLV_rel_FLV_base_first_postop),
    FLV_TLV_ratio_sd     = zscore(FLV_TLV_ratio),
    LVR_index_sd         = zscore(LVR_index),
    tum_d_sd             = zscore(tum_d_c),
    ttlvr_sd             = zscore(ttlvr_c),
    Bili_preop_sd        = zscore(bili_preop_c),
    Albumin_preop_sd     = zscore(alb_preop_c),
    Quick_preop_sd       = zscore(quick_preop_c),
    deritis_sd           = zscore(deritis_c),
    astalb_sd            = zscore(astalb_c),
    APRI_preop_sd        = zscore(apri_c),
    lvr_nam_sd           = zscore(lvr_nam_c),
    tlv_growth_sd        = zscore(tlv_growth_c),
    quick_slope_sd       = zscore(quick_slope),
    got_slope_sd         = zscore(got_slope),
    gpt_slope_sd         = zscore(gpt_slope),
    crp_slope_sd         = zscore(crp_slope),
    albumin_slope_sd     = zscore(albumin_slope),
    thrombos_slope_sd    = zscore(thrombos_slope),
    Leukos_preop_sd      = zscore(leukos_preop_c),
    CRP_preop_sd         = zscore(crp_preop_c),
    Thrombos_preop_sd    = zscore(thrombos_preop_c)
  )

fig_dir <- "./03_Tables_Figures"

tex_escape_cell <- function(x) {
  x <- as.character(x); x[is.na(x)] <- "--"
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x); x
}
write_table_tex <- function(tbl, path, caption = NULL, align = NULL) {
  cols <- ncol(tbl)
  if (is.null(align)) align <- paste0(c("l", rep("r", cols - 1)), collapse = "")
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

var_label <- c(
  age_c        = "Alter (Jahre)",
  sex_m        = "männlich",
  cirrhosis    = "Leberzirrhose",
  dm_bin       = "Diabetes mellitus",
  aetio_model         = "Ätiologie (4 Klassen)",
  aetio_modelMASLD    = "Ätiologie: MASLD",
  aetio_modelviral    = "Ätiologie: viral",
  aetio_modelethyltox = "Ätiologie: ethyltoxisch",
  cp_c         = "Child-Pugh Punkte",
  tum_d_c      = "Tumordurchmesser (cm)",
  tum_d_sd     = "Tumordurchmesser (pro SD)",
  bmi_c        = "BMI",
  flv_c        = "FLV (präop, cm³)",
  flv_sd       = "FLV präop (pro SD)",
  tlv_preop_c  = "TLV präop (cm³)",
  tlv_preop_sd = "TLV präop (pro SD)",
  FLV_TLV_ratio    = "FLV/TLV-Ratio (präop)",
  FLV_TLV_ratio_sd = "FLV/TLV-Ratio (pro SD)",
  LVR_index    = "LVR-Index (TLV/FLV)",
  LVR_index_sd = "LVR-Index (pro SD)",
  TLV_first_postop     = "TLV 1. postop (cm³)",
  tlv_first_postop_sd  = "TLV 1. postop (pro SD)",
  d_TLV_rel_FLV_base_first_postop = "Δ TLV vs. FLV (%)",
  d_TLV_rel_FLV_sd                = "Δ TLV vs. FLV (pro SD)",
  D_OP_seg_1   = "OP betraf Segment 1",
  D_OP_seg_4   = "OP betraf Segment 4",
  D_OP_seg_2_3 = "OP betraf Segmente 2 & 3",
  quick_slope     = "Quick-Slope POD 1-7 (%/Tag)",
  quick_slope_sd  = "Quick-Slope POD 1-7 (pro SD)",
  got_slope       = "GOT-Slope POD 1-7 (U/L/Tag)",
  got_slope_sd    = "GOT-Slope POD 1-7 (pro SD)",
  gpt_slope       = "GPT-Slope POD 1-7 (U/L/Tag)",
  gpt_slope_sd    = "GPT-Slope POD 1-7 (pro SD)",
  crp_slope       = "CRP-Slope POD 1-7 (mg/L/Tag)",
  crp_slope_sd    = "CRP-Slope POD 1-7 (pro SD)",
  albumin_slope   = "Albumin-Slope POD 1-7 (g/L/Tag)",
  albumin_slope_sd= "Albumin-Slope POD 1-7 (pro SD)",
  thrombos_slope   = "Thrombozyten-Slope POD 1-7 (/nL/Tag)",
  thrombos_slope_sd= "Thrombozyten-Slope POD 1-7 (pro SD)",
  T_stadium_ord    = "T-Stadium (ordinal 1-4)",
  lesion_multi_bin = "Tumoranzahl: multipel (vs. solitär)",
  nikotin_bin      = "Nikotinkonsum (dokumentiert)",
  ttlvr_c          = "TTLVR (Tumor/TLV)",
  ttlvr_sd         = "TTLVR (Tumor/TLV, pro SD)",
  bili_preop_c     = "Bilirubin präop (mg/dL)",
  Bili_preop_sd    = "Bilirubin präop (pro SD)",
  alb_preop_c      = "Albumin präop (g/L)",
  Albumin_preop_sd = "Albumin präop (pro SD)",
  quick_preop_c    = "Quick präop (%)",
  Quick_preop_sd   = "Quick (pro SD)",
  deritis_c        = "De-Ritis-Quotient (AST/ALT)",
  deritis_sd       = "De-Ritis-Quotient (pro SD)",
  astalb_c         = "AST/Albumin-Ratio",
  astalb_sd        = "AST/Albumin-Ratio (pro SD)",
  apri_c           = "APRI",
  APRI_preop_sd    = "APRI (pro SD)",
  leukos_preop_c   = "Leukozyten präop (/nL)",
  Leukos_preop_sd  = "Leukozyten präop (pro SD)",
  crp_preop_c      = "CRP präop (mg/L)",
  CRP_preop_sd     = "CRP präop (pro SD)",
  thrombos_preop_c = "Thrombozyten präop (/nL)",
  Thrombos_preop_sd= "Thrombozyten präop (pro SD)",
  nd_count_c       = "Zahl der Nebendiagnosen",
  n_seg_c          = "Resezierte Segmente (Anzahl)",
  major_bin        = "Major-Hepatektomie",
  albi_grade_f     = "ALBI-Grad",
  symptom_f        = "Symptomstatus bei Erstdiagnose",
  `symptom_fAsymptomatisch_Ueberwachung` = "Asymptomatische Überwachung",
  `symptom_fSymptomatisch`               = "Symptomatisch bei Erstdiagnose",
  `albi_grade_fGrade 2` = "ALBI-Grad 2 (vs. 1)",
  `albi_grade_fGrade 3` = "ALBI-Grad 3 (vs. 1)",
  lvr_nam_c        = "LVR-Index POD 91-180 (TLV/FLV)",
  lvr_nam_sd       = "LVR-Index POD 91-180 (pro SD)",
  tlv_growth_c     = "TLV-Log-Wachstumsrate (log/Tag)",
  tlv_growth_sd    = "TLV-Log-Wachstumsrate (pro SD)"
)
label_vars <- function(x) {
  ifelse(x %in% names(var_label), var_label[x], x)
}

fmt_hr <- function(hr, lo, hi, p) {
  if (any(!is.finite(c(hr, lo, hi)))) return(c("--","--","--"))
  c(sprintf("%.2f", hr),
    sprintf("(%.2f–%.2f)", lo, hi),
    if (is.na(p)) "--" else if (p < 0.001) "<0.001"
    else if (p > 0.999) ">0.999" else sprintf("%.3f", p))
}

# ── Tab 3.5 — Univariate Cox auf TTR ──

uni_vars <- list(
  list(name = "Alter (Jahre)",                      v = "age_c",         phase = "präop"),
  list(name = "männlich",                       v = "sex_m",         phase = "präop"),
  list(name = "BMI",                                v = "bmi_c",         phase = "präop"),
  list(name = "Leberzirrhose",                      v = "cirrhosis",     phase = "präop"),
  list(name = "Child-Pugh Punkte",                  v = "cp_c",          phase = "präop"),
  list(name = "Diabetes mellitus",                  v = "dm_bin",        phase = "präop"),
  list(name = "Nikotinkonsum (dokumentiert)",       v = "nikotin_bin",   phase = "präop"),
  list(name = "Zahl der Nebendiagnosen",            v = "nd_count_c",    phase = "präop"),
  list(name = "Resezierte Segmente (Anzahl)",       v = "n_seg_c",       phase = "intraop"),
  list(name = "Major-Hepatektomie",                 v = "major_bin",     phase = "intraop"),
  list(name = "T-Stadium (ordinal)",                v = "T_stadium_ord", phase = "postop"),
  list(name = "Symptomstatus (Faktor)",             v = "symptom_f",     phase = "präop"),
  list(name = "Tumoranzahl: multipel (vs. solitär)", v = "lesion_multi_bin", phase = "präop"),
  list(name = "Tumordurchmesser (pro SD)",          v = "tum_d_sd",      phase = "präop"),
  list(name = "Ätiologie (4 Klassen)",              v = "aetio_model",   phase = "präop"),
  list(name = "FLV (pro SD)",                 v = "flv_sd",        phase = "präop"),
  list(name = "TLV (pro SD)",                 v = "tlv_preop_sd",  phase = "präop"),
  list(name = "FLV/TLV-Ratio (pro SD)",             v = "FLV_TLV_ratio_sd", phase = "präop"),
  list(name = "TTLVR (Tumor/TLV, pro SD)",          v = "ttlvr_sd",      phase = "präop"),
  list(name = "Bilirubin (pro SD)",           v = "Bili_preop_sd",   phase = "präop"),
  list(name = "Albumin (pro SD)",             v = "Albumin_preop_sd", phase = "präop"),
  list(name = "Quick (pro SD)",               v = "Quick_preop_sd",  phase = "präop"),
  list(name = "De-Ritis-Quotient (pro SD)",         v = "deritis_sd",     phase = "präop"),
  list(name = "AST/Albumin-Ratio (pro SD)",         v = "astalb_sd",      phase = "präop"),
  list(name = "APRI (pro SD)",                      v = "APRI_preop_sd",  phase = "präop"),
  list(name = "Leukozyten (pro SD)",         v = "Leukos_preop_sd",   phase = "präop"),
  list(name = "CRP (pro SD)",                v = "CRP_preop_sd",      phase = "präop"),
  list(name = "Thrombozyten (pro SD)",       v = "Thrombos_preop_sd", phase = "präop"),
  list(name = "ALBI-Grad (Faktor)",                v = "albi_grade_f",   phase = "präop"),
  list(name = "TLV 1. postop (pro SD)",             v = "tlv_first_postop_sd", phase = "postop"),
  list(name = "Δ TLV vs. FLV (pro SD)",
       v = "d_TLV_rel_FLV_sd",    phase = "postop"),
  list(name = "LVR-Index (pro SD)",
       v = "LVR_index_sd",  phase = "postop"),
  list(name = "LVR (TLV 91-180 / FLV, pro SD)",     v = "lvr_nam_sd",    phase = "postop"),
  list(name = "TLV-Log-Wachstumsrate (pro SD)",    v = "tlv_growth_sd", phase = "postop"),
  list(name = "Quick (pro SD)",       v = "quick_slope_sd",   phase = "postop"),
  list(name = "GOT (pro SD)",         v = "got_slope_sd",     phase = "postop"),
  list(name = "GPT (pro SD)",         v = "gpt_slope_sd",     phase = "postop"),
  list(name = "CRP (pro SD)",         v = "crp_slope_sd",     phase = "postop"),
  list(name = "Albumin (pro SD)",     v = "albumin_slope_sd", phase = "postop"),
  list(name = "Thrombozyten (pro SD)", v = "thrombos_slope_sd", phase = "postop")
)

.uni_drop <- function(item, grund) {
  message("[WARNUNG Tab 3.5] Zeile verworfen: ", item$name,
          " (v=", item$v, ") -- ", grund)
  NULL
}
uni_rows <- lapply(uni_vars, function(item) {
  if (is.na(item$v)) return(NULL)
  if (!item$v %in% names(sdat)) return(.uni_drop(item, "Variable fehlt in sdat"))
  x <- sdat[[item$v]]
  if (is.null(x) || all(is.na(x))) return(.uni_drop(item, "nur fehlende Werte"))
  ok_levels <- if (is.factor(x)) nlevels(droplevels(x[!is.na(x)]))
               else length(unique(x[!is.na(x)]))
  if (ok_levels < 2) return(.uni_drop(item, "weniger als zwei Auspraegungen"))
  ok <- !is.na(x) & !is.na(sdat$time) & !is.na(sdat$event)
  if (sum(ok) < 20) return(.uni_drop(item, sprintf("nur %d verwertbare Faelle (<20)", sum(ok))))
  f <- tryCatch(
    coxph(reformulate(item$v, "Surv(time, event)"), data = sdat[ok, ]),
    error = function(e) NULL)
  if (is.null(f)) return(NULL)
  s <- summary(f)
  if (nrow(s$conf.int) == 0) return(NULL)
  n_coef <- nrow(s$conf.int)
  rows <- lapply(seq_len(n_coef), function(i) {
    hr <- s$conf.int[i, "exp(coef)"]
    lo <- s$conf.int[i, "lower .95"]
    hi <- s$conf.int[i, "upper .95"]
    p  <- s$coefficients[i, "Pr(>|z|)"]
    vals <- fmt_hr(hr, lo, hi, p)
    coef_key <- if (n_coef == 1) item$v else rownames(s$conf.int)[i]
    nm <- if (n_coef == 1) item$name else {
      coef_name <- rownames(s$conf.int)[i]
      if (coef_name %in% names(var_label)) unname(var_label[coef_name]) else {
        suffix <- sub(paste0("^", item$v), "", coef_name)
        sprintf("%s [%s]", item$name, suffix)
      }
    }
    data.frame(Variable = nm, Key = coef_key, Phase = item$phase,
               HR = vals[1], `95% CI` = vals[2], p = vals[3],
               N = sum(ok), Rezidive = f$nevent,
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  bind_rows(rows)
})
uni_tab <- bind_rows(uni_rows)

uni_alle <- uni_tab |> dplyr::select(-Phase)

# ── Tab 3.8 — Univariate Cox als eine Tabelle (Format wie Tab 2.2) ──
gruppen_uni <- list(
  "Präoperativ: Person und Anamnese" = c(
      "age_c", "ref_sex", "sex_m", "bmi_c", "ref_symptom",
      "symptom_fAsymptomatisch_Ueberwachung", "symptom_fSymptomatisch"),
  "Präoperativ: Komorbidität und Ätiologie" = c(
      "dm_bin", "nikotin_bin", "nd_count_c", "ref_aetio",
      "aetio_modelMASLD", "aetio_modelviral", "aetio_modelethyltox"),
  "Präoperativ: Leberfunktion und Labor (POD $-$1)" = c(
      "cirrhosis", "cp_c", "Bili_preop_sd", "Albumin_preop_sd",
      "Quick_preop_sd", "deritis_sd", "astalb_sd", "APRI_preop_sd",
      "albi_grade_fGrade 2", "albi_grade_fGrade 3",
      "Leukos_preop_sd", "CRP_preop_sd", "Thrombos_preop_sd"),
  "Präoperativ: Volumetrie" = c("flv_sd", "tlv_preop_sd",
      "FLV_TLV_ratio_sd", "ttlvr_sd"),
  "Intraoperativ" = c("n_seg_c", "major_bin"),
  "Postoperativ: histopathologischer Befund" = c(
      "tum_d_sd", "lesion_multi_bin", "T_stadium_ord"),
  "Postoperativ: Labor-Steigung (POD 1 bis 7)" = c(
      "albumin_slope_sd", "quick_slope_sd", "got_slope_sd", "gpt_slope_sd",
      "crp_slope_sd", "thrombos_slope_sd"),
  "Postoperativ: Bildgebung und Volumetrie" = c(
      "tlv_first_postop_sd", "d_TLV_rel_FLV_sd", "LVR_index_sd",
      "lvr_nam_sd", "tlv_growth_sd")
)

ref_zeile <- function(df, vor, name, hr_spalten, schluessel = NULL) {
  i <- match(vor, df$Variable)
  if (is.na(i)) return(df)
  neu <- df[i, , drop = FALSE]
  neu[1, ] <- ""
  neu$Variable <- name
  if (!is.null(schluessel) && "Key" %in% names(df)) neu$Key <- schluessel
  neu[1, hr_spalten] <- "Referenz"
  bind_rows(df[seq_len(i - 1), , drop = FALSE], neu,
            df[i:nrow(df), , drop = FALSE])
}
uni_mit_ref <- bind_rows(
  dplyr::mutate(uni_alle, N = as.character(N),
                Rezidive = as.character(Rezidive)),
  data.frame(Variable = c("weiblich", "Zufallsbefund", "Ätiologie: Rest"),
             Key = c("ref_sex", "ref_symptom", "ref_aetio"),
             HR = "Referenz",
             `95% CI` = "", p = "", N = "", Rezidive = "",
             check.names = FALSE, stringsAsFactors = FALSE))

schreibe_uni <- function(tb, gruppen, datei, kurz, lang, marke) {
  kopf <- paste0(paste(c("Variable", "HR", "95\\% KI", "p", "N", "Rezidive"),
                       collapse = " & "), " \\\\")
  out <- c(
    paste0("% ", lang),
    paste0("\\begin{longtable}{@{}",
           ">{\\raggedright\\arraybackslash}p{5.8cm}",
           ">{\\raggedleft\\arraybackslash}p{1.3cm}",
           ">{\\raggedleft\\arraybackslash}p{2.1cm}",
           ">{\\raggedleft\\arraybackslash}p{1.2cm}",
           ">{\\raggedleft\\arraybackslash}p{0.9cm}",
           ">{\\raggedleft\\arraybackslash}p{1.4cm}@{}}"),
    paste0("\\caption[", kurz, "]{", kurz, ".}\\label{", marke, "}\\\\"),
    "\\toprule", kopf, "\\midrule", "\\endfirsthead",
    "\\toprule", kopf, "\\midrule", "\\endhead",
    "\\bottomrule", "\\insertTableNotes", "\\endlastfoot")
  gezeigt <- character(0)
  for (g in names(gruppen)) {
    zeilen <- tb[tb$Key %in% gruppen[[g]], , drop = FALSE]
    if (nrow(zeilen) == 0) next
    zeilen <- zeilen[match(gruppen[[g]], zeilen$Key), , drop = FALSE]
    zeilen <- zeilen[!is.na(zeilen$Key), , drop = FALSE]
    out <- c(out, sprintf("\\multicolumn{6}{@{}l}{\\textit{%s}} \\\\[2pt]",
                          g))
    for (i in seq_len(nrow(zeilen))) {
      out <- c(out, sprintf("%s & %s & %s & %s & %s & %s \\\\",
                            tex_escape_cell(zeilen$Variable[i]),
                            tex_escape_cell(zeilen$HR[i]),
                            tex_escape_cell(zeilen$`95% CI`[i]),
                            tex_escape_cell(zeilen$p[i]),
                            tex_escape_cell(zeilen$N[i]),
                            tex_escape_cell(zeilen$Rezidive[i])))
    }
    out <- c(out, "\\addlinespace")
    gezeigt <- c(gezeigt, zeilen$Key)
  }
  fehlend <- setdiff(tb$Key, gezeigt)
  if (length(fehlend) > 0) {
    message("[WARNUNG uni-Tabelle] Nicht zugeordnet in ", datei, ": ",
            paste(fehlend, collapse = ", "),
            " -- diese Zeilen fehlen in der Ausgabe. gruppen_uni ergaenzen.")
    warning("Nicht zugeordnet in ", datei, ": ",
            paste(fehlend, collapse = ", "))
  }
  out <- c(out, "\\end{longtable}")
  writeLines(out, file.path(fig_dir, datei), useBytes = TRUE)
  message("Wrote: ", datei, " (", length(gezeigt), " Variablen, ",
          length(gruppen), " Gruppen)")
}

schreibe_uni(uni_mit_ref, gruppen_uni,
             "Tab_3_5_Univariate_Cox.tex",
             "Univariable Cox-Regression für die Zeit bis zum Rezidiv",
             paste("Univariable Cox-Regression auf TTR, alle Variablen.",
                   "Quelle: 02_Code/06_Cox_Analysis.R"),
             "tab:results_cox:univariat")

.alt_uni <- file.path(fig_dir, c("Tab_3_5_Univariate_Cox_RFS.tex",
                                "Tab_3_5a_Univariate_Cox_Praeop.tex",
                                "Tab_3_5b_Univariate_Cox_Postop.tex"))
.alt_uni <- .alt_uni[file.exists(.alt_uni)]
if (length(.alt_uni) > 0) { file.remove(.alt_uni) }
rm(.alt_uni)


# ── Tab 3.6 — Multivariate Cox A / B / C ──


modA_vars <- c("age_c", "sex_m", "cirrhosis",
               "lesion_multi_bin",
               "tum_d_sd", "aetio_model", "dm_bin", "nikotin_bin")
modB_vars <- c(modA_vars,
               "Quick_preop_sd",
               "albi_grade_f")
modC_vars <- c(modB_vars, "FLV_TLV_ratio_sd", "ttlvr_sd")

fitA <- tryCatch(coxph(reformulate(modA_vars, "Surv(time, event)"),
                       data = sdat),
                 error = function(e) NULL)
fitB <- tryCatch(coxph(reformulate(modB_vars, "Surv(time, event)"),
                       data = sdat),
                 error = function(e) NULL)
fitC <- tryCatch(coxph(reformulate(modC_vars, "Surv(time, event)"),
                       data = sdat),
                 error = function(e) NULL)

extract_model <- function(fit, label) {
  if (is.null(fit)) return(NULL)
  s <- summary(fit)
  ci <- s$conf.int
  pv <- s$coefficients[, "Pr(>|z|)"]
  df <- data.frame(
    Variable = label_vars(rownames(ci)),
    Key = rownames(ci),
    HR = sprintf("%.2f", ci[, "exp(coef)"]),
    `95% CI` = sprintf("(%.2f–%.2f)", ci[, "lower .95"], ci[, "upper .95"]),
    p  = ifelse(pv < 0.001, "<0.001",
                ifelse(pv > 0.999, ">0.999", sprintf("%.3f", pv))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  df$Modell <- label
  df
}

mv <- bind_rows(
  extract_model(fitA, "Modell A (Routinebefunde)"),
  extract_model(fitB, "Modell B (+ präop Labor)"),
  extract_model(fitC, "Modell C (+ präop Volumetrie)")
)

if (!is.null(mv) && nrow(mv) > 0) {
  mv_wide <- mv |>
    mutate(`HR (95% CI)` = sprintf("%s %s", HR, `95% CI`)) |>
    select(-HR, -`95% CI`) |>
    pivot_wider(names_from = Modell,
                values_from = c(`HR (95% CI)`, p),
                values_fill = "--") |>
    relocate(Variable)

  global_p_fmt <- NA_character_
  if (!is.null(fitC)) {
    ph <- tryCatch(cox.zph(fitC), error = function(e) NULL)
    if (!is.null(ph)) {
      ph_tab <- ph$table
      is_global <- rownames(ph_tab) == "GLOBAL"
      schoen <- bind_rows(lapply(rownames(ph_tab)[!is_global], function(tv) {
        p_raw <- ph_tab[tv, "p"]
        p_fmt <- if (p_raw < 0.001) "<0.001"
                 else if (p_raw > 0.999) ">0.999" else sprintf("%.3f", p_raw)
        x   <- if (tv %in% names(sdat)) sdat[[tv]] else NULL
        raw <- if (is.factor(x)) paste0(tv, levels(x)[-1]) else tv
        data.frame(Key = raw, `Schoenfeld p` = p_fmt,
                   check.names = FALSE, stringsAsFactors = FALSE)
      }))
      mv_wide <- mv_wide |>
        left_join(schoen, by = "Key") |>
        mutate(`Schoenfeld p` = ifelse(is.na(`Schoenfeld p`), "--",
                                       `Schoenfeld p`))
      if (any(is_global)) {
        global_p <- ph_tab[is_global, "p"]
        global_p_fmt <- if (global_p < 0.001) "<0.001"
                        else if (global_p > 0.999) ">0.999"
                        else sprintf("%.3f", global_p)
      }
    }
  }

  # ── Custom write: two-row header, grouped by model ──
  mod_labels <- c(
    A = "Modell A (Routinebefunde)",
    B = "Modell B (+ präop Labor)",
    C = "Modell C (+ präop Volumetrie)"
  )
  col_order <- c("Variable",
    paste0("HR (95% CI)_", mod_labels["A"]), paste0("p_", mod_labels["A"]),
    paste0("HR (95% CI)_", mod_labels["B"]), paste0("p_", mod_labels["B"]),
    paste0("HR (95% CI)_", mod_labels["C"]), paste0("p_", mod_labels["C"]),
    "Schoenfeld p", "Key"
  )
  col_order <- intersect(col_order, names(mv_wide))
  mv_wide   <- mv_wide[, col_order]
  hr_sp <- grep("^HR \\(95% CI\\)", names(mv_wide), value = TRUE)
  mv_wide <- ref_zeile(mv_wide, "männlich", "weiblich", hr_sp, "ref_sex")
  mv_wide <- ref_zeile(mv_wide, "Ätiologie: MASLD", "Ätiologie: Rest", hr_sp,
                       "ref_aetio")

  has_schoen <- "Schoenfeld p" %in% names(mv_wide)
  tab36_path <- file.path(fig_dir, "Tab_3_6_Multivariate_Cox_ABC_Landscape.tex")
  align_36 <- if (has_schoen) "lrrrrrrr" else "lrrrrrr"
  header_row1 <- paste0(
    paste(
      "",
      "\\multicolumn{2}{c}{Modell A (Routinebefunde)}",
      "\\multicolumn{2}{c}{Modell B (+ präop Labor)}",
      "\\multicolumn{2}{c}{Modell C (+ präop Volumetrie)}",
      sep = " & "
    ),
    if (has_schoen) " & " else ""
  )
  header_row2 <- paste(
    c("Variable",
      "HR (95\\% KI)", "p",
      "HR (95\\% KI)", "p",
      "HR (95\\% KI)", "p",
      if (has_schoen) "Schoenfeld p" else NULL),
    collapse = " & "
  )
  gruppen_mv <- list(
    "Präoperativ: Person und Anamnese" = c("age_c", "ref_sex", "sex_m"),
    "Präoperativ: Komorbidität und Ätiologie" = c(
        "dm_bin", "nikotin_bin", "ref_aetio", "aetio_modelMASLD",
        "aetio_modelviral", "aetio_modelethyltox"),
    "Präoperativ: Leberfunktion und Labor (POD $-$1)" = c(
        "cirrhosis", "Quick_preop_sd",
        "albi_grade_fGrade 2", "albi_grade_fGrade 3"),
    "Präoperativ: Volumetrie" = c("FLV_TLV_ratio_sd", "ttlvr_sd"),
    "Postoperativ: histopathologischer Befund" = c("tum_d_sd",
        "lesion_multi_bin")
  )
  zeig_sp   <- setdiff(names(mv_wide), "Key")
  n_spalten <- length(zeig_sp)
  body_36    <- character(0)
  gezeigt_mv <- character(0)
  for (g in names(gruppen_mv)) {
    idx <- match(gruppen_mv[[g]], mv_wide$Key)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) next
    body_36 <- c(body_36,
                 sprintf("\\multicolumn{%d}{@{}l}{\\textit{%s}} \\\\[2pt]",
                         n_spalten, g))
    for (i in idx) {
      body_36 <- c(body_36,
                   paste0(paste(tex_escape_cell(unlist(mv_wide[i, zeig_sp])),
                                collapse = " & "), " \\\\"))
    }
    body_36 <- c(body_36, "\\addlinespace")
    gezeigt_mv <- c(gezeigt_mv, mv_wide$Key[idx])
  }
  if (length(body_36) > 0 && body_36[length(body_36)] == "\\addlinespace") {
    body_36 <- body_36[-length(body_36)]
  }
  fehlend_mv <- setdiff(mv_wide$Key, gezeigt_mv)
  if (length(fehlend_mv) > 0) {
    message("[WARNUNG Tab 3.9] nicht zugeordnet: ",
            paste(fehlend_mv, collapse = ", "),
            " -- diese Zeilen fehlen in der Ausgabe. gruppen_mv ergaenzen.")
  }

  mfeld <- function(feld) vapply(list(fitA, fitB, fitC), function(f)
    if (is.null(f)) "--" else as.character(f[[feld]]), character(1))
  fuss_zeile <- function(lbl, werte) {
    paste0(paste(c(lbl, sprintf("\\multicolumn{2}{r}{%s}", werte),
                   if (has_schoen) "" else NULL), collapse = " & "), " \\\\")
  }
  body_36 <- c(body_36, "\\midrule",
               fuss_zeile("N", mfeld("n")),
               fuss_zeile("Ereignisse", mfeld("nevent")))
  if (!is.na(global_p_fmt)) {
    body_36 <- c(body_36,
                 paste0(paste(c("Gesamtmodell (PH-Test)",
                                rep("", n_spalten - 2), global_p_fmt),
                              collapse = " & "), " \\\\"))
  }
  out_36 <- c(
    paste("%", "Multivariable Cox-Modelle A/B/C auf TTR (präop bis postop-1.",
          "Volumetrie). Kontinuierliche Variablen z-standardisiert (HR pro",
          "1 SD). Letzte Spalte: PH-Annahme-Test (Schoenfeld) der jeweiligen",
          "Variable in Modell C; Fusszeile Gesamtmodell = zusammenfassender",
          "Schoenfeld-Test auf Modellebene. Faktor-Variablen (Ätiologie,",
          "ALBI-Grad) prüft der Schoenfeld-Test als gemeinsamen Term. Das",
          "Term-p steht daher identisch auf jeder Level-Zeile. N und",
          "Ereignisse sind die vollständigen Fälle des jeweiligen Modells."),
    sprintf("\\begin{tabular}{%s}", align_36),
    "\\toprule",
    paste0(header_row1, " \\\\"),
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
    paste0(header_row2, " \\\\"),
    "\\midrule",
    body_36,
    "\\bottomrule",
    "\\end{tabular}"
  )
  con_36 <- file(tab36_path, "w", encoding = "UTF-8")
  writeLines(out_36, con_36); close(con_36)
}

# ── Tab 3.6b — Goodness-of-Fit-Companion ──

sdat_common <- sdat[complete.cases(sdat[, c("time", "event", modC_vars)]), ]
fitA_cc <- tryCatch(coxph(reformulate(modA_vars, "Surv(time, event)"),
                          data = sdat_common), error = function(e) NULL)
fitB_cc <- tryCatch(coxph(reformulate(modB_vars, "Surv(time, event)"),
                          data = sdat_common), error = function(e) NULL)
fitC_cc <- tryCatch(coxph(reformulate(modC_vars, "Surv(time, event)"),
                          data = sdat_common), error = function(e) NULL)

fmt_p_compact <- function(p) {
  if (!is.finite(p)) return("--")
  if (p < 0.001) "<0.001" else if (p > 0.999) ">0.999" else sprintf("%.3f", p)
}

mfit_row <- function(fit, lbl) {
  if (is.null(fit)) return(NULL)
  s    <- summary(fit)
  cidx <- s$concordance["C"]; cse <- s$concordance["se(C)"]
  lr   <- s$logtest
  data.frame(Modell = lbl,
             N      = s$n,
             Events = s$nevent,
             `C-Index (95% CI)` = sprintf("%.3f (%.3f–%.3f)",
                                            cidx, cidx - 1.96 * cse,
                                            cidx + 1.96 * cse),
             `LR-$\\chi^2$ vs Null` = sprintf("%.2f", lr["test"]),
             `df` = sprintf("%d", as.integer(lr["df"])),
             `p (LRT)` = fmt_p_compact(lr["pvalue"]),
             check.names = FALSE, stringsAsFactors = FALSE)
}

cindex_tab <- bind_rows(
  mfit_row(fitA_cc, "Modell A (Routinebefunde)"),
  mfit_row(fitB_cc, "Modell B (+ präop Labor)"),
  mfit_row(fitC_cc, "Modell C (+ präop Volumetrie)")
)

lrt_pair <- function(fit_small, fit_large, lbl) {
  if (is.null(fit_small) || is.null(fit_large)) return(NULL)
  a <- tryCatch(anova(fit_small, fit_large), error = function(e) NULL)
  if (is.null(a)) return(NULL)
  chi <- a[2, "Chisq"]; dfv <- a[2, "Df"]; pv <- a[2, "Pr(>|Chi|)"]
  data.frame(Modell = lbl,
             N      = "",
             Events = "",
             `C-Index (95% CI)` = "",
             `LR-$\\chi^2$ vs Null` = sprintf("%.2f", chi),
             `df` = sprintf("%d", as.integer(dfv)),
             `p (LRT)` = fmt_p_compact(pv),
             check.names = FALSE, stringsAsFactors = FALSE)
}

lrt_tab <- bind_rows(
  lrt_pair(fitA_cc, fitB_cc, "$\\Delta$ B vs.\\ A (präop-Labor-Block)"),
  lrt_pair(fitB_cc, fitC_cc, "$\\Delta$ C vs.\\ B (präop-Volumetrie-Block)")
)

if (nrow(cindex_tab) > 0) {
  cat("[06_Cox_Analysis] C-Index/LRT-Kennzahlen A-C (vgl. Tab_GOF_Cox_A_D):\n")
  print(cindex_tab, row.names = FALSE)
  if (nrow(lrt_tab) > 0) print(lrt_tab, row.names = FALSE)
}


# ── Tab 3.7 (Schoenfeld) ──


# ── Abb 3.9 — Forest plot Modell C ──

if (!is.null(fitC)) {
  td <- tryCatch(broom::tidy(fitC, conf.int = TRUE, exponentiate = TRUE),
                 error = function(e) NULL)
  if (!is.null(td)) {
    td <- td |> filter(!is.na(estimate), !is.na(conf.low), !is.na(conf.high)) |>
      mutate(Variable = label_vars(term),
             sig = if_else(p.value < 0.05, "sig", "ns"))

  td <- td |>
    mutate(
      lbl_hr = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
      sig_lbl = if_else(p.value < 0.05, "p < 0,05", "p ≥ 0,05"),
      sig_lbl = factor(sig_lbl, levels = c("p < 0,05", "p ≥ 0,05"))
    )

  p_fp <- ggplot(td, aes(x = estimate, y = reorder(Variable, estimate),
                         colour = sig_lbl)) +
    geom_point(size = 2.8) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  orientation = "y", width = 0.22, linewidth = 0.55) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_label(aes(label = lbl_hr), hjust = -0.12, vjust = 0, nudge_y = 0.28,
               size = 3.0, colour = "grey20", fill = "white", linewidth = 0,
               label.padding = unit(0.08, "lines"), show.legend = FALSE) +
    scale_x_log10(breaks = c(0.5, 1, 2, 5, 10),
                  expand = expansion(mult = c(0.05, 0.45))) +
    scale_colour_manual(values = c("p < 0,05" = "#00441B",
                                   "p ≥ 0,05" = "grey40"),
                        name = NULL) +
    labs(x = "Hazard Ratio (log-Skala)",
         y = NULL) +
    theme_minimal(base_family = "sans") +
    theme(legend.position = "bottom")

  ggsave(file.path(fig_dir, "Abb_3_9_Forest_ModelC.png"), plot = p_fp,
         width = 10, height = 6, dpi = 300, bg = "white")
  }
}


# ── Abb 3.10 — KM-Strata (4-Panel) mit 95%-CI-Bändern ──

make_strata <- function(x, label) {
  med <- median(x, na.rm = TRUE)
  factor(if_else(x > med, sprintf("%s > Median", label),
                 sprintf("%s ≤ Median", label)),
         levels = c(sprintf("%s ≤ Median", label),
                    sprintf("%s > Median", label)))
}

sdat <- sdat |>
  mutate(
    strat_FLV_TLV = make_strata(FLV_TLV_ratio, "FLV/TLV"),
    strat_TTLVR   = make_strata(TTLVR_preop, "TTLVR")
  )

LM_DAYS <- 90
lm_dat <- sdat |>
  filter(!is.na(time), time > LM_DAYS,
         !is.na(POD_first_postop_volumetry),
         POD_first_postop_volumetry <= LM_DAYS) |>
  mutate(
    res_time    = time - LM_DAYS,
    strat_TLVfp = make_strata(TLV_first_postop, "TLV 1.postop"),
    strat_dTLVf = make_strata(d_TLV_rel_FLV_base_first_postop, "ΔTLV vs FLV")
  )
message(sprintf("[06] KM-Landmark-Kohorte (POD %d): n=%d von %d (Messung <= LM und at risk)",
                LM_DAYS, nrow(lm_dat), nrow(sdat)))

km_ci_band <- function(sf, col) {
  tt <- c(0, sf$time); lo <- c(1, sf$lower); up <- c(1, sf$upper)
  keep <- !is.na(lo) & !is.na(up); tt <- tt[keep]; lo <- lo[keep]; up <- up[keep]
  n <- length(tt); if (n < 2) return(invisible())
  xs <- ylo <- yup <- numeric(0)
  for (i in 1:(n - 1)) {
    xs  <- c(xs, tt[i], tt[i + 1]); ylo <- c(ylo, lo[i], lo[i]); yup <- c(yup, up[i], up[i])
  }
  polygon(c(xs, rev(xs)), c(yup, rev(ylo)), col = col, border = NA)
}

km_panel <- function(dat, strat_var, title, time_var = "time",
                     xlab = "Jahre nach OP") {
  ok <- !is.na(dat[[strat_var]]) & !is.na(dat[[time_var]])
  g  <- droplevels(dat[[strat_var]][ok])
  tt <- dat[[time_var]][ok] / 365.25; ee <- dat$event[ok]
  lr <- tryCatch(survdiff(Surv(tt, ee) ~ g), error = function(e) NULL)
  p  <- if (is.null(lr)) NA else 1 - pchisq(lr$chisq, length(lr$n) - 1)
  message(sprintf("[06] KM-Panel %-34s n=%d  Log-Rank p=%s", title, sum(ok),
                  if (is.na(p)) "NA" else sprintf("%.4f", p)))
  levs <- levels(g); cols <- c("#5E3C99", "#E66101")
  fits <- lapply(levs, function(L) survfit(Surv(tt[g == L], ee[g == L]) ~ 1))
  plot(NA, xlim = c(0, max(tt)), ylim = c(0, 1),
       xlab = xlab, ylab = "Patient*innen ohne Rezidiv", main = title, cex.main = 1.0)
  for (k in seq_along(levs)) km_ci_band(fits[[k]], adjustcolor(cols[k], alpha.f = 0.18))
  for (k in seq_along(levs)) lines(fits[[k]], col = cols[k], lwd = 2.4,
                                   conf.int = FALSE, mark.time = TRUE)
  legend("bottomleft", legend = levs, col = cols, lwd = 2.4, bty = "n", cex = 0.8)
  p_lab <- if (is.na(p)) "Log-Rank p = NA" else if (p < 0.001) "Log-Rank p < 0.001"
           else if (p > 0.999) "Log-Rank p > 0.999" else sprintf("Log-Rank p = %.3f", p)
  text(x = max(tt) * 0.98, y = 0.97, labels = p_lab, adj = c(1, 1), cex = 0.85)
}

png(file.path(fig_dir, "Abb_3_10_KM_Strata_4Panel.png"),
    width = 1400, height = 1000, res = 130)
op <- par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 2), bg = "white")
km_panel(sdat,   "strat_FLV_TLV", "FLV/TLV-Ratio (präop)")
km_panel(sdat,   "strat_TTLVR",   "TTLVR (präop)")
km_panel(lm_dat, "strat_TLVfp",
         sprintf("TLV 1. postop (Landmark POD %d)", LM_DAYS),
         time_var = "res_time",
         xlab = sprintf("Jahre ab Landmark POD %d", LM_DAYS))
km_panel(lm_dat, "strat_dTLVf",
         sprintf("Δ TLV vs. FLV (Landmark POD %d)", LM_DAYS),
         time_var = "res_time",
         xlab = sprintf("Jahre ab Landmark POD %d", LM_DAYS))
par(op); dev.off()


# ── Tab RCS — Linearitaetspruefung per Restricted Cubic Splines ──
rcs_vars <- c("age_c", "Quick_preop_sd", "FLV_TLV_ratio_sd", "ttlvr_sd",
              "tum_d_sd")
rcs_rows <- lapply(rcs_vars, function(v) {
  others <- setdiff(modC_vars, v)
  dat_cc <- sdat[complete.cases(sdat[, c("time", "event", modC_vars)]), ]
  f_lin <- tryCatch(coxph(reformulate(c(v, others), "Surv(time, event)"),
                          data = dat_cc), error = function(e) NULL)
  f_ns  <- tryCatch(coxph(reformulate(c(sprintf("splines::ns(%s, df = 3)", v),
                                        others), "Surv(time, event)"),
                          data = dat_cc), error = function(e) NULL)
  if (is.null(f_lin) || is.null(f_ns)) return(NULL)
  a <- tryCatch(anova(f_lin, f_ns), error = function(e) NULL)
  if (is.null(a)) return(NULL)
  data.frame(Variable = label_vars(v),
             `LR-$\\chi^2$` = sprintf("%.2f", a[2, "Chisq"]),
             df = as.integer(a[2, "Df"]),
             `p (nichtlinear)` = fmt_p_compact(a[2, "Pr(>|Chi|)"]),
             check.names = FALSE, stringsAsFactors = FALSE)
})
rcs_tab <- bind_rows(rcs_rows)
if (nrow(rcs_tab) > 0) {
  df_werte <- unique(rcs_tab$df)
  message(sprintf("[06] RCS: zusaetzliche Freiheitsgrade je Zeile: %s",
                  paste(df_werte, collapse = ", ")))
  if (!identical(as.integer(df_werte), 2L)) {
    message("[WARNUNG Tab RCS] Freiheitsgrade nicht einheitlich 2 -- ",
            "Legende und Prosa von Tab 3.13 nennen 2 und muessen nachgezogen werden.")
  }
  rcs_tab$df <- NULL
  rcs_path <- file.path(fig_dir, "Tab_RCS_Linearity.tex")
  out_rcs <- c(
    paste("%", "Linearitaetspruefung: je stetiger Variable Natural Spline",
          "(df = 3) statt linearem Term in Modell C, LRT Spline vs. linear",
          "mit je 2 zusaetzlichen Freiheitsgraden auf der",
          "Complete-Case-Subkohorte. Kleines p = Abweichung von der",
          "Log-Linearitaet."),
    "\\begin{tabular}{lrr}", "\\toprule",
    "Variable & LR-$\\chi^2$ & p (nichtlinear) \\\\", "\\midrule",
    apply(rcs_tab, 1, function(r) paste0(paste(
      c(tex_escape_cell(r[[1]]), as.character(r[-1])), collapse = " & "),
      " \\\\")),
    "\\bottomrule", "\\end{tabular}")
  con_rcs <- file(rcs_path, "w", encoding = "UTF-8")
  writeLines(unlist(out_rcs), con_rcs); close(con_rcs)
}


# ── Abb + Tab Interaktionen — drei vorab benannte Effektmodifikatoren ──
REG_VAR <- "d_TLV_rel_FLV_sd"
int_spec <- list(
  list(mod = "aetio_model", lbl = "Ätiologie",             type = "factor"),
  list(mod = "cirrhosis",   lbl = "Leberzirrhose",         type = "bin",
       lv0 = "ohne Leberzirrhose", lv1 = "mit Leberzirrhose"),
  list(mod = "D_major",     lbl = "Resektionsausmaß",      type = "bin",
       lv0 = "Minor-Hepatektomie", lv1 = "Major-Hepatektomie")
)
int_forest <- list(); int_nevent <- NA_integer_
for (sp in int_spec) {
  m <- sp$mod
  if (!m %in% names(sdat)) next
  base_vars <- unique(c(modA_vars, m, REG_VAR))
  f0 <- tryCatch(coxph(reformulate(base_vars, "Surv(time, event)"),
                       data = sdat), error = function(e) NULL)
  f1 <- tryCatch(coxph(reformulate(c(base_vars,
                                     sprintf("%s:%s", REG_VAR, m)),
                                   "Surv(time, event)"),
                       data = sdat), error = function(e) NULL)
  if (is.null(f0) || is.null(f1)) next
  a  <- tryCatch(anova(f0, f1), error = function(e) NULL)
  p_int  <- if (is.null(a)) NA else a[2, "Pr(>|Chi|)"]
  df_int <- if (is.null(a)) NA else as.integer(a[2, "Df"])
  mf1 <- model.frame(f1)
  int_nevent <- f1$nevent
  status_f1 <- as.matrix(mf1[[1]])[, "status"]
  co <- coef(f1); V <- vcov(f1)
  if (sp$type == "bin") {
    i_int <- which(grepl(":", names(co), fixed = TRUE) &
                     grepl(REG_VAR, names(co), fixed = TRUE))
    if (length(i_int) == 1) {
      se_int <- sqrt(V[i_int, i_int])
      cat(sprintf(paste0("[06 interaktion] %s: Verhaeltnis der Untergruppen-HR %.2f, ",
                         "SE(log) %.3f, Nachweiswahrscheinlichkeit %.0f %%, ",
                         "mit 80 %% nachweisbar ab Verhaeltnis %.2f\n"),
                  sp$lbl, exp(abs(co[i_int])), se_int,
                  100 * pnorm(abs(co[i_int]) / se_int - qnorm(0.975)),
                  exp((qnorm(0.975) + qnorm(0.8)) * se_int)))
    }
  }
  i_reg <- which(names(co) == REG_VAR)
  if (length(i_reg) != 1) next
  if (sp$type == "bin") {
    grp_list <- list(
      list(name = sp$lv0, i_int = integer(0), mask = mf1[[m]] == 0),
      list(name = sp$lv1,
           i_int = which(grepl(REG_VAR, names(co), fixed = TRUE) &
                           grepl(":", names(co), fixed = TRUE) &
                           grepl(m, names(co), fixed = TRUE)),
           mask = mf1[[m]] == 1))
  } else {
    fac  <- droplevels(as.factor(mf1[[m]]))
    levs <- levels(fac)
    grp_list <- lapply(seq_along(levs), function(j) {
      L <- levs[j]
      ii <- if (j == 1) integer(0) else
        which(grepl(REG_VAR, names(co), fixed = TRUE) &
                grepl(":", names(co), fixed = TRUE) &
                grepl(paste0(m, L), names(co), fixed = TRUE))
      list(name = if (L == "ethyltox") "ethyltoxisch" else L,
           i_int = ii, mask = fac == L)
    })
    grp_list <- Filter(function(g) length(g$i_int) <= 1, grp_list)
  }
  for (g in grp_list) {
    idx <- c(i_reg, g$i_int)
    cvec <- rep(1, length(idx))
    b  <- sum(co[idx])
    se <- sqrt(as.numeric(t(cvec) %*% V[idx, idx, drop = FALSE] %*% cvec))
    int_forest[[length(int_forest) + 1]] <- data.frame(
      Modifikator = sp$lbl, Gruppe = g$name,
      n = sum(g$mask, na.rm = TRUE),
      ev = sum(status_f1[g$mask], na.rm = TRUE),
      hr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se),
      p_int = p_int, df_int = df_int, stringsAsFactors = FALSE)
  }
}
if (length(int_forest) > 0) {
  f_alle <- tryCatch(coxph(reformulate(unique(c(modA_vars, REG_VAR)),
                                       "Surv(time, event)"), data = sdat),
                     error = function(e) NULL)
  zeile_alle <- NULL
  if (!is.null(f_alle)) {
    ci_alle <- summary(f_alle)$conf.int
    zeile_alle <- data.frame(
      Modifikator = "Alle Patient*innen", Gruppe = "",
      n = f_alle$n, ev = f_alle$nevent,
      hr = ci_alle[REG_VAR, "exp(coef)"], lo = ci_alle[REG_VAR, "lower .95"],
      hi = ci_alle[REG_VAR, "upper .95"], p_int = NA_real_,
      df_int = NA_integer_, stringsAsFactors = FALSE)
  }
  sd_reg <- sd(sdat$d_TLV_rel_FLV_base_first_postop, na.rm = TRUE)
  message(sprintf("[06] Interaktionen: 1 SD Delta TLV vs. FLV = %.1f Prozentpunkte",
                  sd_reg))
  if (round(sd_reg) != 45) {
    message("[WARNUNG Tab 3.14] 1 SD ist nicht mehr 45 Prozentpunkte -- ",
            "Legende und Prosa nachziehen.")
  }
  int_tab <- bind_rows(zeile_alle, bind_rows(int_forest)) |>
    group_by(Modifikator) |>
    mutate(`p (Interaktion, LRT)` = ifelse(
      row_number() == 1 & !is.na(p_int),
      sprintf("%s (%d df)",
              vapply(p_int, fmt_p_compact, character(1)), df_int), "")) |>
    ungroup() |>
    transmute(Effektmodifikator = Modifikator, Subgruppe = Gruppe,
              `HR Regeneration (95% KI)` = sprintf("%.2f (%.2f–%.2f)",
                                                   hr, lo, hi),
              n = sprintf("%d", n), Rezidive = sprintf("%d", ev),
              `p (Interaktion, LRT)`)
  write_table_tex(
    int_tab, file.path(fig_dir, "Tab_Interactions_ModelA.tex"),
    caption = paste("Vorab benannte Effektmodifikationen der volumetrischen",
                    "Regeneration (Delta TLV vs. FLV, pro SD) auf",
                    "Modell A. HR der Regeneration je Subgruppe via",
                    "Linearkombination; p aus dem LRT der Interaktionsterme",
                    "(Aetiologie: Omnibus ueber 3 df). Oberste Zeile: HR der",
                    "Regeneration ohne Interaktion. Power bei",
                    sprintf("%s Events (Complete Cases): nachweisbar erst ab",
                            ifelse(is.na(int_nevent), "--",
                                   as.character(int_nevent))),
                    "Subgruppen-HR-Verhaeltnis ~2,6 (binaer) bzw. ~3,1",
                    "(Omnibus 3 df)."),
    align = "llrrrr")
  fd <- bind_rows(int_forest) |>
    mutate(y = factor(Gruppe, levels = rev(unique(Gruppe))),
           block = sprintf("%s, Interaktions-p = %s", Modifikator,
                           vapply(p_int, fmt_p_compact, character(1))),
           block = factor(block, levels = unique(block)))
  p_int_fig <- ggplot(fd, aes(x = hr, y = y)) +
    geom_point(size = 2.6, colour = "grey30") +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.2,
                  linewidth = 0.55, colour = "grey30") +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_label(aes(label = sprintf("%.2f (%.2f–%.2f), n = %d", hr, lo, hi, n)),
               hjust = -0.12, vjust = 0, nudge_y = 0.38,
               size = 3.0, colour = "grey20", fill = "white", linewidth = 0,
               label.padding = unit(0.08, "lines")) +
    scale_x_log10(breaks = c(0.5, 1, 2, 3, 5),
                  expand = expansion(mult = c(0.05, 0.55))) +
    scale_y_discrete(expand = expansion(add = c(0.5, 0.9))) +
    facet_wrap(~ block, ncol = 1, scales = "free_y") +
    labs(x = "Hazard Ratio (log-Skala)", y = NULL) +
    theme_minimal(base_family = "sans") +
    theme(strip.text = element_text(size = 9, hjust = 0))
  ggsave(file.path(fig_dir, "Abb_Interactions_Forest.png"), plot = p_int_fig,
         width = 9, height = 7.5, dpi = 300, bg = "white")
}


message("Stage 6 done. Wrote: Tab_3_5 (univariate Cox), Tab_3_6 (multivariate A/B/C), ",
        "Tab_RCS_Linearity, Tab/Abb Interactions, ",
        "Abb_3_9 (Forest Modell C), Abb_3_10 (KM 4-Panel, Panel 3/4 Landmark POD 90).")
