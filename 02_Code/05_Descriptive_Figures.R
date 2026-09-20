# 05_Descriptive_Figures.R -- Kohortenbeschreibung, Fehlendprofil, Ueberlebenskurven,
# Volumetrie- und Laborverlaeufe. Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx,
# data_dynamic_surv.xlsx; schreibt Tab_*.tex und Abb_*.png nach 03_Tables_Figures.

pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "ggplot2",
          "wesanderson", "scales", "survival", "tibble")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")

library(readxl); library(dplyr); library(tidyr); library(stringr); library(tibble)
library(ggplot2); library(wesanderson); library(scales); library(survival)

var_label <- c(
  Alter                          = "Alter (Jahre)",
  BMI                            = "BMI (kg/m²)",
  KGkg                           = "Körpergewicht (kg)",
  m                              = "Körpergröße (m)",
  mwd                            = "Geschlecht (m/w)",
  slope_Quick_POD1_7             = "Quick-Slope POD 1-7 (%/Tag)",
  slope_GOT_POD1_7               = "GOT-Slope POD 1-7 (U/L/Tag)",
  slope_GPT_POD1_7               = "GPT-Slope POD 1-7 (U/L/Tag)",
  slope_Albumin_POD1_7           = "Albumin-Slope POD 1-7 (g/L/Tag)",
  slope_CRP_POD1_7               = "CRP-Slope POD 1-7 (mg/L/Tag)",
  slope_Leukos_POD1_7            = "Leukozyten-Slope POD 1-7 (/nL/Tag)",
  slope_Thrombos_POD1_7          = "Thrombozyten-Slope POD 1-7 (/nL/Tag)",
  Leukos_preop                   = "Leukozyten präop (/nL)",
  CRP_preop                      = "CRP präop (mg/L)",
  Thrombos_preop                 = "Thrombozyten präop (/nL)",
  FU_duration_days               = "FU-Dauer (Tage)",
  ChildPugh_Punkte               = "Child-Pugh Punkte",
  ChildPugh_Stadium              = "Child-Pugh Stadium",
  Tumordurchmesser               = "Tumordurchmesser (cm)",
  ND_count                       = "Anzahl Nebendiagnosen",
  ND_Nikotin                     = "Nikotinkonsum (binär)",
  ND_Nikotin_py                  = "Pack-Years (bei Nikotinkonsum)",
  Leberzirrhose                  = "Leberzirrhose",
  ClavienDindo                   = "Clavien-Dindo (0-5)",
  DM                             = "Diabetes mellitus",
  Diagnose                       = "Diagnose",
  Äthiologie                     = "Ätiologie HCC",
  Vorbehandlung                  = "Vorbehandlung (Art bzw. keine)",
  T                              = "TNM T-Stadium",
  N                              = "TNM N-Stadium",
  M                              = "TNM M-Stadium",
  Rezidivtumor                   = "Vor-Rezidiv vor OP",
  FUStatus                       = "FU-Status (NED/AWD/...)",
  Symptom_cat                    = "Symptom-Klasse",
  OP_Name                        = "OP-Bezeichnung",
  OP_num                         = "OP-Segmente (Couinaud)",
  OP_laparoskop                  = "Zugangsweg (offen/laparoskopisch)",
  OP_CCE                         = "Zeitgleiche Cholezystektomie (ja/nein)",
  D_OP_seg_1                     = "OP betraf Segment 1",
  D_OP_seg_4                     = "OP betraf Segment 4",
  D_OP_seg_2_3                   = "OP betraf Segmente 2 & 3",
  FLV                            = "FLV präop (cm³)",
  TLV_preop                      = "TLV präop (cm³)",
  TLV_first_postop               = "TLV 1. postop (cm³)",
  d_TLV_rel_first_postop         = "Δ TLV vs. 1. Volumetrie (%, 1. postop)",
  d_TLV_rel_FLV_base_first_postop = "Δ TLV vs. FLV (%, 1. postop)",
  POD_first_postop_volumetry     = "POD der 1. postop Volumetrie",
  n_postop_volumetrien           = "Anzahl postop Volumetrien",
  date_Geburt                    = "Geburtsdatum",
  date_OP                        = "OP-Datum",
  date_FU                        = "Datum letztes FU",
  date_rezidiv_first             = "Datum 1. Rezidiv",
  Pseudonym                      = "Pseudonym (ID)",
  Studienausschluss              = "Studienausschluss",
  D_rezidiv                      = "Rezidiv-Flag (patient)",
  n_rezidiv                      = "Anzahl Rezidive",
  time_to_first_rezidiv_days     = "Tage OP bis 1. Rezidiv",
  D_rezidiv_cat_first            = "Erstes Rezidiv: Lokalisation",
  rezidiv_therapie_cat_first     = "Erstes Rezidiv: Therapie-Kategorie",
  n_rezidiv_therapie             = "Anzahl Therapie-Ereignisse",
  ALBI_score                     = "ALBI-Score",
  ALBI_grade                     = "ALBI-Grad",
  Albumin_preop                  = "Albumin präop (g/L)",
  deritis_preop                  = "De-Ritis-Quotient präop",
  astalb_preop                   = "AST/Albumin-Ratio präop",
  B_Symptomatik                  = "B-Symptomatik",
  TLV_growth_rate                = "TLV-Log-Wachstumsrate (pro Tag)",
  TLV_nam_91_180                 = "TLV im Fenster POD 91-180 (cm³)",
  POD_nam_91_180                 = "POD der Volumetrie im Fenster 91-180",
  LVR_nam                        = "LVR-Index nach Nam (TLV 91-180 / FLV)"
)
label_var <- function(v) {
  out <- ifelse(v %in% names(var_label), var_label[v], NA_character_)
  out <- ifelse(is.na(out) & grepl("^D_ND_cat_", v),
                paste0("Komorbidität: ", sub("^D_ND_cat_", "", v)), out)
  out <- ifelse(is.na(out) & grepl("^D_Sym_", v),
                paste0("Symptom: ", gsub("_", " ", sub("^D_Sym_", "", v))), out)
  out <- ifelse(is.na(out), v, out)
  out
}

static  <- read_excel("./01_Daten/data_static.xlsx")
dynamic <- read_excel("./01_Daten/data_dynamic.xlsx")

analysis_pseu <- static |>
  filter(is.na(Rezidivtumor) | str_to_lower(Rezidivtumor) != "ja") |>
  pull(Pseudonym)
static_an  <- static  |> filter(Pseudonym %in% analysis_pseu)
dynamic_an <- dynamic |> filter(Pseudonym %in% analysis_pseu)

fig_dir <- "./03_Tables_Figures"

# ── Diagnostik: Patient:innen mit fehlender Volumetrie ──
diag_vol <- list(
  FLV_NA              = static_an$Pseudonym[is.na(static_an$FLV)],
  TLV_preop_NA        = static_an$Pseudonym[is.na(static_an$TLV_preop)],
  TLV_first_postop_NA = static_an$Pseudonym[is.na(static_an$TLV_first_postop)]
)
cat("\n── Volumetrie-Missing-Diagnose (Analyse-Kohorte n=",
    nrow(static_an), ") ──\n", sep = "")
for (nm in names(diag_vol)) {
  pseu <- diag_vol[[nm]]
  cat(sprintf("  %-20s: %d Patient:in(nen) mit NA",
              nm, length(pseu)))
  if (length(pseu) > 0)
    cat(" — Pseudonyme:", paste(pseu, collapse = ", "))
  cat("\n")
}
cat("──────────────────────────────────────────────────────────\n\n")

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

col_blue <- "#3B6FAB"
col_red  <- "#C7261B"


# ── Tab 2.1 — Datensatz-Übersicht (Variablen × Block, Vollständigkeit) ──

n_total <- nrow(static_an)

dataset_overview <- tribble(
  ~Block, ~Variablen, ~Phase, ~Vars,
  "Demographie",        "Pseudonym-ID, Alter, Körpergröße, Körpergewicht, BMI",    "präop",
    c("Alter","m","BMI","KGkg"),
  "Komorbiditäten",     "Nebendiagnose-Kategorien (13), Nikotinstatus, Anzahl Nebendiagnosen", "präop",
    c("ND_Nikotin","ND_count"),
  "Leberfunktion",      "Leberzirrhose, Child-Pugh-Punkte und -Stadium, Bilirubin präoperativ", "präop",
    c("Leberzirrhose","ChildPugh_Punkte","ChildPugh_Stadium"),
  "Tumor / Staging",    "T-, N-, M-Stadium, Tumordurchmesser, Diagnose, Ätiologie", "präop",
    c("T","N","M","Tumordurchmesser","Diagnose","Äthiologie"),
  "OP-Charakteristika", "OP-Bezeichnung, Segmentzahl, laparoskopischer Zugang, Cholezystektomie, resezierte Segmente", "präop",
    c("OP_Name","OP_num","OP_laparoskop","OP_CCE"),
  "Klinisches Symptom", "Symptomstatus (symptomatisch / Zufallsbefund / Überwachung)", "präop",
    c("Symptom_cat"),
  "Volumetrie präop",   "FLV, TLV präoperativ, Tumorvolumen, Leberdichte, Milzdichte", "präop",
    c("FLV","TLV_preop"),
  "Volumetrie postop",  "TLV im Verlauf, relative TLV-Änderung, Regeneration gegenüber FLV-Basis", "postop",
    c("TLV_first_postop","d_TLV_rel_first_postop","d_TLV_rel_FLV_base_first_postop"),
  "Komplikationen",     "Clavien-Dindo-Grad",                                      "postop",
    c("ClavienDindo"),
  "Follow-up / Event",  "FU-Status, FU-Dauer, Rezidivstatus, Rezidivanzahl",       "postop",
    c("FUStatus","FU_duration_days","D_rezidiv","n_rezidiv"),
  "Rezidiv-Detail",     "Rezidivlokalisation (erste), Rezidivtherapie (erste)",    "postop",
    c("D_rezidiv_cat_first","rezidiv_therapie_cat_first")
)

completeness <- function(varnames, df = static_an) {
  v <- intersect(varnames, names(df))
  if (length(v) == 0) return(NA_real_)
  vals <- df[, v, drop = FALSE]
  mean(vapply(vals, function(col) mean(!is.na(col)), numeric(1))) * 100
}

if (FALSE) {
dataset_overview_out <- dataset_overview |>
  mutate(`Vollständigkeit %` = round(vapply(Vars, completeness, numeric(1)), 1)) |>
  select(Block, Phase, Variablen, `Vollständigkeit %`)

write_table_tex(
  dataset_overview_out,
  file.path(fig_dir, "Tab_2_1_Datensatz_Uebersicht_Landscape.tex"),
  caption = sprintf(paste("Datensatz-Übersicht (Analyse-Kohorte n=%d Patient*innen;",
                          "Vollständigkeit gemittelt über die Variablen des Blocks).",
                          "Technische Variablennamen im Variablenlexikon (Anhang A1)."),
                    n_total),
  align = "lllr"
)
}


# ── Abb 2.2 — Datenverfügbarkeits-Heatmap (postoperativer Zeitstrahl) ──

avail_bins <- dynamic |>
  filter(!is.na(lab_date_POD), lab_date_POD > -30) |>
  mutate(
    bin = case_when(
      lab_date_POD <= 0   ~ "präop",
      lab_date_POD <= 7   ~ "POD 1-7",
      lab_date_POD <= 30  ~ "POD 8-30",
      lab_date_POD <= 90  ~ "31-90",
      lab_date_POD <= 180 ~ "91-180",
      lab_date_POD <= 365 ~ "181-365",
      lab_date_POD <= 730 ~ "1-2 J",
      TRUE                ~ "> 2 J"
    ),
    bin = factor(bin, levels = c("präop","POD 1-7","POD 8-30","31-90",
                                 "91-180","181-365","1-2 J","> 2 J"))
  ) |>
  group_by(Pseudonym, bin) |>
  summarise(n_obs = n(), .groups = "drop") |>
  complete(Pseudonym, bin, fill = list(n_obs = 0L)) |>
  left_join(static |> select(Pseudonym, D_rezidiv), by = "Pseudonym") |>
  mutate(Rezidiv_Status = factor(if_else(D_rezidiv == 1, "Rezidiv", "Kein Rezidiv"),
                                 levels = c("Rezidiv", "Kein Rezidiv")))

patient_order <- avail_bins |>
  group_by(Pseudonym, Rezidiv_Status) |>
  summarise(total = sum(n_obs), .groups = "drop") |>
  arrange(Rezidiv_Status, desc(total)) |>
  pull(Pseudonym)
avail_bins <- avail_bins |> mutate(Pseudonym = factor(Pseudonym, levels = patient_order))

p_avail <- ggplot(avail_bins, aes(x = bin, y = Pseudonym, fill = n_obs)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", trans = "sqrt",
                       name = "Beobachtungen") +
  facet_grid(Rezidiv_Status ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Zeitfenster relativ zur OP",
       y = NULL) +
  theme_minimal(base_family = "sans") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"))

if (FALSE) ggsave(file.path(fig_dir, "Abb_2_2_Datenverfuegbarkeit.png"), plot = p_avail,
       width = 10, height = 8, dpi = 300, bg = "white")


# ── Tab 3.1 — Patientencharakteristika gesamt + nach Rezidiv ──

fmt_n_pct <- function(n, denom) sprintf("%d (%.1f%%)", n, 100*n/denom)
fmt_median_iqr <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x)); x <- x[!is.na(x)]
  if (length(x) == 0) return("--")
  fs <- sprintf("%%.%df (%%.%df–%%.%df)", digits, digits, digits)
  s <- sprintf(fs, median(x), quantile(x, .25), quantile(x, .75))
  gsub("(?<![0-9.])-0(\\.0+)?(?![0-9.])", "0\\1", s, perl = TRUE)
}
nolig_dash <- function(x) gsub("–-", "\\,–\\,-", x, fixed = TRUE)
fmt_mean_sd <- function(x) {
  x <- suppressWarnings(as.numeric(x)); x <- x[!is.na(x)]
  if (length(x) == 0) return("--")
  sprintf("%.1f ± %.1f", mean(x), sd(x))
}
p_fmt <- function(p) {
  if (!is.finite(p)) return("--")
  if (p < 0.001) return("<0.001")
  if (p > 0.999) return(">0.999")
  sprintf("%.3f", p)
}
pval_num <- function(x, group) {
  x <- suppressWarnings(as.numeric(x)); g <- as.factor(group)
  ok <- !is.na(x) & !is.na(g)
  if (sum(ok) < 5 || length(unique(g[ok])) < 2) return("--")
  sw <- tryCatch(shapiro.test(x[ok])$p.value, error = function(e) 1)
  if (sw > 0.05) tryCatch(p_fmt(t.test(x[ok] ~ g[ok])$p.value),
                          error = function(e) "--")
  else tryCatch(p_fmt(wilcox.test(x[ok] ~ g[ok], exact = FALSE)$p.value),
                error = function(e) "--")
}
pval_cat <- function(x, group) {
  ok <- !is.na(x) & !is.na(group)
  if (sum(ok) < 5) return("--")
  tab <- table(x[ok], group[ok])
  if (any(dim(tab) < 2)) return("--")
  tryCatch(p_fmt(suppressWarnings(chisq.test(tab))$p.value),
           error = function(e) "--")
}

g <- static_an$D_rezidiv
s <- static_an

row_num <- function(lbl, vec, gruppe, digits = 1) {
  data.frame(
    Variable = lbl, Gruppe = gruppe,
    Gesamt        = fmt_median_iqr(vec, digits),
    `Rezidiv: ja`   = fmt_median_iqr(vec[g == 1], digits),
    `Rezidiv: nein` = fmt_median_iqr(vec[g == 0], digits),
    p             = pval_num(vec, g),
    `Missing %`   = sprintf("%.1f%%",
                            100*mean(is.na(suppressWarnings(as.numeric(vec))))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
row_cat_level <- function(lbl, vec, lvl, gruppe) {
  vec <- as.character(vec); flag <- !is.na(vec) & vec == lvl
  data.frame(
    Variable = lbl, Gruppe = gruppe,
    Gesamt          = fmt_n_pct(sum(flag), length(flag)),
    `Rezidiv: ja`   = fmt_n_pct(sum(flag & g==1), sum(g==1)),
    `Rezidiv: nein` = fmt_n_pct(sum(flag & g==0), sum(g==0)),
    p = pval_cat(vec, g),
    `Missing %` = sprintf("%.1f%%", 100*mean(is.na(vec))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

tab1 <- bind_rows(
  row_num("Alter (Jahre)",                 s$Alter,            "Person und Anamnese"),
  row_num("BMI (kg/m²)",                   s$BMI,              "Person und Anamnese"),
  row_num("Körpergewicht (kg)",            s$KGkg,             "Person und Anamnese"),
  row_num("Körpergröße (m)",               s$m,                "Person und Anamnese"),
  row_cat_level("weiblich", tolower(as.character(s$mwd)), "w", "Person und Anamnese"),
  row_cat_level("männlich", tolower(as.character(s$mwd)), "m", "Person und Anamnese"),
  row_num("Nebendiagnosen (Anzahl)",         s$ND_count,         "Komorbidität und Ätiologie"),
  row_cat_level("Diabetes mellitus",       s$DM, "ja",         "Komorbidität und Ätiologie"),
  row_cat_level("Nikotinkonsum",
                as.character(s$ND_Nikotin), "1",               "Komorbidität und Ätiologie"),
  row_cat_level("≥1 Komorbidität (beliebige Kateg.)",
                if_else(rowSums(s[, grep("^D_ND_cat_", names(s)), drop = FALSE],
                                na.rm = TRUE) > 0, "1", "0"),
                "1", "Komorbidität und Ätiologie"),
  row_cat_level("Hepatische Komorbidität",
                as.character(s$D_ND_cat_hepatic), "1",         "Komorbidität und Ätiologie"),
  row_cat_level("Kardiovaskuläre Komorbidität",
                as.character(s$D_ND_cat_cardiovascular), "1", "Komorbidität und Ätiologie"),
  row_cat_level("Metabolische Komorbidität",
                as.character(s$D_ND_cat_metabolic), "1", "Komorbidität und Ätiologie"),
  row_cat_level("Endokrine Komorbidität",
                as.character(s$D_ND_cat_endokrin), "1", "Komorbidität und Ätiologie"),
  row_cat_level("Substanzkonsum",
                as.character(s$D_ND_cat_substance), "1", "Komorbidität und Ätiologie"),
  row_cat_level("HBV",          s$äthiologie_cat, "HBV",        "Komorbidität und Ätiologie"),
  row_cat_level("HCV",          s$äthiologie_cat, "HCV",        "Komorbidität und Ätiologie"),
  row_cat_level("Ethyltoxisch", s$äthiologie_cat, "Ethyltox",   "Komorbidität und Ätiologie"),
  row_cat_level("MASLD",   s$äthiologie_cat, "MASLD", "Komorbidität und Ätiologie"),
  row_cat_level("Andere",       s$äthiologie_cat, "Andere",     "Komorbidität und Ätiologie"),
  row_cat_level("Unbekannt",    s$äthiologie_cat, "Unbekannt",  "Komorbidität und Ätiologie"),
  row_cat_level("Symptomatisch",           s$Symptom_cat, "Symptomatisch",
                                                              "Person und Anamnese"),
  row_cat_level("Asymptomatisch (Zufallsbefund)",
                s$Symptom_cat, "Asymptomatisch_Zufall",        "Person und Anamnese"),
  row_cat_level("Asymptomatisch (Überwachung)",
                s$Symptom_cat, "Asymptomatisch_Ueberwachung",  "Person und Anamnese"),
  row_cat_level("B-Symptomatik",
                s$B_Symptomatik, "ja",                         "Person und Anamnese"),
  row_cat_level("Leberzirrhose",        s$Leberzirrhose, "ja", "Leberfunktion und Labor (POD $-$1)"),
  row_num("Child-Pugh Punkte",             s$ChildPugh_Punkte, "Leberfunktion und Labor (POD $-$1)"),
  row_num("Bilirubin (mg/dL)",       s$Bili_preop_val,   "Leberfunktion und Labor (POD $-$1)"),
  row_num("Albumin (g/L)",           s$Albumin_preop,    "Leberfunktion und Labor (POD $-$1)"),
  row_num("Quick (%)",               s$Quick_preop,      "Leberfunktion und Labor (POD $-$1)"),
  row_num("Thrombozyten (/nL)",      s$Thrombos_preop,   "Leberfunktion und Labor (POD $-$1)"),
  row_num("ALBI-Score",                    s$ALBI_score,       "Leberfunktion und Labor (POD $-$1)"),
  row_cat_level("ALBI-Grad 1", s$ALBI_grade, "Grade 1",        "Leberfunktion und Labor (POD $-$1)"),
  row_cat_level("ALBI-Grad 2", s$ALBI_grade, "Grade 2",        "Leberfunktion und Labor (POD $-$1)"),
  row_cat_level("ALBI-Grad 3", s$ALBI_grade, "Grade 3",        "Leberfunktion und Labor (POD $-$1)"),
  row_num("GOT (U/L)",              s$AST_preop,        "Leberfunktion und Labor (POD $-$1)"),
  row_num("GPT (U/L)",              s$ALT_preop,        "Leberfunktion und Labor (POD $-$1)"),
  row_num("Leukozyten (/nL)",       s$Leukos_preop,     "Leberfunktion und Labor (POD $-$1)"),
  row_num("CRP (mg/L)",             s$CRP_preop,        "Leberfunktion und Labor (POD $-$1)"),
  row_num("De-Ritis-Quotient",      s$deritis_preop,    "Leberfunktion und Labor (POD $-$1)"),
  row_num("AST/Albumin-Ratio",      s$astalb_preop,     "Leberfunktion und Labor (POD $-$1)"),
  row_num("APRI",                   s$APRI_preop,       "Leberfunktion und Labor (POD $-$1)"),
  row_num("Tumordurchmesser (cm)",         s$Tumordurchmesser, "Tumor"),
  row_cat_level("Tumoranzahl: multipel (vs. solitär)",
                as.character(s$lesion_multi_bin), "1", "Tumor"),
  row_num("T-Stadium (ordinal 1-4)",
          suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(s$T)))), "Tumor"),
  row_cat_level("N-Stadium: N0", na_if(as.character(s$N), "Nx"), "N0", "Tumor"),
  row_cat_level("N-Stadium: N1", na_if(as.character(s$N), "Nx"), "N1", "Tumor"),
  row_cat_level("M-Stadium: M0", na_if(as.character(s$M), "Mx"), "M0", "Tumor"),
  row_cat_level("M-Stadium: M1", na_if(as.character(s$M), "Mx"), "M1", "Tumor"),
  row_num("FLV (cm³)",               s$FLV,              "Volumetrie präop"),
  row_num("TLV (cm³)",               s$TLV_preop,        "Volumetrie präop"),
  row_num("FLV/TLV-Ratio",                 s$FLV / s$TLV_preop, "Volumetrie präop", digits = 2),
  row_num("TTLVR (Tumor/TLV)",             s$TTLVR_preop,      "Volumetrie präop", digits = 3)
)


tab1b <- bind_rows(
  row_num("Resezierte Segmente (Anzahl)",  s$n_op_segments,             "OP"),
  row_cat_level("Major-Hepatektomie (≥ 3 Segmente)",
                as.character(s$D_major),                 "1", "OP"),
  row_cat_level("Laparoskopischer Zugang",
                as.character(s$OP_laparoskop),           "laparoskopisch", "OP"),
  row_cat_level("Zeitgleiche Cholezystektomie",
                as.character(s$OP_CCE),                  "ja", "OP")
)

tab1c <- bind_rows(
  row_num("TLV 1. postop (cm³)",           s$TLV_first_postop,             "Bildgebung und Volumetrie postop"),
  row_num("1. postop Volumetrie (POD)", s$POD_first_postop_volumetry, "Bildgebung und Volumetrie postop"),
  row_num("Δ TLV vs. FLV (%)", s$d_TLV_rel_FLV_base_first_postop, "Bildgebung und Volumetrie postop"),
  row_num("LVR (TLV 91-180 / FLV)", s$LVR_nam, "Bildgebung und Volumetrie postop"),
  row_num("TLV-Log-Wachstumsrate (pro Tag)", s$TLV_growth_rate, "Bildgebung und Volumetrie postop", digits = 4)
)

tab1d <- bind_rows(
  row_num("Albumin (g/L/Tag)",     s$slope_Albumin_POD1_7, "Labor (POD 1 bis 7)"),
  row_num("Quick (%/Tag)",         s$slope_Quick_POD1_7,   "Labor (POD 1 bis 7)"),
  row_num("GOT (U/L/Tag)",         s$slope_GOT_POD1_7,     "Labor (POD 1 bis 7)"),
  row_num("GPT (U/L/Tag)",         s$slope_GPT_POD1_7,     "Labor (POD 1 bis 7)"),
  row_num("CRP (mg/L/Tag)",        s$slope_CRP_POD1_7,     "Labor (POD 1 bis 7)"),
  row_num("Leukozyten (/nL/Tag)",  s$slope_Leukos_POD1_7,  "Labor (POD 1 bis 7)"),
  row_num("Thrombozyten (/nL/Tag)", s$slope_Thrombos_POD1_7, "Labor (POD 1 bis 7)")
)


n_total <- nrow(static_an)
n_rez_y <- sum(g == 1, na.rm = TRUE)
n_rez_n <- sum(g == 0, na.rm = TRUE)
add_n_header <- function(tb) {
  names(tb)[names(tb) == "Gesamt"]        <- sprintf("Gesamt (N=%d)",         n_total)
  names(tb)[names(tb) == "Rezidiv: ja"]   <- sprintf("Rezidiv: ja (N=%d)",    n_rez_y)
  names(tb)[names(tb) == "Rezidiv: nein"] <- sprintf("Rezidiv: nein (N=%d)",  n_rez_n)
  tb
}
tab1  <- add_n_header(tab1)
tab1b <- add_n_header(tab1b)
tab1c <- add_n_header(tab1c)
tab1d <- add_n_header(tab1d)

# ── Tab 3.1 als eine longtable (Format wie Tab 2.2) ──
tab1$phase  <- "Präoperativ"
tab1b$phase <- "Intraoperativ"
tab1c$phase <- "Postoperativ"
tab1d$phase <- "Postoperativ"
tab_char <- bind_rows(tab1, tab1b, tab1c, tab1d)

tab_char$Gruppe <- sub(" (präop|postop)$", "", tab_char$Gruppe)
tab_char$block  <- paste0(tab_char$phase, ": ", tab_char$Gruppe)
tab_char$block[tab_char$Gruppe == "OP"] <- "Intraoperativ"

tab_char$block[tab_char$Variable %in% c("Tumordurchmesser (cm)",
                                        "Tumoranzahl: multipel (vs. solitär)",
                                        "T-Stadium (ordinal 1-4)",
                                        "N-Stadium: N0", "N-Stadium: N1",
                                        "M-Stadium: M0",
                                        "M-Stadium: M1")] <-
  "Postoperativ: histopathologischer Befund"

.block_folge <- c(
  "Präoperativ: Person und Anamnese",
  "Präoperativ: Komorbidität und Ätiologie",
  "Präoperativ: Leberfunktion und Labor (POD $-$1)",
  "Präoperativ: Volumetrie",
  "Intraoperativ",
  "Postoperativ: histopathologischer Befund",
  "Postoperativ: Labor (POD 1 bis 7)",
  "Postoperativ: Bildgebung und Volumetrie")
.ohne_folge <- setdiff(unique(tab_char$block), .block_folge)
if (length(.ohne_folge) > 0) {
  warning("Block ohne feste Reihenfolge: ",
          paste(.ohne_folge, collapse = ", "))
}
tab_char <- tab_char[order(match(tab_char$block, .block_folge)), ]
tab_char$block[tab_char$block == "Postoperativ: Labor (POD 1 bis 7)"] <-
  "Postoperativ: Labor-Steigung (POD 1 bis 7)"

.spalten <- c("Variable",
              sprintf("Gesamt (N=%d)", n_total),
              sprintf("Rezidiv: ja (N=%d)", n_rez_y),
              sprintf("Rezidiv: nein (N=%d)", n_rez_n),
              "p")
.kopf <- paste0(paste(c("Variable",
                        "Gesamt",
                        "Rezidiv: ja",
                        "Rezidiv: nein",
                        "p"), collapse = " & "), " \\\\")

.out <- c(
  paste0("% Patient*innencharakteristika, gesamt und stratifiziert nach ",
         "Rezidivstatus. Quelle: 02_Code/05_Descriptive_Figures.R"),
  "\\setlength{\\tabcolsep}{3pt}",
  paste0("\\begin{longtable}{@{}",
         ">{\\raggedright\\arraybackslash}p{5.2cm}",
         ">{\\raggedleft\\arraybackslash}p{2.6cm}",
         ">{\\raggedleft\\arraybackslash}p{2.6cm}",
         ">{\\raggedleft\\arraybackslash}p{2.6cm}",
         ">{\\raggedleft\\arraybackslash}p{1.15cm}@{}}"),
  paste0("\\caption[Patient*innencharakteristika]{Patient*innencharakteristika, ",
         "gesamt und stratifiziert nach Rezidivstatus.}",
         "\\label{tab:results_desc:char}\\\\"),
  "\\toprule", .kopf, "\\midrule", "\\endfirsthead",
  "\\toprule", .kopf, "\\midrule", "\\endhead",
  "\\bottomrule", "\\insertTableNotes", "\\endlastfoot"
)
for (.b in unique(tab_char$block)) {
  if (.b == "Präoperativ: Volumetrie") {
    .out <- c(.out, "\\pagebreak")
  }
  .out <- c(.out, sprintf("\\multicolumn{5}{@{}l}{\\textit{%s}} \\\\[2pt]", .b))
  .s <- tab_char[tab_char$block == .b, ]
  for (.i in seq_len(nrow(.s))) {
    .out <- c(.out, sprintf("%s & %s & %s & %s & %s \\\\",
                            tex_escape_cell(.s[[.spalten[1]]][.i]),
                            nolig_dash(tex_escape_cell(.s[[.spalten[2]]][.i])),
                            nolig_dash(tex_escape_cell(.s[[.spalten[3]]][.i])),
                            nolig_dash(tex_escape_cell(.s[[.spalten[4]]][.i])),
                            tex_escape_cell(.s[[.spalten[5]]][.i])))
  }
  .out <- c(.out, "\\addlinespace")
}
.out <- c(.out,
          "\\midrule",
          sprintf("N & %d & %d & %d &  \\\\", n_total, n_rez_y, n_rez_n),
          "\\end{longtable}")
writeLines(.out, file.path(fig_dir, "Tab_3_1_Charakteristika.tex"), useBytes = TRUE)
message("Wrote: Tab_3_1_Charakteristika.tex (", nrow(tab_char), " Variablenzeilen, ",
        length(unique(tab_char$block)), " Gruppen)")


# ── Tab 3.2 + Abb 3.1 — Missing-Data-Profil ──

construction_rules <- list(
  list(vars = c("n_rezidiv", "time_to_first_rezidiv_days",
                "date_rezidiv_first", "D_rezidiv_cat_first",
                "rezidiv_therapie_cat_first", "n_rezidiv_therapie"),
       condition = function(df) df$D_rezidiv == 0L,
       label     = "kein Rezidiv aufgetreten"),
  list(vars = c("ND_Nikotin_py"),
       condition = function(df) {
         if (!"ND_Nikotin" %in% names(df)) return(rep(FALSE, nrow(df)))
         df$ND_Nikotin == 0L | is.na(df$ND_Nikotin)
       },
       label     = "kein Nikotinkonsum dokumentiert"),
  list(vars = c("TLV_growth_rate"),
       condition = function(df) {
         if (!"n_tlv_growth_pts" %in% names(df)) return(rep(FALSE, nrow(df)))
         is.na(df$n_tlv_growth_pts) | df$n_tlv_growth_pts < 2
       },
       label     = "weniger als zwei postoperative Volumetrien")
)

classify_missing <- function(df) {
  vars <- names(df)
  out <- vapply(vars, function(v) {
    x <- df[[v]]
    is_na <- is.na(x)
    bc_mask <- rep(FALSE, length(x))
    for (rule in construction_rules) {
      if (v %in% rule$vars) {
        cond <- rule$condition(df)
        bc_mask <- bc_mask | (is_na & cond)
      }
    }
    n_total <- length(x)
    c(true_missing_pct    = mean(is_na & !bc_mask) * 100,
      construction_pct    = mean(bc_mask)          * 100,
      observed_pct        = mean(!is_na)           * 100)
  }, numeric(3))
  as.data.frame(t(out)) |>
    tibble::rownames_to_column("Variable") |>
    mutate(across(c(true_missing_pct, construction_pct, observed_pct),
                  ~ round(.x, 1)))
}

static_miss <- static_an |>
  mutate(N = na_if(as.character(N), "Nx"), M = na_if(as.character(M), "Mx"))
miss_class <- classify_missing(static_miss)

GRP_LEVELS <- c(
  "Präoperativ: Person und Anamnese",
  "Präoperativ: Komorbidität und Ätiologie",
  "Präoperativ: Leberfunktion und Labor (POD $-$1)",
  "Präoperativ: Volumetrie",
  "Intraoperativ",
  "Postoperativ: histopathologischer Befund",
  "Postoperativ: Labor (POD 1 bis 7)",
  "Postoperativ: Bildgebung und Volumetrie",
  "Postoperativ: Nachsorge und Endpunkt",
  "Sonstige")
missing_group <- function(v) {
  dplyr::case_when(
    stringr::str_detect(v, "[Ss]lope|Steigung")                       ~ "Postoperativ: Labor (POD 1 bis 7)",
    v %in% c("Alter", "BMI", "KGkg", "m", "mwd", "Geburt") |
      stringr::str_detect(v, "Symptom")                               ~ "Präoperativ: Person und Anamnese",
    stringr::str_detect(v, "^ND_") | v %in% c("DM", "Vorbehandlung") |
      stringr::str_detect(v, "tiologie|thiologie|Aetio")              ~ "Präoperativ: Komorbidität und Ätiologie",
    v %in% c("Leberzirrhose", "ChildPugh_Punkte", "ChildPugh_Stadium") |
      stringr::str_detect(v, "Albumin|Bili|Quick|GOT|GPT|CRP|Leuko|Thrombo|ALBI|APRI|deritis|astalb|Gluc") ~ "Präoperativ: Leberfunktion und Labor (POD $-$1)",
    v %in% c("T", "N", "M", "Tumordurchmesser") |
      stringr::str_detect(v, "[Tt]umor|TNM")                          ~ "Postoperativ: histopathologischer Befund",
    stringr::str_detect(v, "OP|Segment|Cholezyst")                    ~ "Intraoperativ",
    stringr::str_detect(v, "FLV|TLV|Volum|LVR|TTLVR|POD_nam")        ~ "Postoperativ: Bildgebung und Volumetrie",
    stringr::str_detect(v, "[Rr]ezidiv|^FU|Clavien")                  ~ "Postoperativ: Nachsorge und Endpunkt",
    TRUE                                                              ~ "Sonstige"
  )
}

label_32_map <- c(
  Symptom_cat = "Symptomstatus", BMI = "BMI", m = "Körpergröße",
  KGkg = "Körpergewicht", Äthiologie = "Ätiologie",
  CRP_preop = "CRP", Albumin_preop = "Albumin", Leukos_preop = "Leukozyten",
  Thrombos_preop = "Thrombozyten", deritis_preop = "De-Ritis-Quotient",
  ALBI_score = "ALBI-Score und -Grad",
  T = "T-Stadium", N = "N-Stadium", M = "M-Stadium",
  slope_Albumin_POD1_7 = "Albumin", slope_GOT_POD1_7 = "GOT",
  slope_GPT_POD1_7 = "GPT", slope_CRP_POD1_7 = "CRP",
  slope_Quick_POD1_7 = "Quick", slope_Leukos_POD1_7 = "Leukozyten",
  slope_Thrombos_POD1_7 = "Thrombozyten",
  LVR_nam = "LVR (TLV 91-180 / FLV)", TLV_growth_rate = "TLV-Log-Wachstumsrate",
  rezidiv_therapie_cat_first = "1. Rezidiv: Therapie-Kategorie",
  ClavienDindo = "Clavien-Dindo",
  D_rezidiv_cat_first = "1. Rezidiv: Lokalisation",
  date_rezidiv_first = "1. Rezidiv: Datum")
label_32 <- function(v) {
  ifelse(v %in% names(label_32_map), unname(label_32_map[v]), label_var(v))
}

miss_table <- miss_class |>
  filter(!Variable %in% c("Studienausschluss", "n_tlv_growth_pts",
                          "ALBI_grade", "astalb_preop", "TLV_nam_91_180",
                          "POD_nam_91_180", "time_to_first_rezidiv_days")) |>
  rename(`Echte Lücke %`   = true_missing_pct,
         `Nicht definiert %` = construction_pct,
         `Vorhanden %`     = observed_pct) |>
  filter(`Echte Lücke %` > 5) |>
  mutate(Gruppe = factor(missing_group(Variable), levels = GRP_LEVELS)) |>
  arrange(Gruppe, desc(`Echte Lücke %`)) |>
  mutate(Variable = label_32(Variable)) |>
  select(Variable, Gruppe, everything())

# ── Tab 3.2 als longtable mit Gruppen-Zwischenzeilen (Format wie Tab 2.2) ──
.k32 <- paste0(paste(c("Variable", "Fehlend \\%"),
                     collapse = " & "), " \\\\")
.o32 <- c(
  paste0("% Fehlendprofil der Variablen. ",
         "Quelle: 02_Code/05_Descriptive_Figures.R"),
  paste0("\\begin{longtable}{@{}",
         ">{\\raggedright\\arraybackslash}p{6.4cm}",
         ">{\\raggedleft\\arraybackslash}p{2.4cm}@{}}"),
  paste0("\\caption[Fehlendprofil der Variablen]{Fehlendprofil der ",
         "Variablen.}",
         "\\label{tab:results_desc:missing}\\\\"),
  "\\toprule", .k32, "\\midrule", "\\endfirsthead",
  "\\toprule", .k32, "\\midrule", "\\endhead",
  "\\bottomrule", "\\insertTableNotes", "\\endlastfoot"
)
for (.b in unique(miss_table$Gruppe)) {
  .o32 <- c(.o32, sprintf("\\multicolumn{2}{@{}l}{\\textit{%s}} \\\\[2pt]",
                          sub("Postoperativ: Labor (POD 1 bis 7)",
                              "Postoperativ: Labor-Steigung (POD 1 bis 7)",
                              .b, fixed = TRUE)))
  .s <- miss_table[miss_table$Gruppe == .b, ]
  for (.i in seq_len(nrow(.s))) {
    .o32 <- c(.o32, sprintf("%s & %s \\\\",
                            tex_escape_cell(.s$Variable[.i]),
                            sprintf("%.1f", as.numeric(.s$`Echte Lücke %`[.i]))))
  }
  .o32 <- c(.o32, "\\addlinespace")
}
.o32 <- c(.o32, "\\end{longtable}")
writeLines(.o32, file.path(fig_dir, "Tab_3_2_Missing_Profil.tex"), useBytes = TRUE)
message("Wrote: Tab_3_2_Missing_Profil.tex (", nrow(miss_table), " Variablen, ",
        length(unique(miss_table$Gruppe)), " Gruppen)")

miss_logical <- static_an |>
  select(-any_of("Studienausschluss")) |>
  mutate(across(-Pseudonym, ~ is.na(.x)))
miss_long <- miss_logical |>
  pivot_longer(-Pseudonym, names_to = "Variable", values_to = "is_missing") |>
  left_join(static_an |> select(Pseudonym, D_rezidiv,
                                any_of("ND_Nikotin")),
            by = "Pseudonym") |>
  mutate(
    construction_na = case_when(
      Variable %in% construction_rules[[1]]$vars &
        is_missing & D_rezidiv == 0L ~ TRUE,
      Variable == "ND_Nikotin_py" & is_missing &
        ("ND_Nikotin" %in% names(static)) &
        (replace(ND_Nikotin, is.na(ND_Nikotin), 0) == 0L) ~ TRUE,
      TRUE ~ FALSE
    ),
    status = factor(case_when(
      !is_missing       ~ "Vorhanden",
      construction_na   ~ "By construction",
      TRUE              ~ "True missing"
    ), levels = c("Vorhanden", "By construction", "True missing"))
  )

var_miss_share <- miss_long |> group_by(Variable) |>
  summarise(m = mean(is_missing), .groups = "drop") |> arrange(desc(m))
miss_long <- miss_long |>
  mutate(Variable = factor(Variable, levels = var_miss_share$Variable))

p_miss <- ggplot(miss_long, aes(x = Variable, y = Pseudonym, fill = status)) +
  geom_tile() +
  scale_fill_manual(values = c("Vorhanden" = "grey88",
                               "By construction" = "#F4C95D",
                               "True missing"    = "grey20"),
                    name = NULL) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "sans") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")

if (FALSE) ggsave(file.path(fig_dir, "Abb_3_1_Missing_Heatmap.png"), plot = p_miss,
       width = 12, height = 7, dpi = 300, bg = "white")


# ── Tab 3.3 — Rezidiv- und Todesfall-Übersicht ──

surv_df <- read_excel("./01_Daten/data_dynamic_surv.xlsx")
surv_df <- surv_df |> filter(Pseudonym %in% analysis_pseu)

kmf <- survfit(Surv(time, event) ~ 1, data = surv_df)
s_at <- summary(kmf, times = c(365, 365*3, 365*5))
get_at <- function(t) {
  idx <- which.min(abs(s_at$time - t))
  if (length(idx) == 0) return("--")
  sprintf("%.1f%% [%.1f–%.1f]", s_at$surv[idx]*100,
          s_at$lower[idx]*100, s_at$upper[idx]*100)
}

med_rfs <- summary(kmf)$table["median"]
rez_t <- surv_df$time[surv_df$event == 1 & !is.na(surv_df$time)]
n_rez_early12 <- sum(rez_t <= 365)
n_rez_early24 <- sum(rez_t <= 730)
n_rez_late    <- sum(rez_t  > 730)

fu <- str_to_lower(as.character(static_an$FUStatus))
os_event <- as.integer(fu %in% c("dod", "doc", "cod unknown"))
os_time  <- dplyr::coalesce(
  as.numeric(static_an$FU_duration_days),
  as.numeric(surv_df$time[match(static_an$Pseudonym, surv_df$Pseudonym)]))
ok_os <- !is.na(os_time) & os_time > 0
os_df <- data.frame(time = os_time[ok_os], event = os_event[ok_os])
kmf_os <- survfit(Surv(time, event) ~ 1, data = os_df)
s_at_os <- summary(kmf_os, times = c(365, 365*3, 365*5))

local({
  rez <- as.integer(static_an$D_rezidiv)[ok_os]
  km2 <- survfit(Surv(time, event) ~ rez, data = cbind(os_df, rez = rez))
  s2  <- summary(km2, times = 365*5)
  for (i in seq_along(s2$surv)) {
    cat(sprintf(paste0("[os-by-rez] %s: 5-Jahres-OS %.1f %% [%.1f; %.1f], ",
                       "n unter Risiko %d\n"),
                as.character(s2$strata[i]), 100 * s2$surv[i],
                100 * s2$lower[i], 100 * s2$upper[i], s2$n.risk[i]))
  }
})
get_at_os <- function(t) {
  idx <- which.min(abs(s_at_os$time - t))
  if (length(idx) == 0) return("--")
  sprintf("%.1f%% [%.1f–%.1f]", s_at_os$surv[idx]*100,
          s_at_os$lower[idx]*100, s_at_os$upper[idx]*100)
}

n_an   <- nrow(static_an)
n_rez  <- length(rez_t)
n_rez_early3 <- sum(rez_t <= 90)
n_rez_3_12   <- sum(rez_t > 90  & rez_t <= 365)
n_rez_12_24  <- sum(rez_t > 365 & rez_t <= 730)

pct_kohorte <- function(n) sprintf("%d (%.1f%%)", n, 100*n/n_an)
pct_rez     <- function(n) if (n_rez == 0) sprintf("%d (--)", n) else
                            sprintf("%d (%.1f%% der Rezidive)", n, 100*n/n_rez)

B  <- rawToChar(as.raw(92))
BB <- paste0(B, B)
.row33 <- function(k, w) sprintf(paste0("%s & %s ", BB),
                                 tex_escape_cell(k), tex_escape_cell(w))
.hdr33 <- function(t) sprintf(paste0(B, "multicolumn{2}{@{}l}{", B,
                                     "textit{%s}} ", BB, "[2pt]"),
                              tex_escape_cell(t))
.fu33  <- function(k) fmt_n_pct(sum(fu == k, na.rm = TRUE), length(fu))

.o33 <- c(
  paste0("% Rezidiv- und Todesfall-Uebersicht (Analyse-Kohorte; OS via ",
         "FUStatus All-cause-Death-Konvention). ",
         "Quelle: 02_Code/05_Descriptive_Figures.R"),
  paste0(B, "begin{tabular}{lr}"), paste0(B, "toprule"),
  paste0(tex_escape_cell("Kennzahl"), " & ", tex_escape_cell("Wert"), " ", BB),
  paste0(B, "midrule"),
  .row33("Patient*innen mit Rezidiv", pct_kohorte(n_rez)),
  paste0(B, "addlinespace"),
  .hdr33("Zeitpunkt des Auftretens des Erstrezidivs"),
  .row33("0-3 Monate",   pct_rez(n_rez_early3)),
  .row33("3-12 Monate",  pct_rez(n_rez_3_12)),
  .row33("12-24 Monate", pct_rez(n_rez_12_24)),
  .row33("> 24 Monate",  pct_rez(n_rez_late)),
  .row33("Median TTR (Tage)",
         if (is.na(med_rfs)) "--" else sprintf("%.0f", med_rfs)),
  paste0(B, "addlinespace"),
  .hdr33("Anteil an Patient*innen ohne Rezidiv nach"),
  .row33("1 Jahr",   get_at(365)),
  .row33("3 Jahren", get_at(365 * 3)),
  .row33("5 Jahren", get_at(365 * 5)),
  paste0(B, "addlinespace"),
  .hdr33("FU-Status"),
  .row33("NED", .fu33("ned")),
  .row33("AWD", .fu33("awd")),
  .row33("DOD",                     .fu33("dod")),
  .row33("DOC",                     .fu33("doc")),
  .row33("COD unknown",             .fu33("cod unknown")),
  paste0(B, "addlinespace"),
  .hdr33("Gesamtüberleben (Tod jeder Ursache)"),
  .row33("1 Jahr",  get_at_os(365)),
  .row33("3 Jahre", get_at_os(365 * 3)),
  .row33("5 Jahre", get_at_os(365 * 5)),
  paste0(B, "bottomrule"), paste0(B, "end{tabular}"))

.p33 <- file.path(fig_dir, "Tab_3_3_Rezidiv_Todesfall_Uebersicht_Landscape.tex")
.c33 <- file(.p33, "w", encoding = "UTF-8"); writeLines(.o33, .c33); close(.c33)
message("Wrote: Tab_3_3_Rezidiv_Todesfall_Uebersicht_Landscape.tex (4 Bloecke)")


# ── Abb 3.2 / 3.3 — KM-Hauptkurven TTR und OS mit Risk-Tables ──
suppressPackageStartupMessages({
  library(ggplot2); library(grid); library(gridExtra)
})

km_to_df <- function(fit) {
  lower <- fit$lower
  upper <- fit$upper
  if (any(is.na(lower))) lower[is.na(lower)] <- tail(lower[!is.na(lower)], 1)
  if (any(is.na(upper))) upper[is.na(upper)] <- tail(upper[!is.na(upper)], 1)
  data.frame(time  = c(0, fit$time),
             surv  = c(1, fit$surv),
             lower = c(1, lower),
             upper = c(1, upper))
}

risk_table_grob <- function(fit, times, label_top, xmax) {
  s <- summary(fit, times = times, extend = TRUE)
  n_at_risk <- s$n.risk
  df <- data.frame(x = times, n = sprintf("%d", n_at_risk),
                   stringsAsFactors = FALSE)
  ggplot(df, aes(x = x, y = 0, label = n)) +
    geom_text(size = 3.4, colour = "#00441B") +
    scale_x_continuous(limits = c(0, xmax), breaks = times) +
    labs(title = label_top) +
    theme_void() +
    theme(plot.title = element_text(size = 9.5, face = "bold", hjust = 0.5,
                                    colour = "#00441B"),
          plot.margin = margin(8, 10, 0, 10))
}

km_mit_zahlen <- function(p_top, p_bottom) {
  g1 <- ggplotGrob(p_top)
  g2 <- ggplotGrob(p_bottom)
  if (length(g1$widths) == length(g2$widths)) {
    w <- grid::unit.pmax(g1$widths, g2$widths)
    g1$widths <- w
    g2$widths <- w
  } else {
    message("[05] KM-Zahlenzeile: Spaltenzahl ungleich, keine Ausrichtung")
  }
  arrangeGrob(g1, g2, ncol = 1, heights = c(5, 1))
}

df_rfs <- km_to_df(kmf)
max_d_rfs <- max(surv_df$time, na.rm = TRUE)
yr_times_rfs <- seq(0, max_d_rfs, by = 365)
p_rfs <- ggplot(df_rfs, aes(x = time, y = surv)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18,
              fill = "grey30") +
  geom_step(colour = "grey30", linewidth = 0.9) +
  scale_x_continuous(breaks = yr_times_rfs,
                     labels = seq_along(yr_times_rfs) - 1,
                     limits = c(0, max_d_rfs)) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Jahre nach OP", y = "Patient*innen ohne Rezidiv") +
  theme_bw(base_size = 11) +
  theme(plot.margin = margin(5, 10, 0, 10))

rt_rfs <- risk_table_grob(kmf, yr_times_rfs[yr_times_rfs <= max_d_rfs],
                          "Patient*innen unter Risiko", max_d_rfs)

png(file.path(fig_dir, "Abb_3_2_KM_RFS.png"),
    width = 1680, height = 1222, res = 300)
grid.draw(km_mit_zahlen(p_rfs, rt_rfs))
dev.off()

df_os <- km_to_df(kmf_os)
max_d_os <- max(os_df$time, na.rm = TRUE)
yr_times_os <- seq(0, max_d_os, by = 365)
p_os <- ggplot(df_os, aes(x = time, y = surv)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18,
              fill = "grey30") +
  geom_step(colour = "grey30", linewidth = 0.9) +
  scale_x_continuous(breaks = yr_times_os,
                     labels = seq_along(yr_times_os) - 1,
                     limits = c(0, max_d_os)) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Jahre nach OP", y = "Patient*innen am Leben") +
  theme_bw(base_size = 11) +
  theme(plot.margin = margin(5, 10, 0, 10))

rt_os <- risk_table_grob(kmf_os, yr_times_os[yr_times_os <= max_d_os],
                         "Patient*innen unter Risiko", max_d_os)

png(file.path(fig_dir, "Abb_3_3_KM_OS.png"),
    width = 1680, height = 1222, res = 300)
grid.draw(km_mit_zahlen(p_os, rt_os))
dev.off()


# ── Abb 3.4 — Histogramm Zeit-bis-Rezidiv ──

rez_times <- static_an$time_to_first_rezidiv_days
rez_times <- rez_times[!is.na(rez_times) & rez_times > 0]
df_hist <- data.frame(days = rez_times,
                      months = rez_times / 30.4375)

p_hist <- ggplot(df_hist, aes(x = months)) +
  geom_histogram(binwidth = 3, boundary = 0, fill = "grey60", colour = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = c(12, 24), linetype = "dashed",
             colour = "#00441B", linewidth = 0.8) +
  annotate("text", x = 12, y = Inf, label = "12 Monate", vjust = 2, hjust = -0.1,
           colour = "#00441B", size = 3.2) +
  annotate("text", x = 24, y = Inf, label = "24 Monate", vjust = 2, hjust = -0.1,
           colour = "#00441B", size = 3.2) +
  scale_x_continuous(breaks = seq(0, 240, by = 12)) +
  labs(x = "Monate nach OP", y = "Anzahl Patient*innen") +
  theme_minimal(base_family = "sans")

ggsave(file.path(fig_dir, "Abb_3_4_Histogramm_Zeit_bis_Rezidiv.png"),
       plot = p_hist, width = 9, height = 5.5, dpi = 300, bg = "white")


# ── Abb — Zeitpunkt der ersten postoperativen Volumetrie ──
pod_first <- static_an$POD_first_postop_volumetry
pod_first <- pod_first[!is.na(pod_first)]
n_spaet   <- sum(pod_first > 365)

local({
  pf  <- static_an$POD_first_postop_volumetry
  rez <- as.integer(static_an$D_rezidiv) == 1
  frueh <- !is.na(pf) & pf <= 91
  spaet <- !is.na(pf) & pf >  91
  cat(sprintf(paste0("[messzeitpunkt] erstes Vierteljahr (POD<=91): n=%d, ",
                     "Rezidivrate %.1f %% | spaeter: n=%d, %.1f %% | ",
                     "Gesamtkohorte: n=%d, %.1f %%\n"),
              sum(frueh), 100 * mean(rez[frueh], na.rm = TRUE),
              sum(spaet), 100 * mean(rez[spaet], na.rm = TRUE),
              length(rez), 100 * mean(rez, na.rm = TRUE)))
})

p_volzeit <- ggplot(data.frame(pod = pod_first[pod_first <= 365]), aes(x = pod)) +
  geom_histogram(binwidth = 15, boundary = 0, fill = "grey60",
                 colour = "white", alpha = 0.85) +
  geom_vline(xintercept = c(90, 180), linetype = "dashed",
             colour = "#00441B", linewidth = 0.8) +
  annotate("text", x = 90,  y = Inf, label = "POD 90",  vjust = 2, hjust = -0.1,
           colour = "#00441B", size = 3.2) +
  annotate("text", x = 180, y = Inf, label = "POD 180", vjust = 2, hjust = -0.1,
           colour = "#00441B", size = 3.2) +
  scale_x_continuous(breaks = seq(0, 365, 30)) +
  labs(x = "Tage nach OP (POD)", y = "Anzahl Patient*innen") +
  theme_minimal(base_family = "sans")

ggsave(file.path(fig_dir, "Abb_Volumetrie_Zeitpunkte.png"),
       plot = p_volzeit, width = 9, height = 5.5, dpi = 300, bg = "white")
cat(sprintf("[05] Abb_Volumetrie_Zeitpunkte: %d von %d Erstmessungen jenseits POD 365 (%.1f %%).\n",
            n_spaet, length(pod_first), 100 * n_spaet / length(pod_first)))


# ── Abb 3.5 — Spaghetti TLV-Verlauf (% von TLV präop) nach Rezidivstatus ──

tlv_all <- dynamic_an |>
  filter(datasource == "volumetry",
         !is.na(lab_date_POD_imp), lab_date_POD_imp > 0,
         !is.na(TLV)) |>
  left_join(static_an |> select(Pseudonym, TLV_preop,
                                time_to_first_rezidiv_days),
            by = "Pseudonym") |>
  filter(is.na(time_to_first_rezidiv_days) |
           lab_date_POD_imp <= time_to_first_rezidiv_days) |>
  filter(!is.na(TLV_preop), TLV_preop > 0) |>
  mutate(Monate = lab_date_POD_imp / 30.4375,
         TLV_pct = TLV / TLV_preop * 100,
         Rezidiv = factor(if_else(D_rezidiv == 1, "Rezidiv", "Kein Rezidiv"),
                          levels = c("Kein Rezidiv", "Rezidiv")))

tlv_traj <- tlv_all |> filter(Monate <= 36)
cat(sprintf(paste0("[05] Abb_3_5 Spaghetti: %d Patient*innen, %d Volumetrien ",
                   "(ohne Rezidiv %d, mit Rezidiv %d Patient*innen); ",
                   "%d Messungen nach 36 Monaten nicht dargestellt; ",
                   "%d Werte ausserhalb 20-140 %%; ",
                   "%d Patient*innen mit nur einer Messung im Fenster\n"),
            dplyr::n_distinct(tlv_traj$Pseudonym), nrow(tlv_traj),
            dplyr::n_distinct(tlv_traj$Pseudonym[tlv_traj$Rezidiv == "Kein Rezidiv"]),
            dplyr::n_distinct(tlv_traj$Pseudonym[tlv_traj$Rezidiv == "Rezidiv"]),
            nrow(tlv_all) - nrow(tlv_traj),
            sum(tlv_traj$TLV_pct < 20 | tlv_traj$TLV_pct > 140),
            sum(table(tlv_traj$Pseudonym) == 1)))

p_spag <- ggplot(tlv_traj, aes(x = Monate, y = TLV_pct, group = Pseudonym,
                               colour = Rezidiv)) +
  geom_line(alpha = 0.18, linewidth = 0.3) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey60") +
  geom_smooth(aes(group = Rezidiv), method = "loess", se = TRUE,
              linewidth = 1.3) +
  scale_colour_manual(values = c("Kein Rezidiv" = col_blue,
                                 "Rezidiv"      = col_red)) +
  facet_wrap(~ Rezidiv, ncol = 2) +
  scale_y_continuous(breaks = c(60, 100, 140)) +
  coord_cartesian(ylim = c(20, 140)) +
  labs(x = "Monate nach OP", y = "TLV (% des präoperativen TLV)",
       colour = NULL) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

ggsave(file.path(fig_dir, "Abb_3_5_Spaghetti_TLV.png"), plot = p_spag,
       width = 11, height = 6, dpi = 300, bg = "white")


# ── Abb 3.7 — Box-/Violinplot TLV-Snapshots, zensiert beim 1. Rezidiv ──

tlv_bins <- dynamic_an |>
  filter(datasource == "volumetry", !is.na(TLV)) |>
  left_join(static_an |> select(Pseudonym, TLV_preop,
                                time_to_first_rezidiv_days),
            by = "Pseudonym") |>
  filter(is.na(time_to_first_rezidiv_days) |
           is.na(lab_date_POD_imp) |
           lab_date_POD_imp <= time_to_first_rezidiv_days) |>
  filter(!is.na(TLV_preop), TLV_preop > 0) |>
  filter(!is.na(lab_date_POD_imp), lab_date_POD_imp > 0) |>
  mutate(
    TLV_pct = TLV / TLV_preop * 100,
    Fenster = case_when(
      lab_date_POD_imp <= 30  ~ "1-30 d",
      lab_date_POD_imp <= 90  ~ "31-90 d",
      lab_date_POD_imp <= 180 ~ "91-180 d",
      lab_date_POD_imp <= 365 ~ "181-365 d",
      lab_date_POD_imp <= 730 ~ "1-2 J",
      TRUE                    ~ "> 2 J"),
    Fenster = factor(Fenster, levels = c("1-30 d","31-90 d","91-180 d",
                                         "181-365 d","1-2 J","> 2 J")),
    Rezidiv = factor(if_else(D_rezidiv == 1, "Rezidiv", "Kein Rezidiv"),
                     levels = c("Kein Rezidiv", "Rezidiv")))

eff <- tlv_bins |>
  group_by(Fenster) |>
  summarise(
    med_kein  = median(TLV_pct[Rezidiv == "Kein Rezidiv"], na.rm = TRUE),
    med_rez   = median(TLV_pct[Rezidiv == "Rezidiv"],      na.rm = TRUE),
    diff      = round(med_kein - med_rez, 1),
    n_kein    = sum(Rezidiv == "Kein Rezidiv"),
    n_rez     = sum(Rezidiv == "Rezidiv"),
    p_mwu     = tryCatch(
      wilcox.test(TLV_pct ~ Rezidiv, exact = FALSE)$p.value,
      error = function(e) NA_real_),
    p_lab     = if_else(is.na(p_mwu), "p = NA",
                        if_else(p_mwu < 0.001, "p < 0.001",
                                if_else(p_mwu > 0.999, "p > 0.999",
                                        sprintf("p = %.3f", p_mwu)))),
    .groups   = "drop"
  ) |>
  mutate(annot = sprintf("Δmedian = %+0.1f%%\\n%s\\nn = %d/%d",
                         diff, p_lab, n_kein, n_rez))

y_top <- 150

p_box <- ggplot(tlv_bins, aes(x = Fenster, y = TLV_pct, fill = Rezidiv)) +
  geom_violin(position = position_dodge(0.8), alpha = 0.5,
              colour = NA, scale = "width") +
  geom_boxplot(position = position_dodge(0.8), width = 0.2,
               outlier.size = 0.5) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  geom_text(data = eff,
            aes(x = Fenster, y = y_top, label = gsub("\\\\n", "\n", annot)),
            inherit.aes = FALSE,
            size = 2.9, colour = "grey25", lineheight = 0.9) +
  scale_fill_manual(values = c("Kein Rezidiv" = col_blue,
                               "Rezidiv"      = col_red)) +
  coord_cartesian(ylim = c(20, 165)) +
  labs(x = "Zeitfenster", y = "TLV (% des präoperativen TLV)",
       fill = NULL) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "top")

if (FALSE) ggsave(file.path(fig_dir, "Abb_3_7_TLV_Snapshots_Boxviolin.png"), plot = p_box,
       width = 11, height = 6.5, dpi = 300, bg = "white")


# ── Tab 3.4 — Volumetrie-Eckwerte (deskriptiv) ──

vol_vars <- c(
  "FLV", "TLV_preop", "TLV_first_postop",
  "d_TLV_rel_first_postop", "d_TLV_rel_FLV_base_first_postop"
)
vol_vars <- intersect(vol_vars, names(static_an))

vol_lab <- c(
  FLV                             = "FLV (cm³)",
  TLV_preop                       = "TLV (cm³)",
  TLV_first_postop                = "TLV (cm³)",
  d_TLV_rel_first_postop          = "Δ TLV vs. präop TLV (%)",
  d_TLV_rel_FLV_base_first_postop = "Δ TLV vs. FLV (%)"
)

.row34 <- function(v) {
  x <- suppressWarnings(as.numeric(static_an[[v]]))
  sprintf(paste0("%s & %d & %s & %s & %s & %s ", BB),
          tex_escape_cell(vol_lab[[v]]), sum(!is.na(x)),
          tex_escape_cell(fmt_mean_sd(x)),
          nolig_dash(tex_escape_cell(fmt_median_iqr(x))),
          if (all(is.na(x))) "--" else sprintf("%.1f", min(x, na.rm = TRUE)),
          if (all(is.na(x))) "--" else sprintf("%.1f", max(x, na.rm = TRUE)))
}
.hdr34 <- function(t) sprintf(paste0(B, "multicolumn{6}{@{}l}{", B,
                                     "textit{%s}} ", BB, "[2pt]"),
                              tex_escape_cell(t))

.o34 <- c(
  paste0("% Volumetrie-Eckwerte (Analyse-Kohorte; postop jeweils die erste ",
         "Volumetrie). Quelle: 02_Code/05_Descriptive_Figures.R"),
  paste0(B, "begin{tabular}{lrrrrr}"), paste0(B, "toprule"),
  paste0(tex_escape_cell("Variable"), " & n & ",
         tex_escape_cell("Mittelwert ± SD"), " & ",
         tex_escape_cell("Median (IQR)"), " & ",
         tex_escape_cell("Minimum"), " & ",
         tex_escape_cell("Maximum"), " ", BB),
  paste0(B, "midrule"),
  .hdr34("Präoperativ"),
  .row34("FLV"),
  .row34("TLV_preop"),
  paste0(B, "addlinespace"),
  .hdr34("Postoperativ: erste Volumetrie"),
  .row34("TLV_first_postop"),
  .row34("d_TLV_rel_first_postop"),
  .row34("d_TLV_rel_FLV_base_first_postop"),
  paste0(B, "bottomrule"), paste0(B, "end{tabular}"))

.p34 <- file.path(fig_dir, "Tab_3_4_Volumetrie_Eckwerte.tex")
.c34 <- file(.p34, "w", encoding = "UTF-8"); writeLines(.o34, .c34); close(.c34)
message("Wrote: Tab_3_4_Volumetrie_Eckwerte.tex (2 Bloecke)")


# ── Abb 3.6 — Postop Labor-Trajektorien split by Rezidiv & ClavienDindo ──

lab_vars <- c("GOT", "GPT", "CRP", "Albumin", "Bili_preop", "Quick",
              "Leukos", "Thrombos")
lab_vars <- intersect(lab_vars, names(dynamic_an))

lab_long <- dynamic_an |>
  filter(datasource == "labor",
         !is.na(lab_date_POD_imp), lab_date_POD_imp >= 0,
         lab_date_POD_imp <= 14) |>
  mutate(across(all_of(lab_vars), ~ suppressWarnings(as.numeric(.x)))) |>
  pivot_longer(all_of(lab_vars), names_to = "Lab", values_to = "Wert") |>
  filter(!is.na(Wert)) |>
  mutate(Lab = factor(Lab, levels = lab_vars,
                      labels = dplyr::recode(lab_vars,
                                             GOT        = "GOT (U/L)",
                                             GPT        = "GPT (U/L)",
                                             CRP        = "CRP (mg/L)",
                                             Albumin    = "Albumin (g/L)",
                                             Quick      = "Quick (%)",
                                             Leukos     = "Leukozyten (/nL)",
                                             Thrombos   = "Thrombozyten (/nL)",
                                             Bili_preop = "Bilirubin (präop)")))

lab_rez <- lab_long |>
  filter(lab_date_POD_imp <= 10) |>
  mutate(Rezidiv = factor(if_else(D_rezidiv == 1, "Rezidiv", "Kein Rezidiv"),
                          levels = c("Kein Rezidiv", "Rezidiv")))

cat(sprintf(paste0("[05] Abb_3_6a Labor: %d Patient*innen, %d Messwerte ",
                   "(ohne Rezidiv %d, mit Rezidiv %d Patient*innen); ",
                   "%d Messwerte nach POD 10 nicht dargestellt\n"),
            dplyr::n_distinct(lab_rez$Pseudonym), nrow(lab_rez),
            dplyr::n_distinct(lab_rez$Pseudonym[lab_rez$Rezidiv == "Kein Rezidiv"]),
            dplyr::n_distinct(lab_rez$Pseudonym[lab_rez$Rezidiv == "Rezidiv"]),
            sum(lab_long$lab_date_POD_imp > 10)))

p_lab_rez <- ggplot(lab_rez, aes(x = lab_date_POD_imp, y = Wert, colour = Rezidiv)) +
  geom_point(alpha = 0.18, size = 0.5) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.0) +
  scale_colour_manual(values = c("Kein Rezidiv" = col_blue,
                                 "Rezidiv"      = col_red)) +
  scale_x_continuous(breaks = c(1, 3, 5, 7)) +
  facet_wrap(~ Lab, scales = "free_y", ncol = 4, axes = "all_x") +
  labs(x = "Tage nach OP", y = NULL, colour = NULL) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"))

ggsave(file.path(fig_dir, "Abb_3_6a_Labor_Trajectories_Rezidiv.png"),
       plot = p_lab_rez, width = 12, height = 7, dpi = 300, bg = "white")

cd_clean <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(v))
  out[!is.na(v) & v == 0]       <- "0 (keine)"
  out[!is.na(v) & v == 1]       <- "I (leicht)"
  out[!is.na(v) & v == 2]       <- "II"
  out[!is.na(v) & v == 3]       <- "III"
  out[!is.na(v) & v %in% c(4,5)] <- "IV-V (schwer)"
  out
}
lab_cd <- lab_long |>
  mutate(CD = factor(cd_clean(ClavienDindo),
                     levels = c("0 (keine)", "I (leicht)", "II", "III",
                                "IV-V (schwer)"))) |>
  filter(!is.na(CD))

pal_cd <- wes_palette("Zissou1", n = 5, type = "continuous")

p_lab_cd <- ggplot(lab_cd, aes(x = lab_date_POD_imp, y = Wert, colour = CD)) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.0) +
  scale_colour_manual(values = pal_cd) +
  facet_wrap(~ Lab, scales = "free_y", ncol = 4) +
  labs(x = "Tage nach OP", y = NULL, colour = "Clavien-Dindo") +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"))

if (FALSE) ggsave(file.path(fig_dir, "Abb_3_6b_Labor_Trajectories_ClavienDindo.png"),
       plot = p_lab_cd, width = 12, height = 7, dpi = 300, bg = "white")


lab_os <- lab_long |>
  left_join(static_an |> select(Pseudonym, FUStatus), by = "Pseudonym") |>
  mutate(OS_Status = factor(
    if_else(str_to_lower(FUStatus) %in% c("dod", "doc", "cod unknown"),
            "Verstorben (all-cause)", "Lebt / NED / AWD"),
    levels = c("Lebt / NED / AWD", "Verstorben (all-cause)")
  )) |>
  filter(!is.na(OS_Status))

p_lab_os <- ggplot(lab_os, aes(x = lab_date_POD_imp, y = Wert,
                               colour = OS_Status)) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.0) +
  scale_colour_manual(values = c("Lebt / NED / AWD"        = col_blue,
                                 "Verstorben (all-cause)"  = col_red)) +
  facet_wrap(~ Lab, scales = "free_y", ncol = 4) +
  labs(x = "Tage nach OP", y = NULL, colour = NULL) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"))

if (FALSE) ggsave(file.path(fig_dir, "Abb_3_6c_Labor_Trajectories_OS.png"),
       plot = p_lab_os, width = 12, height = 7, dpi = 300, bg = "white")


lab_fr <- lab_long |>
  left_join(static_an |> select(Pseudonym, time_to_first_rezidiv_days),
            by = "Pseudonym") |>
  mutate(Fruehrez = factor(case_when(
    !is.na(time_to_first_rezidiv_days) & time_to_first_rezidiv_days <= 90 ~
      "Rezidiv 0-3 Mo",
    !is.na(time_to_first_rezidiv_days) & time_to_first_rezidiv_days > 90 ~
      "Späteres Rezidiv (> 3 Mo)",
    TRUE ~ "Kein Rezidiv"
  ), levels = c("Kein Rezidiv", "Späteres Rezidiv (> 3 Mo)",
                "Rezidiv 0-3 Mo"))) |>
  filter(!is.na(Fruehrez))

pal_fr <- c("Kein Rezidiv"                = col_blue,
            "Späteres Rezidiv (> 3 Mo)"   = "#E1A11A",
            "Rezidiv 0-3 Mo"          = col_red)

p_lab_fr <- ggplot(lab_fr, aes(x = lab_date_POD_imp, y = Wert,
                               colour = Fruehrez)) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.0) +
  scale_colour_manual(values = pal_fr) +
  facet_wrap(~ Lab, scales = "free_y", ncol = 4) +
  labs(x = "Tage nach OP", y = NULL, colour = NULL) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"))

if (FALSE) ggsave(file.path(fig_dir, "Abb_3_6d_Labor_Trajectories_Fruehrezidiv.png"),
       plot = p_lab_fr, width = 12, height = 7, dpi = 300, bg = "white")


# ── Tab Labor Snapshots — Median (IQR) per Lab × POD-Fenster × Rezidiv ──

lab_snap_vars <- c("GOT", "GPT", "CRP", "Albumin", "Bili_preop", "Quick",
                   "Leukos", "Thrombos")
lab_snap_vars <- intersect(lab_snap_vars, names(dynamic_an))

lab_snap_long <- dynamic_an |>
  filter(datasource == "labor",
         !is.na(lab_date_POD_imp), lab_date_POD_imp >= 1,
         lab_date_POD_imp <= 14) |>
  mutate(across(all_of(lab_snap_vars), ~ suppressWarnings(as.numeric(.x))),
         Fenster = factor(case_when(
           lab_date_POD_imp <= 3 ~ "POD 1-3",
           lab_date_POD_imp <= 7 ~ "POD 4-7",
           TRUE                  ~ "POD 8-14"
         ), levels = c("POD 1-3", "POD 4-7", "POD 8-14")),
         Rezidiv = factor(if_else(D_rezidiv == 1, "Rezidiv", "Kein Rezidiv"),
                          levels = c("Kein Rezidiv", "Rezidiv"))) |>
  pivot_longer(all_of(lab_snap_vars), names_to = "Lab", values_to = "Wert") |>
  filter(!is.na(Wert))

summarise_one <- function(df) {
  df |> summarise(
    med_k = median(Wert[Rezidiv == "Kein Rezidiv"], na.rm = TRUE),
    p25_k = quantile(Wert[Rezidiv == "Kein Rezidiv"], 0.25, na.rm = TRUE),
    p75_k = quantile(Wert[Rezidiv == "Kein Rezidiv"], 0.75, na.rm = TRUE),
    n_k   = sum(Rezidiv == "Kein Rezidiv"),
    med_r = median(Wert[Rezidiv == "Rezidiv"],      na.rm = TRUE),
    p25_r = quantile(Wert[Rezidiv == "Rezidiv"],    0.25, na.rm = TRUE),
    p75_r = quantile(Wert[Rezidiv == "Rezidiv"],    0.75, na.rm = TRUE),
    n_r   = sum(Rezidiv == "Rezidiv"),
    p_mwu = tryCatch(
      wilcox.test(Wert ~ Rezidiv, exact = FALSE)$p.value,
      error = function(e) NA_real_),
    .groups = "drop")
}

snap_summary <- bind_rows(
  lab_snap_long |> group_by(Lab, Fenster) |> summarise_one(),
  lab_snap_long |> group_by(Lab) |> summarise_one() |>
    mutate(Fenster = factor("Σ POD 1-14",
                            levels = c("POD 1-3","POD 4-7","POD 8-14",
                                       "Σ POD 1-14")))
) |>
  mutate(Fenster = factor(as.character(Fenster),
                          levels = c("POD 1-3","POD 4-7","POD 8-14",
                                     "Σ POD 1-14"))) |>
  arrange(Lab, Fenster)

snap_tab <- snap_summary |>
  transmute(
    Labor   = Lab, Fenster,
    `Kein Rez. n`        = n_k,
    `Kein Rez. Med (IQR)` = sprintf("%.1f (%.1f; %.1f)", med_k, p25_k, p75_k),
    `Rezidiv n`          = n_r,
    `Rezidiv Med (IQR)`  = sprintf("%.1f (%.1f; %.1f)", med_r, p25_r, p75_r),
    p = if_else(is.na(p_mwu), "--",
                if_else(p_mwu < 0.001, "<0.001",
                        if_else(p_mwu > 0.999, ">0.999",
                                sprintf("%.3f", p_mwu))))
  )

if (FALSE) write_table_tex(
  snap_tab,
  file.path(fig_dir, "Tab_Labor_Snapshots_Landscape.tex"),
  caption = paste("Labor-Snapshots POD 0-14 nach Rezidiv-Status",
                  "(Analyse-Kohorte). Median (IQR) und Mann-Whitney-p pro",
                  "(Labor × Zeitfenster). Tests Abb 3.6a numerisch ab."),
  align = "llrlrlr"
)


# ── Abb 3.7 — Regeneration 2D: Δ TLV vs. FLV gegen die sieben Labor-Steigungen ──
regen_slopes <- c(slope_GOT_POD1_7      = "GOT (U/L/Tag)",
                  slope_GPT_POD1_7      = "GPT (U/L/Tag)",
                  slope_CRP_POD1_7      = "CRP (mg/L/Tag)",
                  slope_Albumin_POD1_7  = "Albumin (g/L/Tag)",
                  slope_Quick_POD1_7    = "Quick (%/Tag)",
                  slope_Leukos_POD1_7   = "Leukozyten (/nL/Tag)",
                  slope_Thrombos_POD1_7 = "Thrombozyten (/nL/Tag)")
regen2d <- static_an |>
  select(Pseudonym, D_rezidiv, x = d_TLV_rel_FLV_base_first_postop,
         all_of(names(regen_slopes))) |>
  pivot_longer(all_of(names(regen_slopes)), names_to = "lab", values_to = "y") |>
  filter(!is.na(x), !is.na(y)) |>
  mutate(Rezidiv = factor(if_else(D_rezidiv == 1, "Rezidiv", "Kein Rezidiv"),
                          levels = c("Kein Rezidiv", "Rezidiv")),
         Labor = factor(regen_slopes[lab], levels = regen_slopes))
regen2d_stats <- regen2d |>
  group_by(Labor) |>
  summarise(n   = n(),
            rho = cor(x, y, method = "spearman"),
            p   = suppressWarnings(cor.test(x, y, method = "spearman"))$p.value,
            ylo = quantile(y, 0.01), yhi = quantile(y, 0.99), .groups = "drop") |>
  mutate(titel = sprintf("%s\nSpearman ρ = %s, p = %s, n = %d", Labor,
                         sub(".", ",", sprintf("%.2f", rho), fixed = TRUE),
                         ifelse(p < 0.001, "< 0,001",
                                sub(".", ",", sprintf("%.3f", p), fixed = TRUE)), n))
regen2d <- regen2d |>
  left_join(regen2d_stats |> select(Labor, ylo, yhi, titel), by = "Labor") |>
  mutate(im_ausschnitt = x >= -80 & x <= 150 & y >= ylo & y <= yhi,
         titel = factor(titel, levels = regen2d_stats$titel))

cat(sprintf(paste0("[05] Abb_Regeneration_2D: %d Patient*innen mit Volumenaenderung ",
                   "und mindestens einer Steigung; n je Panel %d bis %d; ",
                   "%d Punkte ausserhalb des Ausschnitts (x -80 bis 150, ",
                   "y je Panel 1. bis 99. Perzentil)\n"),
            dplyr::n_distinct(regen2d$Pseudonym), min(regen2d_stats$n),
            max(regen2d_stats$n), sum(!regen2d$im_ausschnitt)))
for (i in seq_len(nrow(regen2d_stats)))
  cat(sprintf("[05] Abb_Regeneration_2D: %-22s n = %d, Spearman rho = %.2f, p = %.3f\n",
              regen2d_stats$Labor[i], regen2d_stats$n[i],
              regen2d_stats$rho[i], regen2d_stats$p[i]))

p_2d <- ggplot(regen2d |> filter(im_ausschnitt),
               aes(x = x, y = y, colour = Rezidiv)) +
  geom_point(alpha = 0.55, size = 1.1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  scale_colour_manual(values = c("Kein Rezidiv" = col_blue,
                                 "Rezidiv"      = col_red)) +
  facet_wrap(~ titel, scales = "free_y", ncol = 2) +
  labs(x = "Δ TLV vs. FLV (%) — volumetrisch",
       y = "Steigung POD 1–7 — funktionell",
       colour = NULL) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "top",
        strip.text = element_text(size = 9))

ggsave(file.path(fig_dir, "Abb_Regeneration_2D_Volume_vs_Labor.png"),
       plot = p_2d, width = 8, height = 11, dpi = 300, bg = "white")


message("Stage 5 done. Wrote: Tab_3_1, Tab_3_2, Abb_3_1, ",
        "Tab_3_3, Abb_3_2 (KM TTR), Abb_3_3 (KM OS), Abb_3_4 (Histogramm), ",
        "Abb_3_5 (Spaghetti), Abb_3_6a/b/c/d (Labor), Abb_3_7 (Box/Violin), ",
        "Tab_3_4, Abb_Regeneration_2D.")

local({
  ds <- readxl::read_xlsx("01_Daten/data_static.xlsx") |>
    dplyr::filter(is.na(Rezidivtumor) |
                    tolower(as.character(Rezidivtumor)) != "ja")

  n_below <- sum(ds$TLV_first_postop < ds$FLV, na.rm = TRUE)
  cat(sprintf("[textdiag] TLV 1. postop < FLV praeop: %d von %d (%.1f %%)\n",
              n_below, nrow(ds), 100 * n_below / nrow(ds)))

  ok <- !is.na(ds$d_TLV_rel_FLV_base_first_postop) & !is.na(ds$slope_Quick_POD1_7)
  ct <- suppressWarnings(cor.test(ds$d_TLV_rel_FLV_base_first_postop[ok],
                                  ds$slope_Quick_POD1_7[ok],
                                  method = "spearman"))
  cat(sprintf("[textdiag] Regen-2D Spearman rho=%.2f, p=%.3f (n=%d)\n",
              unname(ct$estimate), ct$p.value, sum(ok)))

  w <- ds[!is.na(ds$LVR_nam), ]
  cat(sprintf(paste0("[textdiag] Fenster POD 91-180: n=%d, Events=%d ",
                     "(Rate %.1f %% vs. %.1f %% Gesamtkohorte)\n"),
              nrow(w), sum(w$D_rezidiv == 1, na.rm = TRUE),
              100 * mean(w$D_rezidiv == 1, na.rm = TRUE),
              100 * mean(ds$D_rezidiv == 1, na.rm = TRUE)))
  bb <- w[!is.na(w$d_TLV_rel_FLV_base_first_postop), ]
  same <- bb$POD_first_postop_volumetry == bb$POD_nam_91_180
  r_all  <- cor(bb$d_TLV_rel_FLV_base_first_postop, bb$LVR_nam,
                method = "spearman")
  r_diff <- cor(bb$d_TLV_rel_FLV_base_first_postop[!same], bb$LVR_nam[!same],
                method = "spearman")
  cat(sprintf(paste0("[textdiag] Spearman Schnappschuss vs. Nam-Fenster: ",
                     "r=%.2f (n=%d; %d dieselbe Messung); ",
                     "nur verschiedene Messungen r=%.2f (n=%d)\n"),
              r_all, nrow(bb), sum(same), r_diff, sum(!same)))

  dyn <- readxl::read_xlsx("01_Daten/data_dynamic.xlsx")
  num <- function(x) suppressWarnings(as.numeric(gsub(",", ".",
                                                      as.character(x))))
  lab <- dyn |>
    dplyr::filter(datasource == "labor", !is.na(lab_date_POD_imp),
                  lab_date_POD_imp >= 1, lab_date_POD_imp <= 7,
                  is.na(Rezidivtumor) |
                    tolower(as.character(Rezidivtumor)) != "ja")
  r2_one <- function(x, y) {
    ok <- !is.na(x) & !is.na(y)
    if (sum(ok) < 3 || var(x[ok]) == 0 || var(y[ok]) == 0) return(NA_real_)
    cor(x[ok], y[ok])^2
  }
  slope_one <- function(x, y) {
    ok <- !is.na(x) & !is.na(y)
    if (sum(ok) < 3 || var(x[ok]) == 0) return(NA_real_)
    cov(x[ok], y[ok]) / var(x[ok])
  }
  for (m in c("Quick", "GOT", "GPT", "Albumin", "CRP", "Leukos", "Thrombos")) {
    v <- num(lab[[m]])
    r2 <- lab |>
      dplyr::mutate(.v = v) |>
      dplyr::group_by(Pseudonym) |>
      dplyr::summarise(r2 = r2_one(lab_date_POD_imp, .v), .groups = "drop") |>
      dplyr::filter(!is.na(r2))
    cat(sprintf("[textdiag] Median-R2 Steigungsgerade %-7s: %.2f (n=%d)\n",
                m, median(r2$r2), nrow(r2)))
  }
  crp <- lab |>
    dplyr::mutate(.v = num(CRP)) |>
    dplyr::filter(!is.na(.v))
  peak <- crp |>
    dplyr::group_by(POD = lab_date_POD_imp) |>
    dplyr::summarise(med = median(.v), .groups = "drop")
  cat(sprintf("[textdiag] CRP-Gipfel: POD %d (Median %.0f mg/L)\n",
              peak$POD[which.max(peak$med)], max(peak$med)))
  ras <- crp |>
    dplyr::group_by(Pseudonym) |>
    dplyr::summarise(days = paste(sort(unique(lab_date_POD_imp)),
                                  collapse = "/"),
                     slope = slope_one(lab_date_POD_imp, .v),
                     .groups = "drop") |>
    dplyr::filter(!is.na(slope))
  for (pat in c("1/3/5/7", "1/2/4/7")) {
    g <- ras[ras$days == pat, ]
    cat(sprintf("[textdiag] CRP-Slope Raster %s: Median %+0.2f (n=%d)\n",
                pat, median(g$slope), nrow(g)))
  }
})
