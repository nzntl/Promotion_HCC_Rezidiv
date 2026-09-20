# 02_Meta_and_Static_Analysis.R -- Uebersichtstabellen zu Datensatz, Kohorte und Volumetrie.
# Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx, data_dynamic_surv.xlsx.

pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "ggplot2",
          "wesanderson", "scales")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")

library(readxl); library(dplyr); library(tidyr); library(stringr)
library(ggplot2); library(wesanderson); library(scales)


# ── 0. Load inputs (already eligible-filtered in stage 1) ──

static_eligible  <- read_excel("./01_Daten/data_static.xlsx")
dynamic_eligible <- read_excel("./01_Daten/data_dynamic.xlsx")

analysis_pseudonyms <- static_eligible |>
  filter(is.na(Rezidivtumor) | str_to_lower(Rezidivtumor) != "ja") |>
  pull(Pseudonym)

static_analysis  <- static_eligible  |> filter(Pseudonym %in% analysis_pseudonyms)
dynamic_analysis <- dynamic_eligible |> filter(Pseudonym %in% analysis_pseudonyms)

fig_dir <- "./03_Tables_Figures"
tex_dir <- "./03_Tables_Figures"


# ── 2. FU-Dauer-Verteilung nach Rezidivstatus ──

fu_dur <- static_eligible |>
  filter(!is.na(FU_duration_days), FU_duration_days >= 0) |>
  mutate(Rezidiv = factor(if_else(D_rezidiv == 1, "Rezidiv", "Kein Rezidiv"),
                          levels = c("Kein Rezidiv", "Rezidiv")),
         FU_Jahre = FU_duration_days / 365.25)

p_mwu <- tryCatch(
  wilcox.test(FU_Jahre ~ Rezidiv, data = fu_dur, exact = FALSE)$p.value,
  error = function(e) NA_real_)
p_lbl <- if (is.na(p_mwu)) "p = NA" else if (p_mwu < 0.001) "p < 0.001" else
  sprintf("p = %.3f", p_mwu)

p_fu <- ggplot(fu_dur, aes(x = Rezidiv, y = FU_Jahre, fill = Rezidiv)) +
  geom_violin(alpha = 0.55, colour = NA, scale = "width") +
  geom_boxplot(width = 0.18, outlier.size = 0.7) +
  scale_fill_manual(values = c("Kein Rezidiv" = "#3B6FAB",
                               "Rezidiv"      = "#C7261B"),
                    guide = "none") +
  annotate("text", x = 1.5, y = max(fu_dur$FU_Jahre, na.rm = TRUE) * 1.02,
           label = sprintf("Mann-Whitney %s", p_lbl),
           size = 3.4, colour = "grey25") +
  labs(x = NULL, y = "Jahre seit OP") +
  theme_minimal(base_family = "sans") +
  theme(plot.subtitle = element_text(size = 10, colour = "grey40"))

if (FALSE) ggsave(file.path(fig_dir, "Abb_FU_Duration_by_Rezidiv.png"), plot = p_fu,
       width = 7.5, height = 5.5, dpi = 300, bg = "white")


# ── 3. Helper: write a tabular fragment with LaTeX escaping ──

tex_escape_cell <- function(x) {
  x <- as.character(x); x[is.na(x)] <- "--"
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x); x
}

write_table_tex_mc <- function(tbl, path, caption = NULL, align,
                               groups = NULL,
                               midrule_after_pattern = NULL) {
  header_data <- paste(tex_escape_cell(names(tbl)), collapse = " & ")
  out <- c(if (!is.null(caption)) sprintf("%% %s", caption),
           sprintf("\\begin{tabular}{%s}", align), "\\toprule")
  if (!is.null(groups)) {
    grp_cells <- vapply(groups, function(g) {
      lbl <- tex_escape_cell(g[[1]]); n <- as.integer(g[[2]])
      if (n == 1) lbl else sprintf("\\multicolumn{%d}{c}{%s}", n, lbl)
    }, character(1))
    out <- c(out, paste0(paste(grp_cells, collapse = " & "), " \\\\"),
             "\\midrule")
  }
  out <- c(out, paste0(header_data, " \\\\"), "\\midrule")
  body_rows <- apply(tbl, 1, function(r) {
    paste(tex_escape_cell(r), collapse = " & ")
  })
  for (i in seq_along(body_rows)) {
    out <- c(out, paste0(body_rows[i], " \\\\"))
    if (!is.null(midrule_after_pattern) &&
        any(grepl(midrule_after_pattern,
                  as.character(unlist(tbl[i, ]))))) {
      out <- c(out, "\\midrule")
    }
  }
  out <- c(out, "\\bottomrule", "\\end{tabular}")
  con <- file(path, "w", encoding = "UTF-8"); writeLines(out, con); close(con)
}

write_table_tex <- function(tbl, path, caption = NULL,
                            align = NULL) {
  cols <- ncol(tbl)
  if (is.null(align)) align <- paste0(c("l", rep("r", cols - 1)), collapse = "")
  header <- paste(tex_escape_cell(names(tbl)), collapse = " & ")
  body <- apply(tbl, 1, function(row) {
    paste(tex_escape_cell(row), collapse = " & ")
  })
  body <- paste(body, collapse = " \\\\\n")
  out <- c(
    if (!is.null(caption)) sprintf("%% %s", caption),
    sprintf("\\begin{tabular}{%s}", align),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(body, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )
  con <- file(path, "w", encoding = "UTF-8"); writeLines(out, con); close(con)
}


# ── 4. Therapie pro Rezidiv-Ordinalzahl — Kontingenztabelle ──

rez_indexed <- dynamic_analysis |>
  filter(datasource %in% c("rezidiv", "Rezidiv_Therapie")) |>
  arrange(Pseudonym, lab_date_POD_imp, desc(datasource == "rezidiv")) |>
  group_by(Pseudonym) |>
  mutate(
    .rez_key = if_else(datasource == "rezidiv",
                       as.character(lab_date_POD_imp), NA_character_),
    rez_idx  = pmax(cumsum(datasource == "rezidiv" &
                             !duplicated(.rez_key, incomparables = NA)), 1L)
  ) |>
  select(-.rez_key) |>
  ungroup()

therapy_per_event <- rez_indexed |>
  filter(datasource == "Rezidiv_Therapie", rez_idx > 0,
         !is.na(rezidiv_therapie_cat)) |>
  group_by(Pseudonym, rez_idx) |>
  slice(1) |>
  ungroup() |>
  select(Pseudonym, rez_idx, rezidiv_therapie_cat)

make_block <- function(idx) {
  ifelse(idx <= 4L,
         as.character(idx),
         {
           lo <- 5L + 5L * ((idx - 5L) %/% 5L)
           sprintf("%d–%d", lo, lo + 4L)
         })
}
block_key <- function(idx) ifelse(idx <= 4L, idx,
                                  5L + 5L * ((idx - 5L) %/% 5L))

therapy_cats <- c("Resektion", "Ablation", "Transarteriell",
                  "Bestrahlung", "Systemtherapie", "LTX",
                  "Studie", "Keine", "Unbekannt")

ev <- therapy_per_event |>
  mutate(rez_block = make_block(rez_idx),
         key       = block_key(rez_idx),
         rezidiv_therapie_cat = factor(rezidiv_therapie_cat,
                                       levels = therapy_cats))

agg <- ev |>
  count(key, rez_block, rezidiv_therapie_cat, name = "n_t",
        .drop = FALSE) |>
  filter(!is.na(rez_block))

wide <- agg |>
  pivot_wider(names_from = rezidiv_therapie_cat, values_from = n_t,
              values_fill = 0) |>
  arrange(key)

therapy_present <- therapy_cats[
  vapply(therapy_cats, function(t) t %in% names(wide) && sum(wide[[t]]) > 0,
         logical(1))]

wide_out <- wide |>
  mutate(Gesamt = rowSums(across(all_of(therapy_present)))) |>
  select(`Rezidiv (n-tes)` = rez_block, all_of(therapy_present), Gesamt)

total_row <- wide_out |>
  summarise(across(all_of(c(therapy_present, "Gesamt")), sum)) |>
  mutate(`Rezidiv (n-tes)` = "Gesamt") |>
  select(`Rezidiv (n-tes)`, all_of(therapy_present), Gesamt)
wide_out <- bind_rows(wide_out, total_row)

if (FALSE) write_table_tex(
  wide_out,
  file.path(tex_dir, "Tab_Therapie_x_Rezidivindex_Landscape.tex"),
  caption = paste(
    "Therapie pro Rezidiv-Ordinalzahl (Zellen = Anzahl der Rezidiv-Ereignisse,",
    "die mit der jeweiligen Therapie versorgt wurden)."),
  align = paste0("l", paste(rep("r", ncol(wide_out) - 1), collapse = ""))
)


# ── 5. Recurrence-rate-by-Segment KM curves ──

library(survival)
surv_seg <- read_excel("./01_Daten/data_dynamic_surv.xlsx") |>
  filter(Pseudonym %in% analysis_pseudonyms) |>
  left_join(static_analysis |>
              select(Pseudonym, D_OP_seg_1, D_OP_seg_4, D_OP_seg_2_3),
            by = "Pseudonym")

km_curves <- list()
for (lbl in c("Gesamt (alle Resektionen)", "Segment 1",
              "Segment 4 (4/4a/4b)", "Segmente 2 & 3")) {
  d <- switch(lbl,
              "Gesamt (alle Resektionen)" = surv_seg,
              "Segment 1"                 = surv_seg |> filter(D_OP_seg_1 == 1),
              "Segment 4 (4/4a/4b)"       = surv_seg |> filter(D_OP_seg_4 == 1),
              "Segmente 2 & 3"            = surv_seg |> filter(D_OP_seg_2_3 == 1))
  if (nrow(d) < 5) next
  f <- survfit(Surv(time, event) ~ 1, data = d)
  km_curves[[lbl]] <- data.frame(
    Gruppe = lbl, time = f$time, surv = f$surv,
    lower  = f$lower, upper = f$upper, n_risk = f$n.risk
  )
}
km_df <- bind_rows(km_curves) |>
  mutate(Gruppe   = factor(Gruppe, levels = names(km_curves)),
         Rez_pct  = (1 - surv) * 100,
         Monate   = time / 30.4375)

pal_seg <- c("Gesamt (alle Resektionen)" = "#3B6FAB",
             "Segment 1"                 = "#7B3F99",
             "Segment 4 (4/4a/4b)"       = "#E1A11A",
             "Segmente 2 & 3"            = "#C7261B")

p_segrez <- ggplot(km_df, aes(x = Monate, y = Rez_pct, colour = Gruppe)) +
  geom_step(linewidth = 1.0) +
  geom_ribbon(aes(ymin = (1 - upper) * 100, ymax = (1 - lower) * 100,
                  fill = Gruppe), alpha = 0.10, colour = NA) +
  scale_colour_manual(values = pal_seg) +
  scale_fill_manual(values = pal_seg, guide = "none") +
  coord_cartesian(xlim = c(0, 60)) +
  labs(x = "Monate nach OP", y = "Rezidivwahrscheinlichkeit (%)",
       colour = NULL) +
  theme_minimal(base_family = "sans") +
  theme(plot.subtitle = element_text(size = 9, colour = "grey40"),
        legend.position = "top")

if (FALSE) ggsave(file.path(fig_dir, "Abb_Rezidivinzidenz_Segment.png"),
       plot = p_segrez, width = 10, height = 6.5, dpi = 300, bg = "white")

early_share <- static_analysis |>
  mutate(`Gesamt (alle Resektionen)` = TRUE,
         `Segment 1`              = D_OP_seg_1   == 1,
         `Segment 4 (4/4a/4b)`    = D_OP_seg_4   == 1,
         `Segmente 2 & 3`         = D_OP_seg_2_3 == 1) |>
  pivot_longer(c(`Gesamt (alle Resektionen)`, `Segment 1`,
                 `Segment 4 (4/4a/4b)`, `Segmente 2 & 3`),
               names_to = "Gruppe", values_to = "in_group") |>
  filter(in_group) |>
  mutate(Bucket = factor(case_when(
           is.na(time_to_first_rezidiv_days) ~ "kein Rezidiv",
           time_to_first_rezidiv_days <= 90  ~ "0-3 Mo",
           time_to_first_rezidiv_days <= 365 ~ "3-12 Mo",
           time_to_first_rezidiv_days <= 730 ~ "12-24 Mo",
           TRUE                              ~ "> 24 Mo"),
           levels = c("0-3 Mo","3-12 Mo","12-24 Mo","> 24 Mo","kein Rezidiv"))) |>
  count(Gruppe, Bucket) |>
  group_by(Gruppe) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  mutate(Gruppe = factor(Gruppe, levels = c("Gesamt (alle Resektionen)",
                                            "Segment 1",
                                            "Segment 4 (4/4a/4b)",
                                            "Segmente 2 & 3")))

bucket_pal <- c("0-3 Mo"="#C7261B","3-12 Mo"="#E1A11A",
                "12-24 Mo"="#7B3F99","> 24 Mo"="#3B6FAB",
                "kein Rezidiv"="grey75")

p_bucket <- ggplot(early_share, aes(x = Gruppe, y = pct, fill = Bucket)) +
  geom_col(position = "stack", colour = "white", linewidth = 0.3) +
  geom_text(data = early_share |> filter(Bucket == "0-3 Mo"),
            aes(label = sprintf("%.0f%%", pct)),
            position = position_stack(vjust = 0.5),
            colour = "white", fontface = "bold", size = 3.4) +
  scale_fill_manual(values = bucket_pal) +
  labs(x = NULL, y = "Anteil Patient*innen (%)", fill = "Erstrezidiv-Bucket") +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 12, hjust = 1))

if (FALSE) ggsave(file.path(fig_dir, "Abb_Frührezidiv_Buckets_Segment.png"),
       plot = p_bucket, width = 10, height = 6, dpi = 300, bg = "white")

regen_for_tab <- bind_rows(
  dynamic_analysis |> filter(datasource == "volumetry",
                             !is.na(lab_date_POD_imp),
                             lab_date_POD_imp > 0,
                             lab_date_POD_imp <= 365.25 * 2,
                             !is.na(d_TLV_rel_FLV_base)) |>
    mutate(Gruppe = "Gesamt (alle Resektionen)"),
  dynamic_analysis |> filter(datasource == "volumetry",
                             D_OP_seg_1 == 1,
                             !is.na(lab_date_POD_imp), lab_date_POD_imp > 0,
                             lab_date_POD_imp <= 365.25 * 2,
                             !is.na(d_TLV_rel_FLV_base)) |>
    mutate(Gruppe = "Segment 1"),
  dynamic_analysis |> filter(datasource == "volumetry",
                             D_OP_seg_4 == 1,
                             !is.na(lab_date_POD_imp), lab_date_POD_imp > 0,
                             lab_date_POD_imp <= 365.25 * 2,
                             !is.na(d_TLV_rel_FLV_base)) |>
    mutate(Gruppe = "Segment 4"),
  dynamic_analysis |> filter(datasource == "volumetry",
                             D_OP_seg_2_3 == 1,
                             !is.na(lab_date_POD_imp), lab_date_POD_imp > 0,
                             lab_date_POD_imp <= 365.25 * 2,
                             !is.na(d_TLV_rel_FLV_base)) |>
    mutate(Gruppe = "Segmente 2 & 3")
) |>
  mutate(Monate = lab_date_POD_imp / 30.4375,
         Fenster = factor(case_when(
           Monate <= 3  ~ "0-3 Mo",
           Monate <= 6  ~ "3-6 Mo",
           Monate <= 12 ~ "6-12 Mo",
           Monate <= 24 ~ "12-24 Mo"),
           levels = c("0-3 Mo","3-6 Mo","6-12 Mo","12-24 Mo")))

per_win <- regen_for_tab |>
  filter(!is.na(Fenster)) |>
  group_by(Gruppe, Fenster) |>
  summarise(
    n_pat       = n_distinct(Pseudonym),
    n_rez       = n_distinct(Pseudonym[D_rezidiv == 1]),
    median_dtlv = round(median(d_TLV_rel_FLV_base, na.rm = TRUE), 1),
    .groups = "drop")

per_grp <- regen_for_tab |>
  filter(!is.na(Fenster)) |>
  group_by(Gruppe) |>
  summarise(
    n_pat       = n_distinct(Pseudonym),
    n_rez       = n_distinct(Pseudonym[D_rezidiv == 1]),
    median_dtlv = round(median(d_TLV_rel_FLV_base, na.rm = TRUE), 1),
    .groups = "drop") |>
  mutate(Fenster = factor("Σ 0-24 Mo",
                          levels = c("0-3 Mo","3-6 Mo","6-12 Mo","12-24 Mo",
                                     "Σ 0-24 Mo")))

regen_tab <- bind_rows(per_win, per_grp) |>
  mutate(Gruppe  = factor(Gruppe, levels = c("Gesamt (alle Resektionen)",
                                             "Segment 1", "Segment 4",
                                             "Segmente 2 & 3")),
         Fenster = factor(as.character(Fenster),
                          levels = c("0-3 Mo","3-6 Mo","6-12 Mo","12-24 Mo",
                                     "Σ 0-24 Mo"))) |>
  arrange(Gruppe, Fenster) |>
  dplyr::filter(Gruppe == "Gesamt (alle Resektionen)") |>
  transmute(`Messfenster (Monate)` = gsub("-", "–", sub(" Mo$", "", as.character(Fenster))),
            `Gemessen (n)`           = n_pat,
            `mit Rezidiv (n)`        = n_rez,
            `Anteil`                 = sprintf("%.1f%%", 100 * n_rez / n_pat),
            `Δ TLV vs. FLV (Median)` = sprintf("%+0.1f%%", median_dtlv))

.alt_seg <- file.path(tex_dir, "Tab_TLV_Regeneration_Segment_Landscape.tex")
if (file.exists(.alt_seg)) { file.remove(.alt_seg); rm(.alt_seg) }

local({
  ges <- regen_for_tab |> dplyr::filter(Gruppe == "Gesamt (alle Resektionen)",
                                        !is.na(Fenster))
  n_tab <- dplyr::n_distinct(ges$Pseudonym)
  n_koh <- dplyr::n_distinct(static_analysis$Pseudonym)
  mess24 <- dynamic_analysis |>
    dplyr::filter(datasource == "volumetry", !is.na(lab_date_POD_imp),
                  lab_date_POD_imp > 0, lab_date_POD_imp <= 365.25 * 2)
  n_mess <- dplyr::n_distinct(mess24$Pseudonym)
  frueh <- unique(ges$Pseudonym[ges$Fenster == "0-3 Mo"])
  spaet <- setdiff(unique(ges$Pseudonym), frueh)
  rez_s <- dplyr::n_distinct(ges$Pseudonym[ges$Pseudonym %in% spaet & ges$D_rezidiv == 1])
  cat(sprintf(paste0("[02] Tab Volumetrie-Verfuegbarkeit: %d von %d der Kohorte; ",
                     "%d mit Messung 0-24 Mo, davon %d ohne FLV-Bezug; ",
                     "erst nach 3 Mo gemessen %d, davon %d mit Rezidiv (%.1f %%)
"),
              n_tab, n_koh, n_mess, n_mess - n_tab,
              length(spaet), rez_s, 100 * rez_s / length(spaet)))
})

write_table_tex_mc(
  regen_tab,
  file.path(tex_dir, "Tab_Volumetrie_Verfuegbarkeit.tex"),
  caption = paste("Verfügbarkeit der postoperativen Volumetrie nach Messfenster.",
                  "Ausgewiesen ist, wie viele Patient*innen im jeweiligen Fenster",
                  "gemessen wurden und welcher Anteil von ihnen im gesamten Verlauf",
                  "ein Rezidiv entwickelte."),
  align = "lrrrr",
  midrule_after_pattern = "^Σ 0–24$"
)


# ── 6. Numerical-moments table at the patient level ──

num_vars <- c(
  "Alter", "BMI", "KGkg", "FU_duration_days",
  "ChildPugh_Punkte", "Tumordurchmesser",
  "ND_count", "ND_Nikotin_py",
  "FLV", "TLV_preop", "TLV_first_postop",
  "d_TLV_rel_first_postop", "d_TLV_rel_FLV_base_first_postop",
  "n_rezidiv", "time_to_first_rezidiv_days",
  "n_postop_volumetrien", "n_rezidiv_therapie",
  "POD_first_postop_volumetry"
)
num_vars <- intersect(num_vars, names(static_eligible))

phase_tag <- c(
  Alter = "präop", BMI = "präop", KGkg = "präop",
  FU_duration_days = "postop",
  ChildPugh_Punkte = "präop", Tumordurchmesser = "präop",
  ND_count = "präop", ND_Nikotin_py = "präop",
  FLV = "präop", TLV_preop = "präop",
  TLV_first_postop = "postop",
  d_TLV_rel_first_postop = "postop",
  d_TLV_rel_FLV_base_first_postop = "postop",
  n_rezidiv = "postop", time_to_first_rezidiv_days = "postop",
  n_postop_volumetrien = "postop", n_rezidiv_therapie = "postop",
  POD_first_postop_volumetry = "postop"
)

label_recode <- c(
  Alter = "Alter (Jahre)",
  BMI   = "BMI (kg/m²)",
  KGkg  = "Körpergewicht (kg)",
  FU_duration_days = "FU-Dauer (Tage)",
  ChildPugh_Punkte = "Child-Pugh Punkte",
  Tumordurchmesser = "Tumordurchmesser (cm)",
  ND_count = "Anzahl Nebendiagnosen",
  ND_Nikotin_py = "Pack-Years (Nikotin)",
  FLV = "FLV präop (cm³)",
  TLV_preop = "TLV präop (cm³)",
  TLV_first_postop = "TLV 1. postop (cm³)",
  d_TLV_rel_first_postop = "Δ TLV vs. 1. Volumetrie (%)",
  d_TLV_rel_FLV_base_first_postop = "Δ TLV vs. FLV (%)",
  n_rezidiv = "Anzahl Rezidive",
  time_to_first_rezidiv_days = "Tage OP bis 1. Rezidiv",
  n_postop_volumetrien = "Anzahl postop Volumetrien",
  n_rezidiv_therapie = "Anzahl Therapie-Ereignisse",
  POD_first_postop_volumetry = "POD 1. postop Volumetrie"
)

build_moments <- function(df, num_vars) {
  df |>
    select(all_of(num_vars)) |>
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) |>
    pivot_longer(everything(), names_to = "Variable", values_to = "value") |>
    group_by(Variable) |>
    summarise(
      N      = sum(!is.na(value)),
      mean   = round(mean(value, na.rm = TRUE), 2),
      sd     = round(sd(value,   na.rm = TRUE), 2),
      p25    = round(quantile(value, 0.25, na.rm = TRUE), 2),
      median = round(median(value, na.rm = TRUE), 2),
      p75    = round(quantile(value, 0.75, na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    mutate(Variable = factor(Variable, levels = num_vars)) |>
    arrange(Variable) |>
    mutate(Phase    = phase_tag[as.character(Variable)],
           Variable = label_recode[as.character(Variable)]) |>
    relocate(Phase, .after = Variable)
}

moments_overall <- build_moments(static_analysis, num_vars)
if (FALSE) write_table_tex(
  moments_overall,
  file.path(tex_dir, "Tab_Numerical_Moments_Static_a_Overall.tex"),
  caption = sprintf(paste("Numerische Variablen — Verteilungsmomente gesamt",
                          "(Analyse-Kohorte, n=%d)"), nrow(static_analysis))
)


# ── Therapy cascade tables (Tab 3.22 Rezidivort, Tab 3.23 Therapy) ──

therapy_bucket_map <- c(
  "Resektion"       = "Lokal-kurativ",
  "Ablation"        = "Lokal-kurativ",
  "LTX"             = "Lokal-kurativ",
  "Transarteriell"  = "Lokoregional-palliativ",
  "Bestrahlung"     = "Lokoregional-palliativ",
  "Systemtherapie"  = "Systemisch",
  "Studie"          = "Systemisch"
)

loc_levels <- c("Intrahepatisch", "Intra- & Extrahepatisch", "Lymphknoten",
                "Peritoneal/Andere", "Pulmonal", "Ossär", "Cerebral")

rez_first <- static_analysis |>
  filter(!is.na(D_rezidiv_cat_first)) |>
  mutate(D_rezidiv_cat_first = dplyr::recode(as.character(D_rezidiv_cat_first),
           "Intra + extrahepatisch" = "Intra- & Extrahepatisch",
           "Peritoneal_Andere" = "Peritoneal/Andere",
           "Ossaer"            = "Ossär"),
         D_rezidiv_cat_first = factor(D_rezidiv_cat_first,
                                      levels = loc_levels)) |>
  count(`Lokalisation 1. Rezidiv` = D_rezidiv_cat_first, name = "n") |>
  mutate(`% der Rezidive` = sprintf("%.1f%%", 100 * n / sum(n)),
         n = as.character(n))

write_table_tex(
  rez_first,
  file.path(tex_dir, "Tab_3_22_Rezidivort_Verteilung.tex"),
  caption = paste(
    "Lokalisation des ersten Rezidivs (Analyse-Kohorte, nur Patient*innen",
    "mit dokumentiertem Rezidiv). Intra + extrahepatisch bezeichnet",
    "Erstrezidive mit gleichzeitig intra- und extrahepatischen Herden."),
  align = "lrr"
)

rez_indexed_full <- dynamic_analysis |>
  filter(datasource %in% c("rezidiv", "Rezidiv_Therapie")) |>
  arrange(Pseudonym, lab_date_POD_imp, desc(datasource == "rezidiv")) |>
  group_by(Pseudonym) |>
  mutate(
    .rez_key = if_else(datasource == "rezidiv",
                       as.character(lab_date_POD_imp), NA_character_),
    rez_idx  = pmax(cumsum(datasource == "rezidiv" &
                             !duplicated(.rez_key, incomparables = NA)), 1L)
  ) |>
  select(-.rez_key) |>
  ungroup()

therapy_bucketed <- rez_indexed_full |>
  filter(datasource == "Rezidiv_Therapie",
         !is.na(rezidiv_therapie_cat),
         rez_idx > 0,
         !rezidiv_therapie_cat %in% c("Keine", "Unbekannt")) |>
  group_by(Pseudonym, rez_idx) |>
  slice(1) |>
  ungroup() |>
  mutate(therapy_bucket = factor(therapy_bucket_map[as.character(rezidiv_therapie_cat)],
                                 levels = c("Lokal-kurativ","Lokoregional-palliativ",
                                            "Systemisch")))

tab_323 <- therapy_bucketed |>
  filter(rez_idx == 1) |>
  count(`Therapie nach 1. Rezidiv` = therapy_bucket, name = "n", .drop = FALSE) |>
  mutate(`%` = sprintf("%.1f%%", 100 * n / sum(n)),
         n = as.character(n),
         `Therapie nach 1. Rezidiv` = as.character(`Therapie nach 1. Rezidiv`))

pat_bucket <- unique(therapy_bucketed$Pseudonym[therapy_bucketed$rez_idx == 1])
pat_keine  <- setdiff(
  unique(rez_indexed_full$Pseudonym[
    rez_indexed_full$datasource == "Rezidiv_Therapie" &
      rez_indexed_full$rez_idx == 1 &
      !is.na(rez_indexed_full$rezidiv_therapie_cat) &
      as.character(rez_indexed_full$rezidiv_therapie_cat) == "Keine"]),
  pat_bucket)

n_erstrez <- sum(static_analysis$D_rezidiv == 1, na.rm = TRUE)
n_dok     <- length(pat_bucket)
n_keine   <- length(pat_keine)
n_ohne    <- n_erstrez - n_dok - n_keine

cat(sprintf(paste0("[02] Tab Therapie Erstrezidiv: %d Erstrezidive; %d mit ",
                   "dokumentierter Therapie, %d dokumentiert ohne Therapie, ",
                   "%d ohne Angabe\n"),
            n_erstrez, n_dok, n_keine, n_ohne))

tab_323 <- bind_rows(
  tab_323,
  data.frame(`Therapie nach 1. Rezidiv` = c("Keine Therapie", "Ohne Angabe"),
             n = as.character(c(n_keine, n_ohne)),
             `%` = c("--", "--"),
             check.names = FALSE, stringsAsFactors = FALSE))

write_table_tex(
  tab_323,
  file.path(tex_dir, "Tab_3_23_Therapie_Erstrezidiv.tex"),
  caption = paste("Therapie-Kategorien bei Erstrezidiv. Bezugsbasis sind die",
                  "Erstrezidive mit dokumentierter Therapieangabe."),
  align = "lrr"
)

have_ggalluvial <- requireNamespace("ggalluvial", quietly = TRUE)
if (have_ggalluvial) {
  library(ggalluvial)
} else {
  message("ggalluvial not installed; Sankey figure will be skipped.")
}

flow_wide <- therapy_bucketed |>
  filter(rez_idx %in% c(1, 2)) |>
  select(Pseudonym, rez_idx, therapy_bucket) |>
  mutate(rez_idx_lbl = factor(paste0("R", rez_idx),
                              levels = c("R1", "R2"))) |>
  tidyr::pivot_wider(names_from = rez_idx_lbl, values_from = therapy_bucket) |>
  filter(!is.na(R1))

loc_lookup <- static_analysis |>
  select(Pseudonym, lok = D_rezidiv_cat_first) |>
  mutate(lok = dplyr::recode(as.character(lok),
           "Peritoneal_Andere" = "Peritoneal/Andere",
           "Ossaer"            = "Ossär"),
         lok = factor(
           ifelse(is.na(lok) | lok == "", "unbekannt", lok),
           levels = c("Intrahepatisch", "Intra + extrahepatisch",
                      "Lymphknoten", "Peritoneal/Andere",
                      "Pulmonal", "Ossär", "Cerebral",
                      "unbekannt")
         ))
flow_wide <- flow_wide |>
  left_join(loc_lookup, by = "Pseudonym") |>
  mutate(
    lok = if_else(is.na(lok), factor("unbekannt", levels = levels(lok)), lok),
    rerez = factor(ifelse(is.na(R2), "kein 2. Rezidiv", "Re-Rezidiv"),
                   levels = c("Re-Rezidiv", "kein 2. Rezidiv")),
    R2_lbl = factor(
      ifelse(is.na(R2), "— (keine Zweittherapie)", as.character(R2)),
      levels = c("Lokal-kurativ", "Lokoregional-palliativ",
                 "Systemisch", "Keine/Unklar", "— (keine Zweittherapie)")
    )
  )

bucket_pal_sankey <- c(
  "Intrahepatisch"          = "#3B6FAB",
  "Intra + extrahepatisch"  = "#7B3F99",
  "Lymphknoten"             = "#7AAEBE",
  "Peritoneal/Andere"       = "#A0C8B5",
  "Pulmonal"                = "#CFE3B0",
  "Ossär"                   = "#F3D9A4",
  "Cerebral"                = "#5B8DBD",
  "unbekannt"               = "grey80",
  "Lokal-kurativ"           = "#3B6FAB",
  "Lokoregional-palliativ"  = "#E1A11A",
  "Systemisch"              = "#C7261B",
  "Keine/Unklar"            = "grey70",
  "Re-Rezidiv"              = "#C7261B",
  "kein 2. Rezidiv"         = "grey85",
  "— (keine Zweittherapie)" = "grey85"
)

if (nrow(flow_wide) > 0 && have_ggalluvial) {
  flow_long <- flow_wide |>
    count(lok, R1, rerez, R2_lbl, name = "Freq")

  p_sankey <- ggplot(flow_long,
                     aes(axis1 = lok, axis2 = R1,
                         axis3 = rerez, axis4 = R2_lbl,
                         y = Freq)) +
    geom_alluvium(aes(fill = lok), alpha = 0.7, decreasing = FALSE) +
    geom_stratum(alpha = 0.85, decreasing = FALSE) +
    geom_text(stat = "stratum",
              aes(label = after_stat(stratum)),
              size = 2.7, lineheight = 0.9,
              decreasing = FALSE) +
    scale_x_discrete(limits = c("Lokalisation R1", "Erstherapie",
                                "Re-Rezidiv?", "Zweittherapie"),
                     expand = c(0.12, 0.05)) +
    scale_fill_manual(values = bucket_pal_sankey, guide = "none") +
    labs(x = NULL, y = "Patient*innen") +
    theme_minimal(base_family = "sans") +
    theme(plot.title = element_blank(),
          axis.text.x = element_text(face = "bold", size = 10),
          panel.grid.major.y = element_line(colour = "grey90"),
          panel.grid.major.x = element_blank())

  if (FALSE) ggsave(file.path(fig_dir, "Abb_3_22_Therapie_Sankey.png"),
         plot = p_sankey, width = 11, height = 7, dpi = 300, bg = "white")
}

message("Stage 2 done. Wrote: Tab_Volumetrie_Verfuegbarkeit.tex, ",
        "Tab_3_22, Tab_3_23. ",
        "(FU-Dauer-Abbildung, Segment-Abbildungen, ",
        "Therapie-x-Rezidivindex, Numerical-Moments, Sankey entfallen.)")
