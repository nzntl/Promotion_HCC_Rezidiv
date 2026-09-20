# 04_Descriptive_Tables.R -- Verteilung der Rezidivhaeufigkeit je Person und
# Gegenpruefung der Rezidivkodierung; Ergebnisse nur im Protokoll. Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx.

pkgs <- c("readxl", "dplyr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")

library(readxl); library(dplyr)


# ── 1. Load and apply eligible-cohort filter ──
static  <- read_excel("./01_Daten/data_static.xlsx")
dynamic <- read_excel("./01_Daten/data_dynamic.xlsx")

eligible_pseudonyms <- static |>
  filter(is.na(Studienausschluss) |
           !Studienausschluss %in% c("no preop img", "no postop img", "no OP", "no HCC")) |>
  pull(Pseudonym)

dyn_eligible    <- dynamic |> filter(Pseudonym %in% eligible_pseudonyms)
static_eligible <- static  |> filter(Pseudonym %in% eligible_pseudonyms)


# ── 2. Per-patient summary ──
is_ja <- function(x) tolower(as.character(x)) %in% c("ja", "yes", "1", "true")

per_pat <- dyn_eligible |>
  group_by(Pseudonym) |>
  summarise(
    n_rezidiv       = sum(datasource == "rezidiv", na.rm = TRUE),
    n_vol_postop    = sum(datasource == "volumetry" &
                          !is.na(lab_date_POD) & lab_date_POD > 0, na.rm = TRUE),
    flag_rezidiv_ja = any(is_ja(Rezidiv), na.rm = TRUE),
    .groups = "drop"
  )


# ── 3. Stratify by # recurrences ──
strata <- per_pat |>
  group_by(n_rezidiv) |>
  summarise(
    n_patients        = n(),
    n_rezidivflag_ja  = sum(flag_rezidiv_ja),
    sum_vol_postop    = sum(n_vol_postop),
    median_vol_postop = median(n_vol_postop),
    .groups = "drop"
  ) |>
  arrange(n_rezidiv)

totals <- per_pat |>
  summarise(
    n_patients       = n(),
    n_rezidivflag_ja = sum(flag_rezidiv_ja),
    sum_vol_postop   = sum(n_vol_postop)
  )


# ── 4. Cross-check (datasource == "rezidiv" vs. Rezidiv flag) ──
mismatches   <- per_pat |> filter((n_rezidiv > 0) != flag_rezidiv_ja)
n_consistent <- nrow(per_pat) - nrow(mismatches)

cat("\n── Verteilung Rezidiv-Anzahl (eligible cohort) ──\n")
print(strata)

cat("\nDiagnostik (datasource == 'rezidiv' vs. Rezidiv == 'Ja'):\n")
cat("  konsistent:   ", n_consistent, "/", nrow(per_pat), "\n")
cat("  Diskrepanzen: ", nrow(mismatches), "\n")
if (nrow(mismatches) > 0) {
  cat("  Pseudonyme:   ", paste(head(mismatches$Pseudonym, 20), collapse = ", "),
      if (nrow(mismatches) > 20) " ..." else "", "\n", sep = "")
}


# ── 5. Build LaTeX strata tabular (self-contained block, for \input) ──

tex_row <- function(label, n, n_ja, sum_v, med_v) {
  med <- if (is.na(med_v)) "--" else sprintf("%.1f", med_v)
  sprintf("%s & %d & %d & %d & %s \\\\", label, n, n_ja, sum_v, med)
}

body_rows <- mapply(
  tex_row,
  as.character(strata$n_rezidiv),
  strata$n_patients,
  strata$n_rezidivflag_ja,
  strata$sum_vol_postop,
  strata$median_vol_postop,
  SIMPLIFY = TRUE, USE.NAMES = FALSE
)

total_row <- tex_row(
  "$\\Sigma$",
  totals$n_patients, totals$n_rezidivflag_ja,
  totals$sum_vol_postop, NA
)

tabular_strata <- paste0(
  "\\begin{tabular}{lrrrr}\n",
  "\\toprule\n",
  "Anzahl Rezidive & n Pat. & n Pat. (Rezidiv = Ja) & ",
  "$\\Sigma$ post-OP Vol. & Median Vol. / Pat. \\\\\n",
  "\\midrule\n",
  paste(body_rows, collapse = "\n"), "\n",
  "\\midrule\n",
  total_row, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)


# ── 6. Diagnostic block: tabular fragment (so 08_Aggregate_View wraps it in ──
mm <- mismatches |>
  mutate(Muster = case_when(
    n_rezidiv == 0 &  flag_rezidiv_ja ~ "Flag \"Ja\" ohne rezidiv-Zeile",
    n_rezidiv >  0 & !flag_rezidiv_ja ~ "rezidiv-Zeile(n) ohne Flag \"Ja\"",
    TRUE                              ~ "—"
  ))

if (nrow(mm) == 0) {
  message("Keine Rezidiv-Diagnostik-Diskrepanzen — Tab_Rezidiv_Diagnostik wird nicht erzeugt.")
} else {
  diag_tab <- mm |>
    arrange(Muster, Pseudonym) |>
    transmute(Pseudonym, `n Rezidiv-Zeilen` = n_rezidiv,
              `Rezidiv-Flag` = if_else(flag_rezidiv_ja, "Ja", "Nein"),
              Muster)
}


# ── 7. Write each table as a standalone .tex fragment (for \input use) ──

write_lines_utf8 <- function(text, path) {
  con <- file(path, "w", encoding = "UTF-8")
  writeLines(text, con); close(con)
}

if (FALSE) write_lines_utf8(tabular_strata,
                 "./03_Tables_Figures/Tab_Rezidiv_Strata.tex")

if (exists("diag_tab") && nrow(diag_tab) > 0) {
  esc <- function(x) {
    x <- as.character(x); x[is.na(x)] <- "--"
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("([&%$#_{}])", "\\\\\\1", x); x
  }
  header <- paste(esc(names(diag_tab)), collapse = " & ")
  body   <- apply(diag_tab, 1, function(r)
    paste(esc(r), collapse = " & "))
  body   <- paste(paste0(body, " \\\\"), collapse = "\n")
  diag_out <- c(
    paste0("% Diagnostik: Diskrepanzen zwischen datasource==\"rezidiv\" und ",
           "Flag Rezidiv==\"Ja\"."),
    "\\begin{tabular}{lrll}",
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  )
  if (FALSE) write_lines_utf8(paste(diag_out, collapse = "\n"),
                   "./03_Tables_Figures/Tab_Rezidiv_Diagnostik.tex")
  cat("[04] Rezidiv-Kodierungs-Diagnostik: ", nrow(diag_tab),
      " Diskrepanz-Fall/Faelle (Details oben im Log).\n", sep = "")
}


# ── 8. Aggregate report: table_view.tex is built in 08_Aggregate_View.R, a ──

if (FALSE) {
tab_files <- sort(list.files("./03_Tables_Figures",
                             pattern = "^Tab_.*\\.tex$",
                             full.names = FALSE))

pretty_title <- function(fname) {
  base <- sub("^Tab_", "", sub("\\.tex$", "", fname))
  gsub("_", " ", base)
}

extract_caption <- function(path) {
  lines <- tryCatch(readLines(path, n = 1, encoding = "UTF-8"),
                    error = function(e) character(0))
  if (length(lines) == 0) return(NA_character_)
  if (grepl("^\\s*%", lines[1])) sub("^\\s*%\\s*", "", lines[1])
  else NA_character_
}

tex_escape <- function(x) {
  if (is.na(x)) return(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x
}

section_chunks <- vapply(tab_files, function(f) {
  cap <- extract_caption(file.path("./03_Tables_Figures", f))
  cap_line <- if (is.na(cap)) "" else
    sprintf("\\noindent\\textit{%s}\\par\\smallskip\n", tex_escape(cap))
  is_landscape <- grepl("_Landscape\\.tex$", f)
  body <- sprintf("\\section*{%s}\n%s\\noindent\\input{%s}\n\\bigskip\n\n",
                  tex_escape(pretty_title(f)), cap_line, f)
  if (is_landscape) {
    body <- paste0("\\begin{landscape}\n", body, "\\end{landscape}\n")
  }
  body
}, character(1))

tex_doc <- sprintf(
  paste0(
    "\\documentclass[a4paper,11pt]{article}\n",
    "\\usepackage[utf8]{inputenc}\n",
    "\\usepackage[T1]{fontenc}\n",
    "\\usepackage[ngerman]{babel}\n",
    "\\usepackage{booktabs}\n",
    "\\usepackage{geometry}\n",
    "\\usepackage{pdflscape}\n",
    "\\geometry{margin=2.5cm}\n",
    "\\title{Tabellenübersicht — HCC-Kohorte}\n",
    "\\author{HCC-Resektionskohorte}\n",
    "\\date{%s}\n",
    "\\begin{document}\n",
    "\\maketitle\n\n",
    "Übersicht aller Tab\\_*.tex-Fragmente in ",
    "\\texttt{03\\_Tables\\_Figures/}.\n\n",
    "%s",
    "\\medskip\n",
    "\\noindent\\footnotesize\\textit{Hinweis:} ",
    "Die Spalte \\texttt{RezidivAnzahl} wurde vom Anwender als unzuverlässig ",
    "gekennzeichnet und ist in dieser Auswertung nicht enthalten.\n",
    "\\end{document}\n"
  ),
  format(Sys.Date(), "%d.%m.%Y"),
  paste(section_chunks, collapse = "")
)

write_lines_utf8(tex_doc,
                 "./03_Tables_Figures/table_view.tex")

legacy <- file.path("./03_Tables_Figures",
                    c("Rezidiv_Count_Report.tex",
                      "Rezidiv_Count_Report.aux",
                      "Rezidiv_Count_Report.log",
                      "Rezidiv_Count_Report.pdf",
                      "Rezidiv_Count_Report.synctex.gz"))
unlink(legacy[file.exists(legacy)])

message("Done. Wrote Tab_Rezidiv_Strata.tex, Tab_Rezidiv_Diagnostik.tex, ",
        "and aggregate table_view.tex with ", length(tab_files),
        " included fragments ",
        "(compile with `pdflatex table_view.tex`).")
}
message("Stage 4 done. Wrote Tab_Rezidiv_Strata.tex, Tab_Rezidiv_Diagnostik.tex.")
