# 08_Aggregate_View.R -- fasst alle Tab_*.tex-Fragmente aus 03_Tables_Figures in table_view.tex zusammen.

write_lines_utf8 <- function(text, path) {
  con <- file(path, "w", encoding = "UTF-8"); writeLines(text, con); close(con)
}

tex_escape <- function(x) {
  if (is.na(x) || !nzchar(x)) return(x)
  protected <- paste0(
    "\\$[^$]*\\$",
    "|\\\\[A-Za-z]+(?:\\{[^{}]*\\})*",
    "|\\\\[%&_#$]"
  )
  esc_gap <- function(s) gsub("([%&_#])", "\\\\\\1", s)
  m <- gregexpr(protected, x, perl = TRUE)[[1]]
  if (m[1] == -1L) return(esc_gap(x))
  lens <- attr(m, "match.length")
  out <- character(0); last <- 1L
  for (k in seq_along(m)) {
    if (m[k] > last) out <- c(out, esc_gap(substr(x, last, m[k] - 1L)))
    out <- c(out, substr(x, m[k], m[k] + lens[k] - 1L))
    last <- m[k] + lens[k]
  }
  if (last <= nchar(x)) out <- c(out, esc_gap(substr(x, last, nchar(x))))
  paste(out, collapse = "")
}

pretty_title <- function(fname) {
  base <- sub("^Tab_", "", sub("(_Landscape)?\\.tex$", "", fname))
  gsub("_", " ", base)
}

label_from <- function(fname) {
  base <- sub("(_Landscape)?\\.tex$", "", fname)
  paste0("tab:", tolower(base))
}

extract_caption <- function(path) {
  lines <- tryCatch(readLines(path, n = 1, encoding = "UTF-8"),
                    error = function(e) character(0))
  if (length(lines) == 0) return(NA_character_)
  if (grepl("^\\s*%", lines[1])) sub("^\\s*%\\s*", "", lines[1])
  else NA_character_
}

detect_wide <- function(path, threshold = 9L) {
  ln <- tryCatch(readLines(path, encoding = "UTF-8"),
                 error = function(e) character(0))
  hdr <- ln[grepl("&", ln)][1]
  if (is.null(hdr) || is.na(hdr)) return(FALSE)
  cols <- length(strsplit(hdr, "&")[[1]])
  cols >= threshold
}

detect_longtable <- function(path) {
  ln <- tryCatch(readLines(path, encoding = "UTF-8"),
                 error = function(e) character(0))
  any(grepl("\\\\begin\\{longtable\\}", ln))
}

tab_files <- sort(list.files("./03_Tables_Figures",
                             pattern = "^Tab_.*\\.tex$",
                             full.names = FALSE))

build_block <- function(f) {
  path     <- file.path("./03_Tables_Figures", f)
  cap      <- extract_caption(path)
  if (is.na(cap)) cap <- pretty_title(f)
  cap_tex  <- tex_escape(cap)
  lbl      <- label_from(f)
  title    <- pretty_title(f)

  is_landscape <- grepl("_Landscape\\.tex$", f) || detect_wide(path)

  if (detect_longtable(path)) {
    inner <- paste0(
      sprintf("\\captionof{table}{%s}\n", cap_tex),
      sprintf("\\label{%s}\n", lbl),
      sprintf("\\input{%s}\n", f)
    )
  } else {
    inner <- paste0(
      "\\begin{table}[!htbp]\n",
      "\\centering\n",
      sprintf("\\caption{%s}\n", cap_tex),
      sprintf("\\label{%s}\n", lbl),
      "\\begin{threeparttable}\n",
      sprintf("\\input{%s}\n", f),
      "\\begin{tablenotes}[flushleft]\n",
      "\\footnotesize\n",
      "\\item \\textit{Methodik:} ",
      "Tabelle generiert in \\texttt{02\\_Code/} (R + Python); ",
      "Variablendefinitionen in \\texttt{01\\_Daten/variables\\_legend.xlsx}.\n",
      "\\item \\textit{Datenquelle:} eigene Berechnungen auf Basis der",
      " Resektionskohorte UKHD (Material und Methoden, ",
      "Abschnitt~\\ref{sec:data}).\n",
      "\\end{tablenotes}\n",
      "\\end{threeparttable}\n",
      "\\end{table}\n"
    )
  }

  if (is_landscape) {
    paste0("\\begin{landscape}\n", inner, "\\end{landscape}\n\n")
  } else {
    paste0(inner, "\n")
  }
}

section_chunks <- vapply(tab_files, build_block, character(1))

tex_doc <- sprintf(
  paste0(
    "\\documentclass[a4paper,11pt]{article}\n",
    "\\usepackage[utf8]{inputenc}\n",
    "\\usepackage[T1]{fontenc}\n",
    "\\usepackage[ngerman]{babel}\n",
    "\\usepackage{booktabs}\n",
    "\\usepackage{longtable}\n",
    "\\usepackage[table]{xcolor}\n",
    "\\usepackage{amsmath}\n",
    "\\usepackage{natbib}\n",
    "\\usepackage{threeparttable}\n",
    "\\usepackage{geometry}\n",
    "\\usepackage{pdflscape}\n",
    "\\usepackage{amssymb}\n",
    "\\usepackage{caption}\n",
    "\\captionsetup[table]{justification=raggedright,singlelinecheck=false,",
      "labelfont=bf}\n",
    "\\geometry{margin=2.5cm}\n",
    "\\DeclareUnicodeCharacter{0394}{$\\Delta$}\n",
    "\\DeclareUnicodeCharacter{00B1}{$\\pm$}\n",
    "\\DeclareUnicodeCharacter{2264}{$\\leq$}\n",
    "\\DeclareUnicodeCharacter{2265}{$\\geq$}\n",
    "\\DeclareUnicodeCharacter{00D7}{$\\times$}\n",
    "\\DeclareUnicodeCharacter{2248}{$\\approx$}\n",
    "\\DeclareUnicodeCharacter{2013}{--}\n",
    "\\DeclareUnicodeCharacter{2014}{---}\n",
    "\\DeclareUnicodeCharacter{2026}{\\ldots}\n",
    "\\DeclareUnicodeCharacter{2022}{\\textbullet}\n",
    "\\DeclareUnicodeCharacter{00B2}{$^{2}$}\n",
    "\\DeclareUnicodeCharacter{00B3}{$^{3}$}\n",
    "\\DeclareUnicodeCharacter{00B0}{$^{\\circ}$}\n",
    "\\DeclareUnicodeCharacter{03C1}{$\\rho$}\n",
    "\\DeclareUnicodeCharacter{03A3}{$\\Sigma$}\n",
    "\\DeclareUnicodeCharacter{03A9}{$\\Omega$}\n",
    "\\DeclareUnicodeCharacter{03BC}{$\\mu$}\n",
    "\\DeclareUnicodeCharacter{2192}{$\\rightarrow$}\n",
    "\\DeclareUnicodeCharacter{2190}{$\\leftarrow$}\n",
    "\\title{Tabellenübersicht — HCC-Kohorte}\n",
    "\\author{HCC-Resektionskohorte}\n",
    "\\date{%s}\n",
    "\\begin{document}\n",
    "\\maketitle\n\n",
    "Übersicht aller Tab\\_*.tex-Fragmente in ",
    "\\texttt{03\\_Tables\\_Figures/}. Jede Tabelle wird als LaTeX-Float ",
    "mit \\texttt{\\textbackslash caption} eingebunden; breite Tabellen ",
    "werden automatisch in einer Landscape-Seite gesetzt.\n\n",
    "%s",
    "\\end{document}\n"
  ),
  format(Sys.Date(), "%d.%m.%Y"),
  paste(section_chunks, collapse = "")
)

write_lines_utf8(tex_doc, "./03_Tables_Figures/table_view.tex")

message("Stage 8 done. table_view.tex aggregates ", length(tab_files),
        " Tab_*.tex fragments as LaTeX floats. ",
        "Compile via `pdflatex table_view.tex`.")
