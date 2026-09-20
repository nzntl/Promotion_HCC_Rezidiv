# 10_Software_Manifest.R -- Softwaremanifest des Laufs (R, R-Pakete, Python, pip freeze).
# Schreibt Tab_Software_Versions.tex und Software_Environment_full.txt.

fig_dir <- "./03_Tables_Figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a

tex_escape_cell <- function(x) {
  x <- as.character(x); x[is.na(x)] <- "--"
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x); x
}

# ── R session info ──
si <- sessionInfo()
lauf_zeit <- Sys.time()

r_rows <- data.frame(
  Komponente = c("R", "Plattform", "Betriebssystem"),
  Version    = c(paste(R.version$major, R.version$minor, sep = "."),
                 si$platform,
                 sub("(build ", "(Build ",
                     si$running %||% paste(Sys.info()[c("sysname","release")], collapse = " "),
                     fixed = TRUE))
)

pkgs_loaded <- c(si$otherPkgs, si$loadedOnly)
if (length(pkgs_loaded) > 0) {
  r_pkg_rows <- data.frame(
    Komponente = paste0("R: ", vapply(pkgs_loaded, `[[`, character(1), "Package")),
    Version    = vapply(pkgs_loaded, `[[`, character(1), "Version")
  )
} else {
  r_pkg_rows <- data.frame(Komponente = character(0), Version = character(0))
}

# ── Python interpreter + pip freeze ──
py_exe <- Sys.getenv("HCC_PYTHON", unset = "")
if (!nzchar(py_exe)) {
  for (cand in c("python", "python3", "py")) {
    found <- Sys.which(cand)
    if (nzchar(found)) { py_exe <- found; break }
  }
}
py_rows <- data.frame(Komponente = character(0), Version = character(0))

if (file.exists(py_exe)) {
  py_ver <- tryCatch(
    system2(py_exe, args = c("--version"), stdout = TRUE, stderr = TRUE),
    error = function(e) "n/a"
  )
  py_rows <- rbind(py_rows, data.frame(
    Komponente = "Python", Version = sub("^Python ", "", paste(py_ver, collapse = " "))
  ))

  pip_lines <- tryCatch(
    system2(py_exe, args = c("-m", "pip", "freeze"),
            stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  pip_lines <- pip_lines[grepl("==", pip_lines, fixed = TRUE)]
  if (length(pip_lines) > 0) {
    parts <- strsplit(pip_lines, "==", fixed = TRUE)
    py_rows <- rbind(py_rows, data.frame(
      Komponente = paste0("Python: ",
                          vapply(parts, `[`, character(1), 1)),
      Version    = vapply(parts, `[`, character(1), 2)
    ))
  }
} else {
  py_rows <- data.frame(
    Komponente = "Python", Version = "Interpreter nicht gefunden — ML-Strang übersprungen"
  )
}

manifest <- rbind(r_rows, r_pkg_rows, py_rows)
manifest_full <- manifest

# ── Nur direkt verwendete Pakete ──
grab <- function(lines, pat)
  unique(unlist(regmatches(lines, gregexpr(pat, lines, perl = TRUE))))
r_lines <- unlist(lapply(list.files("./02_Code", pattern = "\\.R$", full.names = TRUE),
                         readLines, warn = FALSE, encoding = "UTF-8"))
r_direct <- unique(c(
  sub("^library\\(",            "", grab(r_lines, "library\\([A-Za-z0-9.]+")),
  sub("^require\\(",            "", grab(r_lines, "require\\([A-Za-z0-9.]+")),
  sub("^requireNamespace\\(\"", "", grab(r_lines, "requireNamespace\\(\"[A-Za-z0-9.]+")),
  sub("::$",                    "", grab(r_lines, "\\b[A-Za-z][A-Za-z0-9.]*::"))
))
py_lines <- unlist(lapply(list.files("./02_Code/py", pattern = "^stage.*\\.py$",
                                     full.names = TRUE),
                          readLines, warn = FALSE, encoding = "UTF-8"))
py_mods <- sub("^\\s*(import|from)\\s+", "",
               grab(py_lines, "^\\s*(import|from)\\s+[A-Za-z0-9_]+"))
py_map  <- c(sklearn = "scikit-learn", sksurv = "scikit-survival", yaml = "PyYAML",
             PIL = "pillow", dateutil = "python-dateutil")
py_dists <- ifelse(py_mods %in% names(py_map), py_map[py_mods], py_mods)
norm_name <- function(x) gsub("_", "-", tolower(x))

is_r_pkg  <- grepl("^R: ", manifest$Komponente)
is_py_pkg <- grepl("^Python: ", manifest$Komponente)
keep <- (!is_r_pkg & !is_py_pkg) |
        (is_r_pkg  & sub("^R: ", "", manifest$Komponente) %in% r_direct) |
        (is_py_pkg & norm_name(sub("^Python: ", "", manifest$Komponente)) %in%
                     norm_name(py_dists))
manifest <- manifest[keep, , drop = FALSE]
cat(sprintf("[10_Software_Manifest] direkt verwendet: %d R-Pakete, %d Python-Pakete (von %d / %d geladen bzw. installiert)\n",
            sum(is_r_pkg & keep), sum(is_py_pkg & keep), sum(is_r_pkg), sum(is_py_pkg)))

env_path <- file.path(fig_dir, "Software_Environment_full.txt")
writeLines(c(sprintf("# Vollstaendige Softwareumgebung des Laufs %s",
                     format(lauf_zeit, "%Y-%m-%d %H:%M:%S")),
             "# alle geladenen R-Pakete laut sessionInfo(), alle installierten Python-Pakete laut pip freeze",
             paste(manifest_full$Komponente, manifest_full$Version, sep = "\t")),
           env_path, useBytes = TRUE)

# ── Write LaTeX fragment ──
caption_line <- sprintf(
  "%% Software-Versions-Manifest (Lauf %s)",
  format(lauf_zeit, "%Y-%m-%d %H:%M:%S")
)
ohne_praefix <- function(d, praefix) {
  d$Komponente <- sub(praefix, "", d$Komponente)
  d[order(tolower(d$Komponente), method = "radix"), , drop = FALSE]
}
seite_r  <- rbind(manifest[manifest$Komponente %in% c("R", "Plattform"), , drop = FALSE],
                  ohne_praefix(manifest[grepl("^R: ", manifest$Komponente), , drop = FALSE],
                               "^R: "))
seite_py <- rbind(manifest[manifest$Komponente == "Python", , drop = FALSE],
                  ohne_praefix(manifest[grepl("^Python: ", manifest$Komponente), , drop = FALSE],
                               "^Python: "))
n_zeilen <- max(nrow(seite_r), nrow(seite_py))
auffuellen <- function(d) {
  n_pad <- n_zeilen - nrow(d)
  if (n_pad > 0) d <- rbind(d, data.frame(Komponente = rep("", n_pad),
                                          Version    = rep("", n_pad)))
  rownames(d) <- NULL
  d
}
wide <- cbind(auffuellen(seite_r), auffuellen(seite_py))
body <- apply(wide, 1,
              function(row) paste(tex_escape_cell(row), collapse = " & "))
body <- paste(body, collapse = " \\\\\n")
kopf <- c("\\multicolumn{2}{l}{R} & \\multicolumn{2}{l}{Python} \\\\",
          "\\cmidrule(r){1-2} \\cmidrule(l){3-4}",
          "Komponente & Version & Komponente & Version \\\\")
os_zeile <- sprintf("Betriebssystem & \\multicolumn{3}{l}{%s} \\\\",
                    tex_escape_cell(manifest$Version[manifest$Komponente == "Betriebssystem"]))
out <- c(
  caption_line,
  "\\begin{longtable}{llll}",
  sprintf("\\caption[Software-Stand des Referenzlaufs]{Softwareversionen des Referenzlaufs vom %s.}\\label{tab:app:software}\\\\",
          paste0(as.integer(format(lauf_zeit, "%d")), ". ",
                 c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli",
                   "August", "September", "Oktober", "November", "Dezember")[
                   as.integer(format(lauf_zeit, "%m"))],
                 " ", format(lauf_zeit, "%Y"))),
  "\\toprule", kopf, "\\midrule",
  "\\endfirsthead",
  "\\toprule", kopf, "\\midrule", "\\endhead",
  paste0(body, " \\\\"),
  "\\midrule",
  os_zeile,
  "\\bottomrule",
  "\\end{longtable}"
)
path <- file.path(fig_dir, "Tab_Software_Versions.tex")
con <- file(path, "w", encoding = "UTF-8")
writeLines(out, con); close(con)
cat(sprintf("[10_Software_Manifest] wrote %s (%d Komponenten)\n",
            path, nrow(manifest)))
