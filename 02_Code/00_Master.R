# 00_Master.R -- startet die gesamte Auswertung: erst den R-Strang (Aufbereitung,
# Deskription, Cox, LASSO, gemeinsames Modell), danach den Python-Strang (02_Code/py).
# Aufruf aus dem Projektstamm: source("./02_Code/00_Master.R")

# ── 0. Environment ──
rm(list = ls())

# ── 1. Purge BOTH output sinks ──
fig_dir    <- "./03_Tables_Figures"
mirror_dir <- "./05_Target_Structure/01_figs_tabs"

purge_dir <- function(d, keep_pattern = "^$") {
  if (!dir.exists(d)) { dir.create(d, recursive = TRUE); return(0L) }
  to_drop <- list.files(d, full.names = TRUE, recursive = FALSE)
  to_drop <- to_drop[!grepl(keep_pattern, basename(to_drop))]
  if (length(to_drop) > 0) unlink(to_drop, recursive = FALSE, force = TRUE)
  length(to_drop)
}
n_p1 <- purge_dir(fig_dir,    keep_pattern = "^table_view\\.tex$")
n_p2 <- purge_dir(mirror_dir, keep_pattern = "^CV\\.pdf$")
cat("Purged", n_p1, "file(s) from", fig_dir,
    "and", n_p2, "from", mirror_dir, "\n")

# ── 1b. Purge Python-OOF-Cache ──
cache_dir <- "./02_Code/py/_cache"
n_p3 <- purge_dir(cache_dir)
cat("Purged", n_p3, "file(s) from", cache_dir, "\n")

# ── 2. Packages shared across scripts ──
library(wesanderson)

# ── 2b. EPV diagnostic helper ──
print_epv_summary <- function(fits = NULL,
                              out_path = "./03_Tables_Figures/Tab_EPV_Diagnostics.tex") {
  if (is.null(fits)) {
    candidates <- c("fitA", "fitB", "fitC", "fitD")
    fits <- mget(candidates[vapply(candidates, exists, logical(1),
                                   envir = .GlobalEnv)],
                 envir = .GlobalEnv, ifnotfound = list(NULL))
  }
  if (length(fits) == 0L) {
    message("[EPV] no Cox fits in scope — skipping diagnostic.")
    return(invisible(NULL))
  }

  epv_labels <- c(fitA = "Modell A (Routinebefunde)",
                  fitB = "Modell B (+ präop Labor)",
                  fitC = "Modell C (+ präop Volumetrie)",
                  fitD = "Modell D (+ Regeneration)")
  rows <- lapply(names(fits), function(nm) {
    f <- fits[[nm]]
    nm_label <- if (nm %in% names(epv_labels)) unname(epv_labels[nm]) else nm
    if (is.null(f) || inherits(f, "try-error")) {
      return(data.frame(Modell = nm_label, Ereignisse = NA_integer_,
                        Variablen = NA_integer_, Parameter = NA_integer_,
                        EPV = NA_real_, Status = "fit fehlgeschlagen"))
    }
    nev <- tryCatch(f$nevent, error = function(e) NA_integer_)
    nv   <- tryCatch(length(coef(f)), error = function(e) NA_integer_)
    nvar <- tryCatch(length(attr(f$terms, "term.labels")),
                     error = function(e) NA_integer_)
    epv <- if (!is.na(nev) && !is.na(nv) && nv > 0) nev / nv else NA_real_
    flag <- if (is.na(epv)) "—"
            else if (epv < 5)  "kritisch"
            else if (epv < 10) "grenzwertig"
            else               "ok"
    data.frame(Modell = nm_label, Ereignisse = nev, Variablen = nvar,
               Parameter = nv, EPV = round(epv, 2), Status = flag)
  })
  tab <- do.call(rbind, rows)

  cat("[EPV] summary:\n"); print(tab, row.names = FALSE)

  esc <- function(x) {
    x <- as.character(x); x[is.na(x)] <- "--"
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    gsub("([&%$#_{}])", "\\\\\\1", x)
  }
  tab_tex <- tab[, c("Modell", "Ereignisse", "Variablen", "Parameter", "EPV")]
  header <- paste(esc(names(tab_tex)), collapse = " & ")
  body <- apply(tab_tex, 1, function(row) paste(esc(row), collapse = " & "))
  body <- paste(body, collapse = " \\\\\n")
  out <- c(
    "% Events-per-Variable-Diagnostik je Cox-Modell (Schwelle: <10 grenzwertig, <5 kritisch). EPV = Ereignisse / Parameter.",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(body, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )
  con <- file(out_path, "w", encoding = "UTF-8")
  writeLines(out, con); close(con)
  cat(sprintf("[EPV] wrote %s\n", out_path))
  invisible(tab)
}

# ── 3. Pipeline ──
source("./02_Code/01_Preparation_Selection.R")
source("./02_Code/02_Meta_and_Static_Analysis.R")
source("./02_Code/02b_Correlation_Heatmap.R")
source("./02_Code/03b_Hazard_Plots.R")
source("./02_Code/04_Descriptive_Tables.R")
source("./02_Code/05_Descriptive_Figures.R")
source("./02_Code/06_Cox_Analysis.R")
source("./02_Code/06b_Cox_Chan_Postop.R")
source("./02_Code/06c_FineGray_CompetingRisk.R")
source("./02_Code/06e_TimingRestricted_PostopVol.R")
source("./02_Code/06d_Cox_LASSO_Main.R")
print_epv_summary()
source("./02_Code/07_Variable_Legend.R")
source("./02_Code/09_TRIPOD_AI_Table.R")
source("./02_Code/13_Imaging_Density_Audit.R")
source("./02_Code/16_Diskussion_Zusatzrechnungen.R")   # braucht fitD und sdat_D aus 06b

# ── Stufe 4 — Joint Model (Sensitivität) ──
cat("[Master] Joint Model Variante A: MCMC 200k x 4 chains (~0,5-8 h je nach CPU; Schalter HCC_JM_ITER, HCC_JM_BURNIN, HCC_JM_CHAINS)\n")
source("./02_Code/04b_Joint_Model_VarianteA.R")

# ── 4. Python ML strand (optional — skipped if no interpreter found) ──
py_exe <- Sys.getenv("HCC_PYTHON", unset = "")
if (!nzchar(py_exe)) {
  for (cand in c("python", "python3", "py")) {
    found <- Sys.which(cand)
    if (nzchar(found)) { py_exe <- found; break }
  }
}
py_scripts <- c(
  "02_Code/py/stage2_nested_cv.py",
  "02_Code/py/stage3_nested_cv.py",
  "02_Code/py/stage3c_nested_cv_ablation.py",
  "02_Code/py/stage3d_hpt_table.py",
  "02_Code/py/stage4_model_comparison.py",
  "02_Code/py/stage4_delta_metrics.py",
  "02_Code/py/stage4_km_tertiles.py",
  "02_Code/py/stage4_calibration_dca_cv.py",
  "02_Code/py/stage6_fruerezidiv.py",
  "02_Code/py/stage6_surveillance.py",
  "02_Code/py/stage7_sensitivity.py"
)
if (file.exists(py_exe)) {
  py_crash_log <- character(0)
  for (script in py_scripts) {
    if (!file.exists(script)) {
      cat(sprintf("[python] SKIP (Skript fehlt): %s\n", script))
      py_crash_log <- c(py_crash_log, basename(script))
      next
    }
    t0 <- Sys.time()
    rc <- system2(py_exe, args = c(script), stdout = TRUE, stderr = TRUE)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    status <- attr(rc, "status")
    if (is.null(status)) status <- 0L
    if (status != 0L) {
      cat("\n", strrep("!", 70), "\n", sep = "")
      cat(sprintf("[python] CRASH  %s  exit=%d  (%.1f s)\n",
                  basename(script), status, elapsed))
      cat(strrep("!", 70), "\n", sep = "")
      tail_lines <- tail(rc, 12)
      for (ln in tail_lines) cat("  ", ln, "\n", sep = "")
      cat(strrep("!", 70), "\n\n", sep = "")
      py_crash_log <- c(py_crash_log, basename(script))
    } else {
      cat(sprintf("[python] %-38s  ok  (%5.1f s)
",
                  basename(script), elapsed))
      for (ln in rc) cat("     ", ln, "
", sep = "")
    }
  }
  if (length(py_crash_log) > 0L) {
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat(sprintf("[python] %d von %d Skripten CRASHED:\n",
                length(py_crash_log), length(py_scripts)))
    for (s in py_crash_log) cat("   -", s, "\n")
    cat("Affected Tab_*/Abb_* bleiben als TBD-Placeholder im PDF.\n")
    cat(strrep("=", 70), "\n\n", sep = "")
    .py_crash_fatal <- TRUE
  } else {
    cat(sprintf("\n[python] ALLE %d Skripte erfolgreich.\n\n", length(py_scripts)))
  }
} else {
  message("No Python interpreter found (HCC_PYTHON not set, ",
          "python/python3/py not on PATH) — ML stages skipped. ",
          "See requirements.txt for setup.")
}

# ── 4b. Infrastructure / audit-trail stages ──
source("./02_Code/10_Software_Manifest.R")
source("./02_Code/14_Static_Reference_Tables.R")
source("./02_Code/15_Variable_Overview.R")

source("./02_Code/08_Aggregate_View.R")

# ── 5. Mirror outputs to 05_Target_Structure/01_figs_tabs ──
source("./02_Code/00b_Mirror_Figs.R")

# ── Abschluss: unvollstaendiger Lauf endet mit Fehlerstatus ──
if (exists(".py_crash_fatal") && isTRUE(.py_crash_fatal)) {
  stop("Pipeline unvollstaendig: mindestens ein Python-Skript ist abgestuertzt. ",
       "Siehe die CRASH-Bloecke weiter oben im Protokoll.")
}
