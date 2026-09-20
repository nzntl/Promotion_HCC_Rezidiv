# 04b_Joint_Model_VarianteA.R -- gemeinsames Verlaufs- und Ereigniszeitmodell (JMbayes2).
# Liest 01_Daten/data_static.xlsx, data_dynamic.xlsx, data_dynamic_surv.xlsx; schreibt
# Tab_3_14_JointModel_Posterior.tex, Abb_JointModel_Slope_Posterior.png und die Fit-Objekte (.rds).

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Variante-A Konstanten ──
N_ITER   <- as.integer(Sys.getenv("HCC_JM_ITER",   "200000"))
N_BURNIN <- as.integer(Sys.getenv("HCC_JM_BURNIN", "40000"))
N_CHAINS <- as.integer(Sys.getenv("HCC_JM_CHAINS", "4"))
N_THIN   <-    10L
SEED     <- 20260528L


suppressPackageStartupMessages({
  ok_pkgs <- TRUE
  for (p in c("readxl", "dplyr", "stringr", "tidyr", "survival",
              "splines", "nlme", "ggplot2", "gridExtra")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      message("[04b_JM_A] Paket fehlt: ", p, " - uebersprungen.")
      ok_pkgs <- FALSE
    }
  }
  if (!requireNamespace("JMbayes2", quietly = TRUE)) {
    message("[04b_JM_A] Paket JMbayes2 fehlt.")
    ok_pkgs <- FALSE
  }
})

fig_dir <- "./03_Tables_Figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

tab_path  <- file.path(fig_dir, "Tab_3_14_JointModel_Posterior.tex")
abb_path  <- file.path(fig_dir, "Abb_JointModel_Slope_Posterior.png")

write_failure_placeholder <- function(reason) {
  cat(sprintf("[04b_JM_A] !! Joint Model NICHT durchgefuehrt: %s\n", reason))
  for (pth in c(tab_path, abb_path)) {
    if (file.exists(pth)) {
      unlink(pth)
      cat(sprintf("[04b_JM_A] veraltete Datei entfernt: %s\n", pth))
    }
  }
  invisible(NULL)
}

run_variante_A <- function() {
  set.seed(SEED)

  cat(sprintf("[04b_JM_A] Variante A: linearer Zeit-Trend; MCMC %d iter x %d chains, burnin %d, thin %d\n",
              N_ITER, N_CHAINS, N_BURNIN, N_THIN))

  static  <- read_excel("./01_Daten/data_static.xlsx")
  dynamic <- read_excel("./01_Daten/data_dynamic.xlsx")
  surv_df <- read_excel("./01_Daten/data_dynamic_surv.xlsx")

  analysis_pseu <- static |>
    filter(is.na(Rezidivtumor) | str_to_lower(Rezidivtumor) != "ja") |>
    pull(Pseudonym)

  as_num <- function(x) suppressWarnings(as.numeric(x))

  static_an <- static |>
    filter(Pseudonym %in% analysis_pseu) |>
    mutate(
      age_c            = as_num(Alter),
      cirrhosis        = as.integer(Leberzirrhose == "ja"),
      cp_c             = as_num(ChildPugh_Punkte),
      tum_d_sd         = scale(as_num(Tumordurchmesser))[, 1],
      aetio_model      = factor(aetio_model,
                                levels = c("Rest", "MASLD", "viral", "ethyltox")),
      dm_bin           = as.integer(str_to_lower(as.character(DM)) == "ja"),
      FLV_TLV_ratio    = ifelse(TLV_preop > 0, FLV / TLV_preop, NA_real_),
      FLV_TLV_ratio_sd = scale(FLV_TLV_ratio)[, 1]
    )

  surv_an <- surv_df |> filter(Pseudonym %in% analysis_pseu)
  surv_jm <- static_an |>
    inner_join(surv_an, by = "Pseudonym") |>
    select(Pseudonym, time, event, age_c, cirrhosis, tum_d_sd,
           FLV_TLV_ratio_sd) |>
    filter(complete.cases(across(everything())))

  if (nrow(surv_jm) < 100) {
    write_failure_placeholder(
      sprintf("Survival-Subkohorte zu klein (n=%d)", nrow(surv_jm)))
    return(invisible(NULL))
  }

  ev_lookup <- surv_jm |> select(Pseudonym, .ev_time = time)
  long_all <- dynamic |>
    filter(Pseudonym %in% analysis_pseu,
           datasource == "volumetry",
           !is.na(TLV_parenchym), TLV_parenchym > 0,
           !is.na(lab_date_POD_imp_m), lab_date_POD_imp_m >= 0) |>
    left_join(ev_lookup, by = "Pseudonym") |>
    filter(is.na(.ev_time) | lab_date_POD_imp <= .ev_time) |>
    transmute(
      Pseudonym,
      months_post_op = as_num(lab_date_POD_imp_m),
      TLV_log        = log(as_num(TLV_parenchym))
    ) |>
    filter(!is.na(months_post_op), !is.na(TLV_log))

  JM_COHORTS <- Sys.getenv("HCC_JM_COHORTS", "both")
  cohort_spec <- list()
  if (JM_COHORTS %in% c("2", "both"))
    cohort_spec <- c(cohort_spec, list(list(
      key = "min2", min_obs = 2L,
      lbl = "nur Personen mit >= 2 Messungen")))
  if (JM_COHORTS %in% c("1", "both"))
    cohort_spec <- c(cohort_spec, list(list(
      key = "alle", min_obs = 1L,
      lbl = "alle Personen (Einzelmessung zugelassen)")))
  if (length(cohort_spec) == 0)
    stop("[04b_JM_A] HCC_JM_COHORTS muss '2', '1' oder 'both' sein.")

  surv_jm_base <- surv_jm
  jm_results <- list()

  for (cs in cohort_spec) {
  cat(sprintf("\n[04b_JM_A] ════ Kohortenfassung %s: %s ════\n",
              cs$key, cs$lbl))
  long_jm <- long_all |>
    group_by(Pseudonym) |>
    filter(n() >= cs$min_obs) |>
    ungroup()

  pseu_both <- intersect(unique(long_jm$Pseudonym),
                         unique(surv_jm_base$Pseudonym))
  long_jm <- long_jm |> filter(Pseudonym %in% pseu_both)
  surv_jm <- surv_jm_base |> filter(Pseudonym %in% pseu_both)

  n_single <- long_jm |> count(Pseudonym) |> filter(n == 1) |> nrow()
  cat(sprintf("[04b_JM_A] n_pat=%d, n_obs=%d, events=%d, Ein-Punkt-Personen=%d (%.1f %%)\n",
              length(pseu_both), nrow(long_jm), sum(surv_jm$event),
              n_single, 100 * n_single / max(length(pseu_both), 1)))

  if (length(pseu_both) < 60 || sum(surv_jm$event) < 30) {
    cat(sprintf("[04b_JM_A] !! Subkohorte zu klein (n_pat=%d, events=%d) — uebersprungen.\n",
                length(pseu_both), sum(surv_jm$event)))
    next
  }

  lme_fit <- tryCatch(
    lme(TLV_log ~ months_post_op,
        random = ~ months_post_op | Pseudonym,
        data = long_jm,
        control = lmeControl(opt = "optim",
                             maxIter = 200, msMaxIter = 200)),
    error = function(e) { message("[04b_JM_A] LME failed: ", e$message); NULL }
  )
  if (is.null(lme_fit)) {
    cat("[04b_JM_A] !! LME konvergierte nicht — Fassung uebersprungen.\n")
    next
  }
  slope_sd <- tryCatch(
    sd(stats::coef(lme_fit)[["months_post_op"]], na.rm = TRUE),
    error = function(e) NA_real_)
  cat(sprintf("[04b_JM_A] SD der individuellen Steigungen (%s): %.5f log-Parenchymvolumen/Monat\n",
              cs$key, slope_sd))

  cox_fit <- tryCatch(
    coxph(Surv(time, event) ~ age_c + cirrhosis + tum_d_sd +
                              FLV_TLV_ratio_sd,
          data = surv_jm, x = TRUE, model = TRUE),
    error = function(e) { message("[04b_JM_A] Cox failed: ", e$message); NULL }
  )
  if (is.null(cox_fit)) {
    cat("[04b_JM_A] !! Cox konvergierte nicht — Fassung uebersprungen.\n")
    next
  }

  rds_path <- sprintf("./01_Daten/jm_fit_%s.rds", cs$key)
  aus_rds <- Sys.getenv("HCC_JM_AUS_RDS", "0") == "1" && file.exists(rds_path)
  if (aus_rds) {
    gespeichert <- readRDS(rds_path)
    if (!isTRUE(all.equal(gespeichert$slope_sd, slope_sd)) ||
        gespeichert$meta$n_pat    != length(pseu_both) ||
        gespeichert$meta$n_obs    != nrow(long_jm) ||
        gespeichert$meta$n_events != sum(surv_jm$event)) {
      stop(sprintf(paste0("[04b_JM_A] gespeicherter Fit (%s) passt nicht zu ",
                          "den aktuellen Daten; ohne HCC_JM_AUS_RDS neu rechnen"),
                   cs$key))
    }
    jm_fit_c <- gespeichert$jm
    cat(sprintf("[04b_JM_A] Fit aus %s geladen, kein MCMC-Lauf\n", rds_path))
  }
  if (!aus_rds) {
  cat(sprintf("[04b_JM_A] MCMC start (Steigungskopplung, %s) ...\n", cs$key))
  t0 <- Sys.time()
  jm_fit_c <- tryCatch(
    jm(cox_fit, lme_fit, time_var = "months_post_op",
       functional_forms = ~ slope(TLV_log),
       n_iter = N_ITER, n_burnin = N_BURNIN,
       n_thin = N_THIN, n_chains = N_CHAINS,
       seed = SEED),
    error = function(e) { message("[04b_JM_A] jm() failed: ", e$message); NULL }
  )
  elapsed_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(sprintf("[04b_JM_A] MCMC elapsed (%s): %.1f min\n", cs$key, elapsed_min))
  if (is.null(jm_fit_c)) {
    cat("[04b_JM_A] !! jm()-Fit fehlgeschlagen — Fassung uebersprungen.\n")
    next
  }

  rds_path <- sprintf("./01_Daten/jm_fit_%s.rds", cs$key)
  saveRDS(list(jm = jm_fit_c, slope_sd = slope_sd,
               meta = list(key = cs$key, lbl = cs$lbl,
                           n_pat = length(pseu_both), n_obs = nrow(long_jm),
                           n_events = sum(surv_jm$event),
                           n_single = n_single)),
          rds_path)
  cat(sprintf("[04b_JM_A] Fit gespeichert: %s\n", rds_path))
  }

  s <- summary(jm_fit_c)
  cat("[04b_JM_A] Posterior-Block-Struktur:\n")
  cat(sprintf("  Outcome1 nrow=%s, Survival nrow=%s\n",
              ifelse(is.null(s$Outcome1), "NULL", nrow(s$Outcome1)),
              ifelse(is.null(s$Survival), "NULL", nrow(s$Survival))))

  # ── Tab 3.14 (Posterior-Summary) ──
  tag_block <- function(df, label) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    rn <- rownames(df)
    df2 <- as.data.frame(df, stringsAsFactors = FALSE)
    df2$Variable_raw <- rn
    df2$Block        <- label
    df2
  }
  blocks <- Filter(Negate(is.null), list(
    tag_block(s$Outcome1, "Longitudinal (LME, log-Parenchymvolumen)"),
    tag_block(s$Survival, "Survival (Cox + Assoziation)"),
    tag_block(s$sigmaF,   "Residual-SD (longitudinal)")))
  if (length(blocks) == 0) {
    cat(sprintf("[04b_JM_A] !! Posterior-Summary leer (Modell %s) — uebersprungen.\n",
                cs$key))
    next
  }
  all_cols <- unique(unlist(lapply(blocks, names)))
  pad <- function(df) {
    miss <- setdiff(all_cols, names(df))
    for (m in miss) df[[m]] <- NA
    df[, all_cols, drop = FALSE]
  }
  fixed_stats <- do.call(rbind, lapply(blocks, pad))

  rhat_col <- grep("Rhat|R_hat", names(fixed_stats), value = TRUE,
                   ignore.case = TRUE)[1]
  rhat_vec <- if (!is.na(rhat_col)) as.numeric(fixed_stats[[rhat_col]])
              else rep(NA_real_, nrow(fixed_stats))
  rhat_max <- if (any(!is.na(rhat_vec))) max(rhat_vec, na.rm = TRUE) else NA

  converged <- (!is.na(rhat_max) && rhat_max < 1.01)
  cat(sprintf("[04b_JM_A] max(R-hat) = %.3f  -> converged=%s\n",
              rhat_max, converged))

  center_vec <- fixed_stats$Mean   %||% fixed_stats$Median
  lo_vec     <- fixed_stats[["2.5%"]]
  hi_vec     <- fixed_stats[["97.5%"]]
  jm_labels <- c("(Intercept)"         = "Achsenabschnitt",
                 "months_post_op"      = "Zeit postop (Monate, linear)",
                 "sigma"               = "Residuum (sigma)",
                 "age_c"               = "Alter",
                 "cirrhosis"           = "Leberzirrhose",
                 "cp_c"                = "Child-Pugh Punkte",
                 "tum_d_sd"            = "Tumordurchmesser (pro SD)",
                 "aetio_modelMASLD"    = "Ätiologie: MASLD",
                 "aetio_modelviral"    = "Ätiologie: viral",
                 "aetio_modelethyltox" = "Ätiologie: ethyltoxisch",
                 "dm_bin"              = "Diabetes mellitus",
                 "FLV_TLV_ratio_sd"    = "FLV/TLV-Ratio (pro SD)",
                 "value(TLV_log)"      = "Current-Value (log-Parenchymvolumen)",
                 "slope(TLV_log)"      = "Steigung (log-Parenchymvolumen, Wachstumsrate)",
                 "sigma_frailty"       = "Frailty-SD")
  vr     <- as.character(fixed_stats$Variable_raw)
  vr_lab <- ifelse(vr %in% names(jm_labels), unname(jm_labels[vr]), vr)
  rows <- data.frame(
    Variable = vr_lab,
    Block    = fixed_stats$Block,
    Median   = sprintf("%.3f", center_vec),
    `95\\%-CrI` = sprintf("(%.3f, %.3f)", lo_vec, hi_vec),
    Rhat     = sprintf("%.3f", rhat_vec),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  cat("[04b_JM_A] ── Posterior-Summary (vollstaendig) ──\n")
  print(rows, row.names = FALSE)
  i_sl <- which(vr == "slope(TLV_log)")
  if (length(i_sl) == 1 && is.finite(slope_sd)) {
    cat(sprintf(paste0("[04b_JM_A] Steigungs-Assoziation pro 1 SD ",
                       "(SD = %.5f): log-HR %.3f (%.3f; %.3f) ",
                       "= HR %.2f (%.2f; %.2f)\n"),
                slope_sd,
                center_vec[i_sl] * slope_sd,
                lo_vec[i_sl] * slope_sd, hi_vec[i_sl] * slope_sd,
                exp(center_vec[i_sl] * slope_sd),
                exp(lo_vec[i_sl] * slope_sd), exp(hi_vec[i_sl] * slope_sd)))
  }
  slope_draws <- tryCatch({
    am <- do.call(rbind, jm_fit_c$mcmc[["alphas"]])
    ic <- if (!is.null(colnames(am))) grep("slope", colnames(am))[1] else 1L
    as.numeric(am[, ic])
  }, error = function(e) NULL)
  if (is.null(slope_draws))
    cat("[04b_JM_A] !! MCMC-Draws der Steigungskopplung nicht extrahierbar.\n")
  jm_results[[cs$key]] <- list(
    key = cs$key, lbl = cs$lbl,
    n_pat = length(pseu_both), n_obs = nrow(long_jm),
    n_events = sum(surv_jm$event), n_single = n_single,
    pct_single = 100 * n_single / max(length(pseu_both), 1),
    slope_sd = slope_sd, rhat_max = rhat_max,
    est = data.frame(vr = vr, center = center_vec, lo = lo_vec, hi = hi_vec,
                     stringsAsFactors = FALSE),
    slope_draws = slope_draws)

  # ── Diagnostik: R-hat je Parameter ins Log ──
  diag_df <- data.frame(
    Variable = vr_lab,
    Block    = fixed_stats$Block,
    Rhat     = rhat_vec,
    stringsAsFactors = FALSE
  )
  diag_df <- diag_df[!is.na(diag_df$Rhat), , drop = FALSE]
  cat("[04b_JM_A] R-hat je Parameter (Ziel < 1,01):\n")
  print(diag_df, row.names = FALSE)
  }

  # ── Objekte: Tabelle (Fassungen nebeneinander) + Posteriordichte ──
  if (length(jm_results) == 0) {
    write_failure_placeholder("keine Kohortenfassung erfolgreich gefittet")
  } else {
    res <- jm_results
    col_lbl <- c(min2 = "$\\geq$ 2 postop. Volumetrien",
                 alle = "$\\geq$ 1 postop. Volumetrie")
    plt_lbl <- c(min2 = "≥ 2 postop. Volumetrien",
                 alle = "≥ 1 postop. Volumetrie")
    lab_of  <- function(map, key) if (key %in% names(map)) map[[key]] else key

    hr_cell <- function(r, key) {
      i <- which(r$est$vr == key)
      if (length(i) != 1) return("--")
      m <- if (key == "slope(TLV_log)") r$slope_sd else 1
      if (!is.finite(m)) return("--")
      sprintf("%.2f (%.2f; %.2f)", exp(r$est$center[i] * m),
              exp(r$est$lo[i] * m), exp(r$est$hi[i] * m))
    }
    cells <- function(fun) paste(vapply(res, fun, character(1)), collapse = " & ")
    tab_lines <- c(
      sprintf(paste0("%% Joint Model (JMbayes2, Steigungskopplung ",
                     "slope(log-Parenchymvolumen)): beide Kohortenfassungen. HRs mit ",
                     "95%%-CrI; die Steigungskopplung pro 1 SD der ",
                     "individuellen LME-Steigungen (SD je Fassung: letzte ",
                     "Zeile, log-Parenchymvolumen/Monat). Linearer Zeit-Trend + Random ",
                     "Intercept/Slope; MCMC %d Iterationen x %d Ketten ",
                     "(Burn-in %d, Thinning %d)."),
              N_ITER, N_CHAINS, N_BURNIN, N_THIN),
      sprintf("\\begin{tabular}{l%s}", strrep("r", length(res))),
      "\\toprule",
      paste0("Kennzahl & ",
             paste(vapply(res, function(r) lab_of(col_lbl, r$key),
                          character(1)), collapse = " & "), " \\\\"),
      "\\midrule",
      paste0("Patient*innen (n) & ",
             cells(function(r) sprintf("%d", r$n_pat)), " \\\\"),
      paste0("Beobachtungen (n) & ",
             cells(function(r) sprintf("%d", r$n_obs)), " \\\\"),
      paste0("Rezidivereignisse (n) & ",
             cells(function(r) sprintf("%d", r$n_events)), " \\\\"),
      paste0("Personen mit einer Messung (\\%) & ",
             cells(function(r) sprintf("%.1f", r$pct_single)), " \\\\"),
      "\\midrule",
      paste0("Steigung log-Parenchymvolumen (HR pro 1 SD) & ",
             cells(function(r) hr_cell(r, "slope(TLV_log)")), " \\\\"),
      paste0("Alter (HR pro Jahr) & ",
             cells(function(r) hr_cell(r, "age_c")), " \\\\"),
      paste0("Leberzirrhose (HR) & ",
             cells(function(r) hr_cell(r, "cirrhosis")), " \\\\"),
      paste0("Tumordurchmesser (HR pro 1 SD) & ",
             cells(function(r) hr_cell(r, "tum_d_sd")), " \\\\"),
      paste0("FLV/TLV-Ratio (HR pro 1 SD) & ",
             cells(function(r) hr_cell(r, "FLV_TLV_ratio_sd")), " \\\\"),
      "\\midrule",
      paste0("max. R-hat & ",
             cells(function(r) sprintf("%.3f", r$rhat_max)), " \\\\"),
      paste0("SD der individuellen Steigungen & ",
             cells(function(r) sprintf("%.5f", r$slope_sd)), " \\\\"),
      "\\bottomrule",
      "\\end{tabular}")
    con <- file(tab_path, "w", encoding = "UTF-8")
    writeLines(tab_lines, con); close(con)
    cat(sprintf("[04b_JM_A] wrote %s\n", tab_path))

    dens_df <- bind_rows(lapply(res, function(r) {
      if (is.null(r$slope_draws) || !is.finite(r$slope_sd)) return(NULL)
      data.frame(Fassung = lab_of(plt_lbl, r$key),
                 hr = exp(r$slope_draws * r$slope_sd),
                 stringsAsFactors = FALSE)
    }))
    if (nrow(dens_df) > 0) {
      fass_pal <- c("#00441B", "#D95F02")
      names(fass_pal) <- c(plt_lbl[["min2"]], plt_lbl[["alle"]])
      x_hi <- as.numeric(quantile(dens_df$hr, 0.995))
      p_dens <- ggplot(dens_df, aes(x = hr, colour = Fassung, fill = Fassung)) +
        geom_density(alpha = 0.18, linewidth = 0.9) +
        geom_vline(xintercept = 1, linetype = "dashed", colour = "grey35") +
        scale_colour_manual(values = fass_pal, name = "Kohortenfassung") +
        scale_fill_manual(values = fass_pal, name = "Kohortenfassung") +
        coord_cartesian(xlim = c(0, x_hi)) +
        labs(x = "HR pro 1 SD der individuellen Steigung (log-Parenchymvolumen/Monat)",
             y = "Posteriordichte") +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom")
      ggsave(abb_path, p_dens, width = 8, height = 5, dpi = 220, bg = "white")
      cat(sprintf("[04b_JM_A] wrote %s\n", abb_path))
    } else {
      cat("[04b_JM_A] !! keine MCMC-Draws verfuegbar — Abbildung entfaellt.\n")
    }
  }

  cat("[04b_JM_A] done (alle angeforderten Kohortenfassungen ausgewertet).\n")
  invisible(NULL)
}

if (!ok_pkgs) {
  write_failure_placeholder("Paket JMbayes2 oder Dependency fehlt")
} else {
  suppressPackageStartupMessages({
    library(readxl); library(dplyr); library(stringr); library(tidyr)
    library(survival); library(splines); library(nlme); library(JMbayes2)
    library(ggplot2); library(gridExtra)
  })
  tryCatch(
    run_variante_A(),
    error = function(e) {
      message("[04b_JM_A] Fehler: ", conditionMessage(e))
      write_failure_placeholder(
        sprintf("Fehler im Variante-A-Lauf: %s", conditionMessage(e)))
    }
  )
}

# ── Mirror nach 05_Target_Structure/01_figs_tabs (LaTeX-Build sieht die ──
local({
  mirror_dir <- "./05_Target_Structure/01_figs_tabs"
  if (!dir.exists(mirror_dir)) dir.create(mirror_dir, recursive = TRUE)
  for (p in c(tab_path, abb_path)) {
    if (file.exists(p)) {
      file.copy(p, file.path(mirror_dir, basename(p)), overwrite = TRUE)
      cat(sprintf("[04b_JM_A] mirrored %s -> %s\n",
                  basename(p), mirror_dir))
    }
  }
})
