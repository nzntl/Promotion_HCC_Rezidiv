# 06d_Cox_LASSO_Main.R -- penalisierte Cox-Regression (glmnet, verschachtelte 10x10-
# Kreuzvalidierung) als Sensitivitaet zu den Modellen A bis D. Setzt die Objekte aus
# 06_Cox_Analysis.R und 06b_Cox_Chan_Postop.R voraus; schreibt py/_cache/cox_lasso_oof_lp.csv.

required <- c("sdat", "modA_vars", "modB_vars", "modC_vars", "modD_vars",
              "label_vars", "tex_escape_cell", "fig_dir")
missing  <- required[!vapply(required, exists, logical(1), envir = .GlobalEnv)]
if (length(missing) > 0) {
  stop(sprintf(
    "[06d_Cox_LASSO_Main] required objects missing: %s. ",
    paste(missing, collapse = ", "),
    "Source 02_Code/06_Cox_Analysis.R + 06b_Cox_Chan_Postop.R first."
  ))
}

suppressPackageStartupMessages({
  library(survival); library(dplyr); library(tidyr)
  library(glmnet);   library(ggplot2)
})

set.seed(20260528L)

LASSO_FULL_OUTPUTS <- FALSE

if (!exists("%||%", envir = baseenv(), inherits = FALSE)) {
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 0L) return(b)
    if (is.na(a[1L])) return(b)
    a
  }
}

N_OUTER     <- 10L
N_REPS      <- 10L
N_INNER     <- 10L
BOOTSTRAP_B <- 5000L

cache_dir <- file.path(".", "02_Code", "py", "_cache")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# ── 1. Helper: Build design matrix for a block variable set ──
build_block <- function(sdat, vars) {
  vars_present <- intersect(vars, names(sdat))
  if (length(vars_present) < length(vars)) {
    warning(sprintf("[06d] missing vars dropped: %s",
                    paste(setdiff(vars, vars_present), collapse = ", ")))
  }
  if (length(vars_present) == 0L) {
    stop(sprintf(
      paste0(
        "[06d] build_block: NONE of the requested vars are columns of sdat. ",
        "Requested: %s. ",
        "Most likely cause: 06_Cox_Analysis.R + 06b_Cox_Chan_Postop.R were ",
        "not sourced in this session (or sdat was overwritten). Source both ",
        "scripts before sourcing 06d."),
      paste(vars, collapse = ", ")
    ))
  }
  cols  <- c("Pseudonym", "time", "event", vars_present)
  df    <- sdat[, cols, drop = FALSE]
  keep  <- complete.cases(df)
  df    <- df[keep, , drop = FALSE]
  rhs   <- paste(vars_present, collapse = " + ")
  fml   <- as.formula(paste("~", rhs))
  X     <- model.matrix(fml, data = df)[, -1L, drop = FALSE]
  y     <- Surv(df$time, df$event)
  list(X = X, y = y, pseu = df$Pseudonym, n = nrow(df), n_ev = sum(df$event),
       vars = vars_present, var_cols = colnames(X))
}

# ── 2. Nested CV (10x10) ──
nested_cv_lasso <- function(blk, n_outer = N_OUTER, n_reps = N_REPS,
                            n_inner = N_INNER, alpha = 1, label = "") {
  X      <- blk$X; y <- blk$y; n <- blk$n
  pmat   <- matrix(NA_real_, n, n_reps)
  ev     <- y[, "status"]
  n_fits <- n_outer * n_reps
  beta_mat <- matrix(0.0, ncol(X), n_fits,
                     dimnames = list(colnames(X), NULL))
  lambda_1se_vec <- numeric(n_fits)
  lambda_min_vec <- numeric(n_fits)
  fit_idx <- 0L
  for (rep in seq_len(n_reps)) {
    seed_r <- 20260528L + rep
    set.seed(seed_r)
    fold_id <- integer(n)
    idx0    <- which(ev == 0); idx1 <- which(ev == 1)
    fold_id[idx0] <- sample(rep_len(seq_len(n_outer), length(idx0)))
    fold_id[idx1] <- sample(rep_len(seq_len(n_outer), length(idx1)))
    for (k in seq_len(n_outer)) {
      fit_idx <- fit_idx + 1L
      tr <- which(fold_id != k); te <- which(fold_id == k)
      Xtr <- X[tr, , drop = FALSE]; Xte <- X[te, , drop = FALSE]
      ytr <- y[tr, ]; yte <- y[te, ]
      cvfit <- tryCatch(
        cv.glmnet(Xtr, ytr, family = "cox", alpha = alpha,
                  nfolds = n_inner, standardize = TRUE,
                  type.measure = "deviance"),
        error = function(e) NULL
      )
      if (is.null(cvfit)) next
      lambda_1se_vec[fit_idx] <- cvfit$lambda.1se
      lambda_min_vec[fit_idx] <- cvfit$lambda.min
      lp_te <- as.numeric(predict(cvfit, newx = Xte, s = "lambda.min",
                                  type = "link"))
      pmat[te, rep] <- lp_te
      b <- as.numeric(coef(cvfit, s = "lambda.min"))
      beta_mat[, fit_idx] <- b
    }
  }
  oof_lp <- rowMeans(pmat, na.rm = TRUE)
  list(oof_lp = oof_lp, beta_mat = beta_mat,
       lambda_1se = lambda_1se_vec, lambda_min = lambda_min_vec)
}

# ── 3. Final-fit on full data for coefficient reporting (λ_1se / λ_min) ──
final_fit_lasso <- function(blk, alpha = 1) {
  set.seed(20260528L)
  cvfit <- cv.glmnet(blk$X, blk$y, family = "cox", alpha = alpha,
                     nfolds = N_INNER, standardize = TRUE,
                     type.measure = "deviance")
  list(cvfit = cvfit,
       beta_1se = as.numeric(coef(cvfit, s = "lambda.1se")),
       beta_min = as.numeric(coef(cvfit, s = "lambda.min")),
       lambda_1se = cvfit$lambda.1se,
       lambda_min = cvfit$lambda.min,
       var_cols = colnames(blk$X))
}

# ── 4. Bootstrap-ΔC on aggregated OOF-LPs ──
cidx <- function(tm, ev, risk) {
  cfit <- tryCatch(concordance(Surv(tm, ev) ~ risk, reverse = TRUE),
                   error = function(e) NULL)
  if (is.null(cfit)) return(NA_real_)
  cfit$concordance
}

bootstrap_dc <- function(oof_lp_small, oof_lp_large, surv_obj,
                         B = BOOTSTRAP_B, seed = 20260528L) {
  set.seed(seed)
  n <- length(oof_lp_small)
  stopifnot(length(oof_lp_large) == n)
  ok <- !is.na(oof_lp_small) & !is.na(oof_lp_large)
  idx_ok <- which(ok)
  ev <- surv_obj[, "status"]; tm <- surv_obj[, "time"]
  c_pt_small <- cidx(tm[idx_ok], ev[idx_ok], oof_lp_small[idx_ok])
  c_pt_large <- cidx(tm[idx_ok], ev[idx_ok], oof_lp_large[idx_ok])
  dc_vec <- numeric(0)
  for (b in seq_len(B)) {
    s <- sample(idx_ok, length(idx_ok), replace = TRUE)
    cs <- cidx(tm[s], ev[s], oof_lp_small[s])
    cl <- cidx(tm[s], ev[s], oof_lp_large[s])
    if (!is.na(cs) && !is.na(cl)) dc_vec <- c(dc_vec, cl - cs)
  }
  if (length(dc_vec) < 50) {
    return(list(c_small = c_pt_small, c_large = c_pt_large,
                dc_med = NA_real_, dc_lo = NA_real_, dc_hi = NA_real_,
                p_gt0 = NA_real_, n_boot = length(dc_vec)))
  }
  q <- quantile(dc_vec, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  list(c_small = c_pt_small, c_large = c_pt_large,
       dc_med = q[[2]], dc_lo = q[[1]], dc_hi = q[[3]],
       p_gt0 = mean(dc_vec > 0),
       n_boot = length(dc_vec))
}

# ── 5. Fit all 4 blocks ──
cat("[06d] building design matrices and running nested 10x10 CV...\n")
blk_A <- build_block(sdat, modA_vars)
blk_B <- build_block(sdat, modB_vars)
blk_C <- build_block(sdat, modC_vars)
blk_D <- build_block(sdat, modD_vars)
cat(sprintf("[06d]   A: n=%d events=%d feats=%d\n", blk_A$n, blk_A$n_ev,
            ncol(blk_A$X)))
cat(sprintf("[06d]   B: n=%d events=%d feats=%d\n", blk_B$n, blk_B$n_ev,
            ncol(blk_B$X)))
cat(sprintf("[06d]   C: n=%d events=%d feats=%d\n", blk_C$n, blk_C$n_ev,
            ncol(blk_C$X)))
cat(sprintf("[06d]   D: n=%d events=%d feats=%d\n", blk_D$n, blk_D$n_ev,
            ncol(blk_D$X)))

t0 <- Sys.time()
cv_A <- nested_cv_lasso(blk_A, label = "A")
cv_B <- nested_cv_lasso(blk_B, label = "B")
cv_C <- nested_cv_lasso(blk_C, label = "C")
cv_D <- nested_cv_lasso(blk_D, label = "D")
cat(sprintf("[06d] nested CV done in %.1f s\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))

ff_C <- final_fit_lasso(blk_C)
ff_D <- final_fit_lasso(blk_D)

# ── 6. Selection-frequency table ──
sel_freq <- function(beta_mat) rowMeans(beta_mat != 0)
mean_abs <- function(beta_mat) rowMeans(abs(beta_mat))
sign_consistency <- function(beta_mat) {
  pos <- rowMeans(beta_mat > 0)
  neg <- rowMeans(beta_mat < 0)
  pmax(pos, neg)
}

sel_C <- sel_freq(cv_C$beta_mat)
sel_D <- sel_freq(cv_D$beta_mat)

# ── 7. Tab 3.5b — Cox-LASSO-Koeffizienten ──
all_vars <- union(blk_D$var_cols, blk_C$var_cols)
b_C_1se <- setNames(ff_C$beta_1se, ff_C$var_cols)
b_C_min <- setNames(ff_C$beta_min, ff_C$var_cols)
b_D_1se <- setNames(ff_D$beta_1se, ff_D$var_cols)
b_D_min <- setNames(ff_D$beta_min, ff_D$var_cols)
sel_C_v <- setNames(sel_C, ff_C$var_cols)
sel_D_v <- setNames(sel_D, ff_D$var_cols)

pretty_var <- function(v) {
  if (v %in% names(var_label)) return(unname(var_label[v]))
  v
}

fmt_beta <- function(b) {
  if (is.na(b) || abs(b) < 1e-9) return("--")
  sprintf("%+.3f", b)
}

tab_3_5b <- data.frame(
  Variable = vapply(all_vars, pretty_var, character(1)),
  beta_C_min = vapply(all_vars, function(v) fmt_beta(b_C_min[v] %||% NA_real_),
                       character(1)),
  beta_D_min = vapply(all_vars, function(v) fmt_beta(b_D_min[v] %||% NA_real_),
                       character(1)),
  sel_C = vapply(all_vars, function(v) {
    val <- sel_C_v[v]; if (is.na(val)) "--" else sprintf("%.0f\\%%", val * 100)
  }, character(1)),
  sel_D = vapply(all_vars, function(v) {
    val <- sel_D_v[v]; if (is.na(val)) "--" else sprintf("%.0f\\%%", val * 100)
  }, character(1)),
  stringsAsFactors = FALSE
)
sel_key  <- vapply(all_vars,
                   function(v) max(sel_C_v[v] %||% 0, sel_D_v[v] %||% 0),
                   numeric(1))
beta_key <- abs(vapply(all_vars, function(v) b_D_min[v] %||% 0, numeric(1)))
ord <- order(-sel_key, -beta_key)
tab_3_5b <- tab_3_5b[ord, , drop = FALSE]

write_tab_3_5b <- function(tab, path) {
  hdr_cells <- c(
    "Variable",
    "$\\hat\\beta$ ($\\lambda_{min}$, C)", "$\\hat\\beta$ ($\\lambda_{min}$, D)",
    "Sel.\\,Freq.\\ C", "Sel.\\,Freq.\\ D"
  )
  align <- "lrrrr"
  body <- apply(tab, 1, function(r) {
    name <- tex_escape_cell(r[["Variable"]])
    paste(c(name, r[["beta_C_min"]], r[["beta_D_min"]],
            r[["sel_C"]], r[["sel_D"]]),
          collapse = " & ")
  })
  body <- paste0(body, " \\\\")
  out <- c(
    paste("% Cox-LASSO-Koeffizienten der Hauptmodelle C (praeop) und D",
          "(praeop + Regeneration, Chan-Aufbau) bei lambda_min (minimaler",
          "CV-Fehler, Default fuer OOF-Risiko-Scores). lambda_1se-Spalten",
          "entfernt (Review 2026-08-17): unter lambda_1se sind alle",
          "Koeffizienten 0 (Nullmodell) -- Aussage gehoert in Hinweis/Text.",
          "Sel.-Freq. = Anteil der 100 Outer-CV-Fits (10 Outer-Reps x 10",
          "Folds) mit beta != 0 bei lambda_min. Sortierung: max. Sel.-Freq.",
          "(C/D) absteigend, Tie-Break |beta_D_min|."),
    sprintf("\\begin{tabular}{%s}", align),
    "\\toprule",
    paste0(paste(hdr_cells, collapse = " & "), " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  )
  con <- file(path, "w", encoding = "UTF-8"); writeLines(out, con); close(con)
}
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
if (LASSO_FULL_OUTPUTS) write_tab_3_5b(tab_3_5b, file.path(fig_dir, "Tab_3_5b_CoxLASSO_Coefficients.tex"))
if (LASSO_FULL_OUTPUTS) cat(sprintf("[06d] wrote Tab_3_5b (%d Variablen-Zeilen)\n", nrow(tab_3_5b)))


# ── 7b. Abb 3.9d — Koeffizienten-Plot des Hauptmodells ──

n_C_sel   <- sum(abs(ff_C$beta_min) > 1e-9)
sel_C_pct <- setNames(100 * sel_C, ff_C$var_cols)
cat(sprintf(paste0("[06d] Modell C: %d Koeffizienten != 0 bei lambda_min; ",
                   "Selektionsfrequenz Tumordurchmesser %.0f %%, ",
                   "Tumoranzahl %.0f %%, FLV/TLV-Ratio %.0f %%\n"),
            n_C_sel, sel_C_pct["tum_d_sd"], sel_C_pct["lesion_multi_bin"],
            sel_C_pct["FLV_TLV_ratio_sd"]))
if (n_C_sel > 0 || round(sel_C_pct["tum_d_sd"]) != 87 ||
    round(sel_C_pct["lesion_multi_bin"]) != 79 ||
    round(sel_C_pct["FLV_TLV_ratio_sd"]) != 70) {
  cat(paste0("[06d] WARNUNG Abb 3.9d: Modell C weicht von der Prosa ab ",
             "(keine Variable; 87/79/70 %) -- Text pruefen.\n"))
}
lasso_label <- c(Quick_preop_sd    = "Quick präop (pro SD)",
                 albumin_slope_sd  = "Albumin-Steigung (pro SD)",
                 crp_slope_sd      = "CRP-Steigung (pro SD)",
                 quick_slope_sd    = "Quick-Steigung (pro SD)",
                 got_slope_sd      = "GOT-Steigung (pro SD)",
                 gpt_slope_sd      = "GPT-Steigung (pro SD)",
                 thrombos_slope_sd = "Thrombozyten-Steigung (pro SD)")
lasso_plot_df <- data.frame(var = ff_D$var_cols, beta = ff_D$beta_min,
                            sel = as.numeric(sel_D_v[ff_D$var_cols]),
                            stringsAsFactors = FALSE) |>
  filter(!is.na(beta), abs(beta) > 1e-9) |>
  mutate(HR      = exp(beta),
         Label   = ifelse(var %in% names(lasso_label),
                          unname(lasso_label[var]),
                          vapply(var, pretty_var, character(1))),
         sel_pct = 100 * sel)

ord_levels <- lasso_plot_df |> arrange(abs(beta)) |> pull(Label)

p_lasso_coef <- lasso_plot_df |>
  mutate(Label = factor(Label, levels = ord_levels)) |>
  ggplot(aes(x = HR, y = Label, colour = sel_pct)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
  geom_point(size = 3) +
  geom_label(aes(label = sprintf("%.2f, %.0f %%", HR, sel_pct)),
             hjust = -0.2, size = 2.9, colour = "grey25", fill = "white",
             linewidth = 0, label.padding = unit(0.08, "lines")) +
  scale_x_log10(breaks = c(0.95, 1.00, 1.05, 1.10, 1.15, 1.20),
                expand = expansion(mult = c(0.06, 0.30))) +
  scale_colour_gradient(low = "grey85", high = "#00441B", limits = c(0, 100),
                        name = "Selektionsfrequenz (%)") +
  labs(x = "Hazard Ratio (log-Skala)", y = NULL) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "top")

ggsave(file.path(fig_dir, "Abb_3_9d_CoxLASSO_Coefficients.png"),
       plot = p_lasso_coef, width = 11, height = 6.5, dpi = 300, bg = "white")
cat("[06d] wrote Abb_3_9d (LASSO coefficient plot, Hauptmodell)\n")


# ── 8. Tab 3.6c — Block-DeltaC im LASSO-Framework ──
all_pseu <- sdat$Pseudonym
oof_A <- rep(NA_real_, length(all_pseu)); names(oof_A) <- all_pseu
oof_A[blk_A$pseu] <- cv_A$oof_lp
oof_B <- rep(NA_real_, length(all_pseu)); names(oof_B) <- all_pseu
oof_B[blk_B$pseu] <- cv_B$oof_lp
oof_C <- rep(NA_real_, length(all_pseu)); names(oof_C) <- all_pseu
oof_C[blk_C$pseu] <- cv_C$oof_lp
oof_D <- rep(NA_real_, length(all_pseu)); names(oof_D) <- all_pseu
oof_D[blk_D$pseu] <- cv_D$oof_lp

surv_all <- Surv(sdat$time, sdat$event)

cat("[06d] bootstrap Delta-C per block ...\n")
res_BA <- bootstrap_dc(oof_A, oof_B, surv_all)
res_CB <- bootstrap_dc(oof_B, oof_C, surv_all)
res_DC <- bootstrap_dc(oof_C, oof_D, surv_all)

fmt_signed <- function(x, nd = 3) {
  if (is.na(x) || !is.finite(x)) return("--")
  sprintf("%+.*f", nd, x)
}
fmt_pair_ci <- function(lo, hi, nd = 3) {
  if (is.na(lo) || is.na(hi)) return("--")
  sprintf("(%+.*f, %+.*f)", nd, lo, nd, hi)
}
fmt_prob <- function(p) {
  if (is.na(p) || !is.finite(p)) return("--")
  sprintf("%.3f", p)
}

tab_3_6c <- data.frame(
  Vergleich = c("B vs.\\ A (präop-Labor)",
                "C vs.\\ B (präop-Volumetrie)",
                "D vs.\\ C (Regeneration)"),
  C_klein   = sprintf("%.3f", c(res_BA$c_small, res_CB$c_small, res_DC$c_small)),
  C_gross   = sprintf("%.3f", c(res_BA$c_large, res_CB$c_large, res_DC$c_large)),
  DC_med    = vapply(list(res_BA, res_CB, res_DC),
                     function(r) fmt_signed(r$dc_med), character(1)),
  DC_CI     = vapply(list(res_BA, res_CB, res_DC),
                     function(r) fmt_pair_ci(r$dc_lo, r$dc_hi),
                     character(1)),
  Pgt0      = vapply(list(res_BA, res_CB, res_DC),
                     function(r) fmt_prob(r$p_gt0), character(1)),
  stringsAsFactors = FALSE
)

write_tab_3_6c <- function(tab, path) {
  hdr <- paste(c("Vergleich",
                 "C (kleineres Modell)", "C (größeres Modell)",
                 "$\\Delta$C Median", "$\\Delta$C 95\\%-KI",
                 "P($\\Delta$C $>$ 0)"),
               collapse = " & ")
  body <- apply(tab, 1, function(r) paste(r, collapse = " & "))
  body <- paste0(body, " \\\\")
  out <- c(
    paste("% Block-Mehrwert im Cox-LASSO-Framework. C-Index (Harrell)",
          "auf den aggregierten OOF-Linear-Predictors aus 10x10 nested",
          "CV. Bootstrap-Perzentil-KI ueber B=5000 Resamples der",
          "Patienten (gemeinsame Schnittmenge mit gueltigem OOF-LP in",
          "beiden Modellen, hier durchgehend B=5000 gueltig).",
          "P(DeltaC>0) = Anteil der Resamples mit hoeherem C-Index des",
          "groesseren Modells; Signifikanz entspraeche P(DeltaC>0)>0.975",
          "bzw. untere KI-Grenze > 0."),
    "\\begin{tabular}{lrrrrr}",
    "\\toprule",
    paste0(hdr, " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  )
  con <- file(path, "w", encoding = "UTF-8"); writeLines(out, con); close(con)
}
if (LASSO_FULL_OUTPUTS) write_tab_3_6c(tab_3_6c, file.path(fig_dir, "Tab_3_6c_CoxLASSO_BlockComparison.tex"))
if (LASSO_FULL_OUTPUTS) cat("[06d] wrote Tab_3_6c\n")


# ── 9. Tab 3.6d — Top-15 Variablen-Selektion-Stabilitaet (D-Modell) ──
sel_D_df <- data.frame(
  Variable = names(sel_D),
  SelFreq  = unname(sel_D),
  MeanAbsBeta = mean_abs(cv_D$beta_mat),
  SignConsistency = sign_consistency(cv_D$beta_mat),
  stringsAsFactors = FALSE
)
sel_D_df <- sel_D_df[order(-sel_D_df$SelFreq, -sel_D_df$MeanAbsBeta), ]
top15 <- head(sel_D_df, 15)
top15$Variable <- vapply(top15$Variable, pretty_var, character(1))

write_tab_3_6d <- function(tab, path) {
  hdr <- paste(c("Variable", "Sel.\\,Freq.", "$\\overline{|\\hat\\beta|}$",
                 "Vorzeichen-Konsistenz"),
               collapse = " & ")
  body <- apply(tab, 1, function(r) {
    nm <- tex_escape_cell(as.character(r[["Variable"]]))
    sf <- as.numeric(r[["SelFreq"]])
    paste(c(nm,
            sprintf("%.0f\\%%", sf * 100),
            sprintf("%.3f", as.numeric(r[["MeanAbsBeta"]])),
            sprintf("%.0f\\%%",
                    as.numeric(r[["SignConsistency"]]) * 100)),
          collapse = " & ")
  })
  body <- paste0(body, " \\\\")
  out <- c(
    paste("% Top-15 Variablen nach Selektionsfrequenz im Cox-LASSO-Modell D",
          "(100 Outer-CV-Fits). Sel.-Freq. = Anteil mit beta != 0. Mittel",
          "der absoluten Koeffizienten-Magnitude und Vorzeichen-Konsistenz",
          "(Anteil mit dominantem Vorzeichen). Robust assoziiert:",
          "Sel.-Freq. >= 80 %."),
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    paste0(hdr, " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  )
  con <- file(path, "w", encoding = "UTF-8"); writeLines(out, con); close(con)
}
if (LASSO_FULL_OUTPUTS) write_tab_3_6d(top15, file.path(fig_dir, "Tab_3_6d_CoxLASSO_SelectionStability.tex"))
if (LASSO_FULL_OUTPUTS) cat("[06d] wrote Tab_3_6d (Top-15 D)\n")


# ── 10. Konvergenz LASSO vs Full-Model (Tab_LASSO_vs_FullModel_Agreement) ──
agree_rows <- list()
if (exists("fitC", envir = .GlobalEnv) && !is.null(get("fitC", envir = .GlobalEnv))) {
  fc <- get("fitC", envir = .GlobalEnv)
  s_fc <- summary(fc)
  pv_fc <- s_fc$coefficients[, "Pr(>|z|)"]
  for (v in ff_C$var_cols) {
    sf <- sel_C_v[v]
    in_full <- v %in% names(pv_fc)
    pval    <- if (in_full) pv_fc[v] else NA_real_
    sel_lasso <- !is.na(sf) && sf >= 0.50
    sig_full  <- !is.na(pval) && pval < 0.05
    cat_lbl <- if (sel_lasso && sig_full) "beide"
              else if (sel_lasso)         "nur LASSO"
              else if (sig_full)          "nur Full-Model"
              else                         "keiner"
    agree_rows[[length(agree_rows) + 1L]] <- data.frame(
      Variable = pretty_var(v),
      SelFreq  = if (is.na(sf)) "--" else sprintf("%.0f%%", sf * 100),
      `Full-Model p` = if (is.na(pval)) "--" else sprintf("%.3f", pval),
      Kategorie = cat_lbl,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
}
if (LASSO_FULL_OUTPUTS && length(agree_rows) > 0) {
  agree_tab <- do.call(rbind, agree_rows)
  sf_num    <- suppressWarnings(as.numeric(sub("%", "", agree_tab$SelFreq)))
  agree_tab <- agree_tab[order(agree_tab$Kategorie,
                               -ifelse(is.na(sf_num), -1, sf_num)), ]
  hdr <- "Variable & Sel.\\,Freq.\\ (LASSO C) & Full-Model p (C) & Kategorie"
  body <- apply(agree_tab, 1, function(r)
    paste(tex_escape_cell(r), collapse = " & "))
  body <- paste0(body, " \\\\")
  out <- c(
    paste("% Konvergenz Cox-LASSO vs Full-Model-Cox (Modell C). Sel.-Freq.",
          "= Anteil der 100 Outer-CV-Fits mit beta != 0 im LASSO. Full-Model",
          "p aus klassischer multivariater Cox-Regression (Tab 3.6). Robuste",
          "Konvergenz wenn 'beide' dominant."),
    "\\begin{tabular}{lrrl}", "\\toprule",
    paste0(hdr, " \\\\"), "\\midrule", body, "\\bottomrule", "\\end{tabular}"
  )
  path <- file.path(fig_dir, "Tab_LASSO_vs_FullModel_Agreement.tex")
  con <- file(path, "w", encoding = "UTF-8"); writeLines(out, con); close(con)
  cat(sprintf("[06d] wrote Tab_LASSO_vs_FullModel_Agreement (%d rows)\n",
              nrow(agree_tab)))
}


# ── 11. Abb 3.9b — Regularisierungs-Pfade (C + D als 2-Panel) ──
if (LASSO_FULL_OUTPUTS) {
abb_path <- file.path(fig_dir, "Abb_3_9b_LASSO_RegularisationPath.png")
png(abb_path, width = 1400, height = 700, res = 130)
op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 2.5, 1), bg = "white")
fit_path_C <- glmnet(blk_C$X, blk_C$y, family = "cox", alpha = 1,
                     standardize = TRUE)
fit_path_D <- glmnet(blk_D$X, blk_D$y, family = "cox", alpha = 1,
                     standardize = TRUE)
plot_lasso_path <- function(fit, lambda_1se, lambda_min, ttl) {
  B  <- as.matrix(fit$beta)
  ll <- log(fit$lambda)
  cols <- grDevices::hcl.colors(max(nrow(B), 1L), "Dark 3")
  matplot(ll, t(B), type = "l", lty = 1, lwd = 1.3, col = cols,
          xlab = expression(log(lambda)), ylab = "Koeffizient")
  abline(h = 0, col = "grey75", lwd = 0.8)
  abline(v = log(lambda_1se), lty = 2, col = "#C7261B")
  abline(v = log(lambda_min), lty = 3, col = "#00441B", lwd = 1.6)
  legend("bottomleft",
         legend = c(expression(lambda[
           "1se"] ~ "(verworfen: Nullmodell)"),
           expression(lambda[min] ~ "(Hauptmodell)")),
         lty = c(2, 3), col = c("#C7261B", "#00441B"), lwd = c(1, 1.6),
         bty = "n", cex = 0.8)
  title(main = ttl, line = 1)
}
plot_lasso_path(fit_path_C, ff_C$lambda_1se, ff_C$lambda_min, "Modell C (+ präop Volumetrie)")
plot_lasso_path(fit_path_D, ff_D$lambda_1se, ff_D$lambda_min, "Modell D (+ Regeneration)")
par(op); dev.off()
cat("[06d] wrote Abb_3_9b (LASSO regularisation path)\n")
}


# ── 12. Abb 3.9c — Stabilitaets-Heatmap (D-Modell) ──
if (LASSO_FULL_OUTPUTS) {
heat_path <- file.path(fig_dir, "Abb_3_9c_LASSO_StabilityHeatmap.png")
ord_v <- order(-sel_D)
beta_disp <- cv_D$beta_mat[ord_v, , drop = FALSE]
var_labels <- vapply(rownames(beta_disp), pretty_var, character(1))
df_heat <- as.data.frame(beta_disp)
df_heat$Variable <- factor(var_labels, levels = rev(var_labels))
df_long <- tidyr::pivot_longer(df_heat, -Variable,
                                names_to = "Fit", values_to = "beta")
df_long$Fit_num <- as.integer(sub("V", "", df_long$Fit))
p_heat <- ggplot(df_long, aes(x = Fit_num, y = Variable, fill = beta)) +
  geom_tile() +
  scale_fill_gradient2(low = "#3B6FAB", mid = "white", high = "#C7261B",
                        midpoint = 0, name = expression(hat(beta))) +
  labs(x = "Outer-CV-Fit (1-100)", y = NULL,
       title = "Cox-LASSO Modell D — Variablen-Selektions-Stabilität") +
  theme_minimal(base_family = "sans") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        legend.position = "right",
        panel.grid = element_blank())
ggsave(heat_path, plot = p_heat, width = 11, height = 6, dpi = 140,
       bg = "white")
cat("[06d] wrote Abb_3_9c (stability heatmap)\n")
}


# ── 13. Export OOF-LPs fuer stage4 Modellvergleich ──
oof_csv_path <- file.path(cache_dir, "cox_lasso_oof_lp.csv")
oof_df <- data.frame(
  Pseudonym = all_pseu,
  oof_lp_A_lasso = oof_A,
  oof_lp_B_lasso = oof_B,
  oof_lp_C_lasso = oof_C,
  oof_lp_D_lasso = oof_D,
  stringsAsFactors = FALSE
)
oof_df <- oof_df[!duplicated(oof_df$Pseudonym), ]
write.csv(oof_df, oof_csv_path, row.names = FALSE)
cat(sprintf("[06d] wrote %s (n=%d patients)\n", oof_csv_path, nrow(oof_df)))


assign("cv_C_lasso", cv_C, envir = .GlobalEnv)
assign("cv_D_lasso", cv_D, envir = .GlobalEnv)
assign("ff_C_lasso", ff_C, envir = .GlobalEnv)
assign("ff_D_lasso", ff_D, envir = .GlobalEnv)

cat("[06d] Cox-LASSO main done.\n")
