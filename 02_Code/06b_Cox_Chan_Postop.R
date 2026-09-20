# 06b_Cox_Chan_Postop.R -- Cox-Modell D (postoperative Regeneration) als Erweiterung von
# Modell C. Setzt die Objekte aus 06_Cox_Analysis.R voraus (im selben R-Lauf sourcen).

required <- c("sdat", "modC_vars", "label_vars", "tex_escape_cell", "fig_dir")
missing  <- required[!vapply(required, exists, logical(1), envir = .GlobalEnv)]
if (length(missing) > 0) {
  stop(sprintf(
    "[06b_Cox_Chan_Postop] required objects missing: %s. ",
    paste(missing, collapse = ", "),
    "Source 02_Code/06_Cox_Analysis.R first."
  ))
}

suppressPackageStartupMessages({
  library(survival); library(dplyr); library(tidyr)
})

BOOTSTRAP_B <- 5000L
set.seed(20260527L)

# ── Modell D variable set ──
modD_extra <- c("LVR_index_sd",
                "quick_slope_sd",
                "got_slope_sd", "gpt_slope_sd",
                "crp_slope_sd", "albumin_slope_sd",
                "thrombos_slope_sd")
modD_vars  <- c(modC_vars, modD_extra)

sdat_D <- sdat[complete.cases(sdat[, c("time", "event", modD_vars)]), ]
n_D <- nrow(sdat_D); e_D <- sum(sdat_D$event)

cat(sprintf("[06b_Cox_Chan_Postop] complete-case Subkohorte fuer Modell D: n=%d (events=%d)\n",
            n_D, e_D))

if (n_D < 50 || e_D < 20) {
  warning("[06b_Cox_Chan_Postop] n or events too small for stable Cox D fit. ",
          "Outputs werden trotzdem geschrieben, sind aber als rein deskriptiv zu lesen.")
}

fitC_d <- tryCatch(coxph(reformulate(modC_vars, "Surv(time, event)"),
                         data = sdat_D),
                   error = function(e) NULL)
fitD   <- tryCatch(coxph(reformulate(modD_vars, "Surv(time, event)"),
                         data = sdat_D),
                   error = function(e) NULL)

if (is.null(fitC_d) || is.null(fitD)) {
  stop("[06b_Cox_Chan_Postop] Cox-Fit fehlgeschlagen (C oder D = NULL).")
}

cC <- summary(fitC_d)$concordance["C"]
cD <- summary(fitD)$concordance["C"]
cat(sprintf("[06b_Cox_Chan_Postop] C-Index: C=%.3f, D=%.3f, dC=%+.3f (point estimate)\n",
            cC, cD, cD - cC))

# ── Schoenfeld / PH assumption for Modell D ──
ph_D <- tryCatch(cox.zph(fitD), error = function(e) NULL)
if (!is.null(ph_D)) {
  cat("[06b_Cox_Chan_Postop] Schoenfeld-Test Modell D (per term + GLOBAL):\n")
  print(round(ph_D$table, 4))
  cat(sprintf("[06b_Cox_Chan_Postop] Schoenfeld GLOBAL Modell D: p=%.3f\n",
              ph_D$table["GLOBAL", "p"]))
} else {
  warning("[06b_Cox_Chan_Postop] cox.zph for Modell D failed.")
}

# ── Bootstrap ΔC ──
boot_dc <- numeric(0)
boot_fail <- 0L
n_rows <- nrow(sdat_D)
t0 <- Sys.time()

c_on <- function(fit, newdata) {
  lp <- predict(fit, newdata = newdata, type = "lp")
  if (all(is.na(lp)) || stats::sd(lp, na.rm = TRUE) == 0) return(NA_real_)
  concordance(Surv(time, event) ~ lp, data = newdata, reverse = TRUE)$concordance
}

for (b in seq_len(BOOTSTRAP_B)) {
  idx <- sample.int(n_rows, n_rows, replace = TRUE)
  oob <- setdiff(seq_len(n_rows), unique(idx))
  d_b   <- sdat_D[idx, , drop = FALSE]
  d_oob <- sdat_D[oob, , drop = FALSE]
  if (sum(d_oob$event, na.rm = TRUE) < 20) { boot_fail <- boot_fail + 1L; next }
  fb <- tryCatch({
    fc <- coxph(reformulate(modC_vars, "Surv(time, event)"), data = d_b)
    fd <- coxph(reformulate(modD_vars, "Surv(time, event)"), data = d_b)
    c_on(fd, d_oob) - c_on(fc, d_oob)
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
  if (is.na(fb)) { boot_fail <- boot_fail + 1L; next }
  boot_dc <- c(boot_dc, fb)
}

.dC_apparent <- summary(fitD)$concordance["C"] - summary(fitC_d)$concordance["C"]
cat(sprintf("[06b_Cox_Chan_Postop] Delta-C apparent (unkorrigiert) = %+.4f
",
            .dC_apparent))
boot_elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("[06b_Cox_Chan_Postop] Bootstrap: B=%d, success=%d, fail=%d, elapsed=%.1f s\n",
            BOOTSTRAP_B, length(boot_dc), boot_fail, boot_elapsed))

if (length(boot_dc) < 50) {
  warning("[06b_Cox_Chan_Postop] Bootstrap success rate too low (<50 valid replicates).")
  dC_ci_lo <- NA_real_; dC_ci_hi <- NA_real_; dC_med <- NA_real_
} else {
  q <- quantile(boot_dc, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  dC_ci_lo <- q[[1]]; dC_med <- q[[2]]; dC_ci_hi <- q[[3]]
}

# ── Likelihood-Ratio-Test D vs C ──
lrt <- tryCatch(anova(fitC_d, fitD), error = function(e) NULL)
lrt_chi <- NA_real_; lrt_df <- NA_integer_; lrt_p <- NA_real_
if (!is.null(lrt)) {
  lrt_chi <- lrt[2, "Chisq"]; lrt_df <- as.integer(lrt[2, "Df"])
  lrt_p   <- lrt[2, "Pr(>|Chi|)"]
}

# ── Build coefficient table (Modell C | Modell D nebeneinander) ──
extract_coef <- function(fit) {
  s <- summary(fit)
  ci <- s$conf.int; pv <- s$coefficients[, "Pr(>|z|)"]
  data.frame(
    Variable_raw = rownames(ci),
    HRCI = sprintf("%.2f (%.2f--%.2f)",
                   ci[, "exp(coef)"], ci[, "lower .95"], ci[, "upper .95"]),
    p    = ifelse(pv < 0.001, "<0.001",
                  ifelse(pv > 0.999, ">0.999", sprintf("%.3f", pv))),
    stringsAsFactors = FALSE
  )
}
cf_C <- extract_coef(fitC_d); names(cf_C)[-1] <- paste0(names(cf_C)[-1], "_C")
cf_D <- extract_coef(fitD);   names(cf_D)[-1] <- paste0(names(cf_D)[-1], "_D")

vars_union <- unique(c(cf_C$Variable_raw, cf_D$Variable_raw))
tab_cd <- data.frame(Variable_raw = vars_union, stringsAsFactors = FALSE) |>
  left_join(cf_C, by = "Variable_raw") |>
  left_join(cf_D, by = "Variable_raw") |>
  mutate(across(-Variable_raw, ~ ifelse(is.na(.), "--", .))) |>
  mutate(Variable = label_vars(Variable_raw), Key = Variable_raw) |>
  select(Variable, HRCI_C, p_C, HRCI_D, p_D, Key)

tab_cd <- ref_zeile(tab_cd, "männlich", "weiblich", c("HRCI_C", "HRCI_D"),
                    "ref_sex")
tab_cd <- ref_zeile(tab_cd, "Ätiologie: MASLD", "Ätiologie: Rest",
                    c("HRCI_C", "HRCI_D"), "ref_aetio")

steigung_label <- c(
  albumin_slope_sd  = "Albumin (pro SD)",
  quick_slope_sd    = "Quick (pro SD)",
  got_slope_sd      = "GOT (pro SD)",
  gpt_slope_sd      = "GPT (pro SD)",
  crp_slope_sd      = "CRP (pro SD)",
  thrombos_slope_sd = "Thrombozyten (pro SD)")
ist_steigung <- tab_cd$Key %in% names(steigung_label)
tab_cd$Variable[ist_steigung] <- unname(steigung_label[tab_cd$Key[ist_steigung]])

p_schoen <- function(p) {
  if (!is.finite(p)) return("--")
  if (p < 0.001) "<0.001" else if (p > 0.999) ">0.999" else sprintf("%.3f", p)
}
global_ph_D <- NA_character_
tab_cd$`Schoenfeld p` <- ""
ph_D <- tryCatch(cox.zph(fitD), error = function(e) NULL)
if (!is.null(ph_D)) {
  pht     <- ph_D$table
  is_glob <- rownames(pht) == "GLOBAL"
  for (tv in rownames(pht)[!is_glob]) {
    x   <- if (tv %in% names(sdat_D)) sdat_D[[tv]] else NULL
    raw <- if (is.factor(x)) paste0(tv, levels(x)[-1]) else tv
    tab_cd$`Schoenfeld p`[tab_cd$Key %in% raw] <- p_schoen(pht[tv, "p"])
  }
  if (any(is_glob)) global_ph_D <- p_schoen(pht[is_glob, "p"])
}

gruppen_cd <- list(
  "Präoperativ: Person und Anamnese" = c("age_c", "ref_sex", "sex_m"),
  "Präoperativ: Komorbidität und Ätiologie" = c(
      "dm_bin", "nikotin_bin", "ref_aetio", "aetio_modelMASLD",
      "aetio_modelviral", "aetio_modelethyltox"),
  "Präoperativ: Leberfunktion und Labor (POD $-$1)" = c(
      "cirrhosis", "Quick_preop_sd",
      "albi_grade_fGrade 2", "albi_grade_fGrade 3"),
  "Präoperativ: Volumetrie" = c("FLV_TLV_ratio_sd", "ttlvr_sd"),
  "Postoperativ: histopathologischer Befund" = c("tum_d_sd",
      "lesion_multi_bin"),
  "Postoperativ: Labor-Steigung (POD 1 bis 7)" = c(
      "albumin_slope_sd", "quick_slope_sd", "got_slope_sd", "gpt_slope_sd",
      "crp_slope_sd", "thrombos_slope_sd"),
  "Postoperativ: Bildgebung und Volumetrie" = c("LVR_index_sd")
)
tab38_path <- file.path(fig_dir, "Tab_3_8_Cox_Chan_Postop_Landscape.tex")
zeig_cd <- c("Variable", "HRCI_C", "p_C", "HRCI_D", "p_D", "Schoenfeld p")
n_sp_cd <- length(zeig_cd)
body <- character(0)
gezeigt_cd <- character(0)
for (g in names(gruppen_cd)) {
  idx <- match(gruppen_cd[[g]], tab_cd$Key)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) next
  body <- c(body, sprintf("\\multicolumn{%d}{@{}l}{\\textit{%s}} \\\\[2pt]",
                          n_sp_cd, g))
  for (i in idx) {
    zellen <- unlist(tab_cd[i, zeig_cd])
    zellen[1] <- tex_escape_cell(zellen[1])
    body <- c(body, paste0(paste(zellen, collapse = " & "), " \\\\"))
  }
  body <- c(body, "\\addlinespace")
  gezeigt_cd <- c(gezeigt_cd, tab_cd$Key[idx])
}
if (length(body) > 0 && body[length(body)] == "\\addlinespace") {
  body <- body[-length(body)]
}
fehlend_cd <- setdiff(tab_cd$Key, gezeigt_cd)
if (length(fehlend_cd) > 0) {
  message("[WARNUNG Tab 3.11] nicht zugeordnet: ",
          paste(fehlend_cd, collapse = ", "),
          " -- diese Zeilen fehlen in der Ausgabe. gruppen_cd ergaenzen.")
}
fuss_cd <- function(lbl, w_c, w_d) {
  paste0(paste(c(lbl, sprintf("\\multicolumn{2}{r}{%s}", c(w_c, w_d)), ""),
               collapse = " & "), " \\\\")
}
body <- c(body, "\\midrule",
          fuss_cd("N", fitC_d$n, fitD$n),
          fuss_cd("Ereignisse", fitC_d$nevent, fitD$nevent))
if (!is.na(global_ph_D)) {
  body <- c(body, paste0(paste(c("Gesamtmodell (PH-Test)",
                                 rep("", n_sp_cd - 2), global_ph_D),
                               collapse = " & "), " \\\\"))
}
align <- "lrrrrr"

caption_38 <- paste0(
  "% Cox-Modell C (Routinebefunde + praeop Labor + praeop Volumetrie) vs.\\ Cox-Modell D ",
  "(Chan-Postop-Aufbau: + LVR-Index (TLV 1. postop / FLV praeop) + Labor-Slopes Quick/GOT/GPT/CRP/Albumin POD 1-7). ",
  sprintf("Komplette-Case-Subkohorte n=%d, Events=%d. ", n_D, e_D),
  "HR pro 1 SD fuer kontinuierliche Variablen, ALBI-Grad als 3-stufiger Faktor ",
  "(Referenz Grad 1). Delta TLV vs. FLV nicht aufgenommen wegen perfekter ",
  "Kolinearitaet zu LVR-Index nach z-Standardisierung (siehe Univariate Tab 3.5).")

out_38 <- c(
  caption_38,
  sprintf("\\begin{tabular}{%s}", align),
  "\\toprule",
  paste0(" & \\multicolumn{2}{c}{Modell C (+ präop Volumetrie)}",
         " & \\multicolumn{2}{c}{Modell D (+ Regeneration)} &  \\\\"),
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Variable & HR (95\\% KI) & p & HR (95\\% KI) & p & Schoenfeld p \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}"
)
con_38 <- file(tab38_path, "w", encoding = "UTF-8")
writeLines(out_38, con_38); close(con_38)
cat(sprintf("[06b_Cox_Chan_Postop] wrote %s (%d Variablen-Zeilen)\n",
            tab38_path, nrow(tab_cd)))

fmt_p_compact <- function(p) {
  if (is.null(p) || is.na(p) || !is.finite(p)) return("--")
  if (p < 0.001) "<0.001" else if (p > 0.999) ">0.999" else sprintf("%.3f", p)
}

# ── Tab_GOF_Cox_A_D — kombinierte Goodness-of-Fit-Tabelle Modelle A-D ──
tab_gof_ad_path <- file.path(fig_dir, "Tab_GOF_Cox_A_D.tex")

gof_row <- function(fit, mod_lbl) {
  if (is.null(fit)) return(NULL)
  s    <- summary(fit)
  cidx <- s$concordance["C"]; cse <- s$concordance["se(C)"]
  lr   <- s$logtest
  data.frame(
    Modell     = mod_lbl,
    N          = sprintf("%d", s$n),
    Ereignisse = sprintf("%d", s$nevent),
    C          = sprintf("%.3f (%.3f--%.3f)",
                         cidx, cidx - 1.96 * cse, cidx + 1.96 * cse),
    Chi        = sprintf("%.2f", lr["test"]),
    df         = sprintf("%d", as.integer(lr["df"])),
    p          = fmt_p_compact(lr["pvalue"]),
    stringsAsFactors = FALSE)
}
gof_delta_row <- function(fit_small, fit_large, lbl) {
  if (is.null(fit_small) || is.null(fit_large)) return(NULL)
  a <- tryCatch(anova(fit_small, fit_large), error = function(e) NULL)
  if (is.null(a)) return(NULL)
  data.frame(
    Modell     = lbl,
    N          = sprintf("%d", fit_small$n),
    Ereignisse = sprintf("%d", fit_small$nevent),
    C          = "--",
    Chi        = sprintf("%.2f", a[2, "Chisq"]),
    df         = sprintf("%d", as.integer(a[2, "Df"])),
    p          = fmt_p_compact(a[2, "Pr(>|Chi|)"]),
    stringsAsFactors = FALSE)
}

n_I <- NA_integer_
rows_06 <- NULL; deltas_06 <- NULL
have_06 <- all(vapply(c("fitA_cc", "fitB_cc", "fitC_cc"),
                      function(x) exists(x, envir = .GlobalEnv), logical(1)))
if (have_06) {
  fA <- get("fitA_cc", envir = .GlobalEnv)
  fB <- get("fitB_cc", envir = .GlobalEnv)
  fC <- get("fitC_cc", envir = .GlobalEnv)
  if (exists("sdat_common", envir = .GlobalEnv)) {
    n_I <- nrow(get("sdat_common", envir = .GlobalEnv))
  }
  rows_06 <- bind_rows(
    gof_row(fA, "Modell A (Routinebefunde)"),
    gof_row(fB, "Modell B (+ präop Labor)"),
    gof_row(fC, "Modell C (+ präop Volumetrie)"))
  deltas_06 <- bind_rows(
    gof_delta_row(fA, fB, "$\\Delta$ B vs.\\ A"),
    gof_delta_row(fB, fC, "$\\Delta$ C vs.\\ B"))
} else {
  cat(paste0("[06b_Cox_Chan_Postop] WARNUNG: 06-Objekte (fitA_cc/fitB_cc/",
             "fitC_cc) fehlen — Tab_GOF_Cox_A_D enthaelt nur die C/D-Haelfte.\n"))
}

rows_06b <- bind_rows(
  gof_row(fitC_d, "Modell C (Vergleichsbasis für D)"),
  gof_row(fitD,   "Modell D (+ Regeneration)"))
delta_D <- data.frame(
  Modell     = "$\\Delta$ D vs.\\ C",
  N          = sprintf("%d", n_D),
  Ereignisse = sprintf("%d", e_D),
  C          = if (is.na(dC_med)) "--"
               else sprintf("%+.3f (%+.3f--%+.3f)", dC_med, dC_ci_lo, dC_ci_hi),
  Chi        = if (is.na(lrt_chi)) "--" else sprintf("%.2f", lrt_chi),
  df         = if (is.na(lrt_chi)) "--" else sprintf("%d", lrt_df),
  p          = fmt_p_compact(lrt_p),
  stringsAsFactors = FALSE)

gof_models <- bind_rows(rows_06, rows_06b)
gof_deltas <- bind_rows(deltas_06, delta_D)

hdr_gof <- paste(c("Modell", "N", "Ereignisse",
                   "C-Index (95\\% KI)",
                   "\\makecell[r]{LR-$\\chi^2$\\\\gegen Nullmodell}",
                   "df", "p"), collapse = " & ")
hdr_delta <- paste(c("Vergleich", "N", "Ereignisse",
                     "$\\Delta$ C-Index (95\\% KI)",
                     "\\makecell[r]{LR-$\\chi^2$\\\\gegen Vormodell}",
                     "df", "p"), collapse = " & ")
align_gof <- ">{\\raggedright\\arraybackslash}p{5.2cm}rrrrrr"
row_tex <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  paste0(apply(df, 1, paste, collapse = " & "), " \\\\")
}
n_I_txt <- if (is.na(n_I)) "?" else sprintf("%d", n_I)
comment_gof <- c(
  paste0("% Kombinierte Goodness-of-Fit-Tabelle A-D (Review 2026-08-18). ",
         "Subkohorten: A-C auf gemeinsamer CC-Basis (n=", n_I_txt, "), ",
         "C/D auf der Subkohorte mit vollstaendiger postoperativer Information (n=", n_D, "). ",
         "Modell C erscheint zweimal mit leicht unterschiedlichen Kennwerten ",
         "-- Folge der unterschiedlichen Datenbasis, kein Rechenfehler."),
  paste0("% Zweite Kopfzeile trennt die Vergleichszeilen ab: dort ist die ",
         "C-Index-Spalte eine Differenz und der LR-Test laeuft gegen das ",
         "naechstkleinere Modell, nicht gegen das Nullmodell."),
  paste0("% Zeile Delta D vs. C: Bootstrap-Delta-C (Median, ",
         "Perzentil-95%-KI); Chi^2/df/p = LRT D vs. C."))

out_gof <- c(
  comment_gof,
  sprintf("\\begin{tabular}{%s}", align_gof),
  "\\toprule",
  paste0(hdr_gof, " \\\\"),
  "\\midrule",
  row_tex(gof_models),
  if (nrow(gof_deltas) > 0)
    c("\\midrule", paste0(hdr_delta, " \\\\"), "\\midrule") else NULL,
  row_tex(gof_deltas),
  "\\bottomrule",
  "\\end{tabular}")
con_gof <- file(tab_gof_ad_path, "w", encoding = "UTF-8")
writeLines(out_gof, con_gof); close(con_gof)
cat(sprintf("[06b_Cox_Chan_Postop] wrote %s (%d Modell- + %d Delta-Zeilen)\n",
            tab_gof_ad_path, nrow(gof_models), nrow(gof_deltas)))

# ── Sensitivitaet: erste Volumetrie vor dem Ereignistag (Log-only) ──
pod_first_D <- suppressWarnings(as.numeric(sdat_D$POD_first_postop_volumetry))
on_day_D    <- sdat_D$event == 1 & !is.na(pod_first_D) & pod_first_D == sdat_D$time
after_day_D <- sdat_D$event == 1 & !is.na(pod_first_D) & pod_first_D >  sdat_D$time
sdat_pre    <- sdat_D[!(on_day_D | after_day_D), , drop = FALSE]
cat(sprintf("[06b praeevent] Modell-D-Subkohorte n=%d (events=%d): erste Volumetrie am Ereignistag %d, nach dem Ereignistag %d
",
            n_D, e_D, sum(on_day_D), sum(after_day_D)))
fitD_pre <- tryCatch(coxph(reformulate(modD_vars, "Surv(time, event)"), data = sdat_pre),
                     error = function(e) NULL)
hr_line <- function(fit, var) {
  s <- summary(fit)$conf.int; p <- summary(fit)$coefficients[var, "Pr(>|z|)"]
  sprintf("HR=%.2f (%.2f--%.2f), p=%.3f", s[var, "exp(coef)"], s[var, "lower .95"], s[var, "upper .95"], p)
}
if (!is.null(fitD_pre)) {
  cat(sprintf("[06b praeevent] nur erste Volumetrie vor dem Ereignistag: n=%d (events=%d), EPV=%.1f
",
              nrow(sdat_pre), sum(sdat_pre$event), sum(sdat_pre$event) / length(coef(fitD_pre))))
  cat(sprintf("[06b praeevent] LVR-Index: volle Fassung %s | vor Ereignistag %s
",
              hr_line(fitD, "LVR_index_sd"), hr_line(fitD_pre, "LVR_index_sd")))
  cat(sprintf("[06b praeevent] Albumin-Slope: volle Fassung %s | vor Ereignistag %s
",
              hr_line(fitD, "albumin_slope_sd"), hr_line(fitD_pre, "albumin_slope_sd")))
} else {
  cat("[06b praeevent] Cox-Fit der Prae-Ereignis-Fassung fehlgeschlagen.
")
}

assign("fitD", fitD, envir = .GlobalEnv)
