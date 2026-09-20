# 03b_Hazard_Plots.R -- kumulative Inzidenz (Aalen-Johansen) und geglaettete Hazard-Rate
# je Aetiologie. Liest 01_Daten/data_static.xlsx, data_dynamic_surv.xlsx.

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(survival); library(bshazard); library(ggplot2); library(gridExtra)
})

fig_dir <- "./03_Tables_Figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

static <- read_excel("./01_Daten/data_static.xlsx")
surv_c <- read_excel("./01_Daten/data_dynamic_surv.xlsx")

analysis <- static |>
  filter(is.na(Rezidivtumor) | str_to_lower(Rezidivtumor) != "ja") |>
  select(Pseudonym, äthiologie_cat) |>
  inner_join(surv_c, by = "Pseudonym") |>
  mutate(
    aetio = {
      a <- äthiologie_cat
      a[a %in% c("Andere", "Unbekannt")] <- "Andere/Unbekannt"
      factor(a, levels = c("HBV", "HCV", "Ethyltox", "MASLD", "Andere/Unbekannt"),
             labels = c("HBV", "HCV", "Ethyltoxisch", "MASLD", "Andere/Unbekannt"))
    },
    ev_f = factor(cr_status, levels = c(0, 1, 2),
                  labels = c("censor", "Rezidiv", "Tod ohne Rezidiv"))
  ) |>
  filter(!is.na(time), time > 0, !is.na(aetio))

cat(sprintf("[03b_Hazard_Plots] Analyse-Kohorte n=%d (Rezidive=%d, konk. Tode=%d)\n",
            nrow(analysis), sum(analysis$cr_status == 1),
            sum(analysis$cr_status == 2)))

pal <- c(
  "HBV"              = "#1f77b4",
  "HCV"              = "#9467bd",
  "Ethyltoxisch"     = "#00441B",
  "MASLD"            = "#d62728",
  "Andere/Unbekannt" = "#7f7f7f"
)

n_lab <- analysis |>
  group_by(aetio) |>
  summarise(n = n(), rez = sum(cr_status == 1), .groups = "drop")
aetio_lab <- setNames(sprintf("%s (%d/%d)", n_lab$aetio, n_lab$n, n_lab$rez),
                      as.character(n_lab$aetio))
cat(sprintf("[03b_Hazard_Plots] Fallzahl je Gruppe: %s\n", aetio_lab))

max_days <- 1825
day_to_month <- function(d) d / 30.4375

# ── (1) Aalen-Johansen-CIF pro Ätiologie, beide Ereignistypen ──
ci_dfs <- analysis |>
  group_by(aetio) |>
  group_split() |>
  lapply(function(d) {
    sf <- survfit(Surv(time, ev_f) ~ 1, data = d)
    st_names <- sf$states
    out <- lapply(setdiff(seq_along(st_names), 1), function(j) {
      data.frame(
        aetio     = unique(d$aetio),
        Ereignis  = st_names[j],
        time_days = sf$time,
        cuminc    = sf$pstate[, j],
        lower     = sf$lower[, j],
        upper     = sf$upper[, j]
      )
    })
    bind_rows(out)
  })
ci_df <- bind_rows(ci_dfs) |>
  filter(time_days <= max_days, Ereignis != "censor") |>
  mutate(time_mo = day_to_month(time_days),
         Ereignis = factor(Ereignis,
                           levels = c("Rezidiv", "Tod ohne Rezidiv")))

p_ci <- ggplot(ci_df, aes(x = time_mo, y = cuminc, colour = aetio,
                          linetype = Ereignis)) +
  geom_step(linewidth = 0.7) +
  scale_colour_manual(values = pal, labels = aetio_lab, name = "Ätiologie") +
  scale_linetype_manual(values = c("Rezidiv" = "solid",
                                   "Tod ohne Rezidiv" = "22"),
                        name = "Ereignis") +
  scale_x_continuous(breaks = seq(0, 60, 12), limits = c(0, 60)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(x = "Monate seit OP",
       y = "Kumulative Inzidenz (Aalen-Johansen)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", legend.box = "vertical")

# ── (2) Hazard via bshazard pro Ätiologie, mit Risikoset-Abschneidung ──
riskset_cut <- function(d, min_at_risk = 15) {
  tt <- sort(d$time)
  n  <- length(tt)
  at_risk <- n - seq_along(tt) + 1
  idx <- which(at_risk < min_at_risk)
  if (length(idx) == 0) max(tt) else tt[min(idx)]
}

hz_dfs <- analysis |>
  group_by(aetio) |>
  group_split() |>
  lapply(function(d) {
    e <- sum(d$event)
    if (e < 10) {
      message(sprintf("[03b_Hazard_Plots] %s: nur %d Events — Hazard-Schaetzung uebersprungen.",
                      as.character(d$aetio[1]), e))
      return(NULL)
    }
    hz <- tryCatch(
      bshazard(Surv(time, event) ~ 1, data = as.data.frame(d), verbose = FALSE),
      error = function(e) NULL
    )
    if (is.null(hz)) {
      message(sprintf("[03b_Hazard_Plots] %s: bshazard-Fit fehlgeschlagen.",
                      as.character(d$aetio[1])))
      return(NULL)
    }
    t_cut <- riskset_cut(d, 15)
    lam_hz <- hz$phi / hz$sv2
    cat(sprintf("[03b_Hazard_Plots] %s: eff. df = %.1f, lambda = %.3g, Abschneidung bei %.0f d (< 15 at risk)\n",
                as.character(d$aetio[1]), hz$df, lam_hz, t_cut))
    data.frame(
      aetio     = unique(d$aetio),
      time_days = hz$time,
      hazard    = hz$hazard,
      lower     = hz$lower.ci,
      upper     = hz$upper.ci,
      eff_df    = hz$df
    ) |> filter(time_days <= t_cut)
  })
hz_df <- bind_rows(Filter(Negate(is.null), hz_dfs)) |>
  filter(time_days <= max_days) |>
  mutate(time_mo = day_to_month(time_days),
         hazard_pm = hazard * 30.4375 * 100,
         lower_pm  = lower  * 30.4375 * 100,
         upper_pm  = upper  * 30.4375 * 100)

df_note <- hz_df |>
  distinct(aetio, eff_df) |>
  arrange(aetio) |>
  (\(d) paste(sprintf("%s %.1f", d$aetio, d$eff_df), collapse = " | "))()

p_hz <- ggplot(hz_df, aes(x = time_mo, y = hazard_pm,
                          colour = aetio, fill = aetio)) +
  geom_ribbon(aes(ymin = lower_pm, ymax = upper_pm),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = pal, name = "Ätiologie") +
  scale_fill_manual(values = pal, name = "Ätiologie") +
  scale_x_continuous(breaks = seq(0, 60, 12), limits = c(0, 60)) +
  labs(x = "Monate seit OP",
       y = "Hazard-Rate (Rezidive pro 100 Patient*innen-Monat)",
       caption = paste0("Effektive Freiheitsgrade der Glättung: ", df_note)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 8, colour = "grey30"))

# ── (3) Manuskript-Abbildung: zwei Einzelbilder ──
cif_path <- file.path(fig_dir, "Abb_Diagnostik_CIF_by_Aetiologie.png")
ggsave(cif_path, p_ci, width = 9, height = 5.2, dpi = 220, bg = "white")
cat(sprintf("[03b_Hazard_Plots] wrote %s\n", cif_path))
hz_path <- file.path(fig_dir, "Abb_Diagnostik_Hazard_by_Aetiologie.png")
ggsave(hz_path, p_hz, width = 9, height = 5, dpi = 220, bg = "white")
cat(sprintf("[03b_Hazard_Plots] wrote %s\n", hz_path))

# ── (4) Glaettungs-Sensitivitaet (Gesamtkohorte, Konsole) ──
hz_all <- tryCatch(bshazard(Surv(time, event) ~ 1,
                            data = as.data.frame(analysis), verbose = FALSE),
                   error = function(e) NULL)
if (!is.null(hz_all)) {
  peak_at <- function(h) {
    i <- which.max(h$hazard[h$time <= 730])
    h$time[h$time <= 730][i]
  }
  lam_all <- hz_all$phi / hz_all$sv2
  cat(sprintf("[03b_Hazard_Plots] Gesamt: eff. df = %.1f, lambda = %.3g, Peak (0-24 Mo) bei %.0f d\n",
              hz_all$df, lam_all, peak_at(hz_all)))
  for (lam in c(1e3, 1e2, 10, 1)) {
    hz_s <- tryCatch(bshazard(Surv(time, event) ~ 1,
                              data = as.data.frame(analysis),
                              lambda = lam, verbose = FALSE),
                     error = function(e) NULL)
    if (!is.null(hz_s)) {
      cat(sprintf("[03b_Hazard_Plots]   Sensitivitaet lambda = %g: eff. df = %.1f, Peak bei %.0f d\n",
                  lam, hz_s$df, peak_at(hz_s)))
    } else {
      cat(sprintf("[03b_Hazard_Plots]   Sensitivitaet lambda = %g: Fit fehlgeschlagen!\n",
                  lam))
    }
  }
}

# ── (5) Peak-Position pro Ätiologie als Konsolen-Diagnose ──
peaks <- hz_df |>
  group_by(aetio) |>
  filter(time_mo <= 24) |>
  slice_max(hazard_pm, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(time_mo)
cat("[03b_Hazard_Plots] Hazard-Peak (0–24 Mo) pro Ätiologie:\n")
print(as.data.frame(peaks |> select(aetio, time_mo, hazard_pm)),
      row.names = FALSE, digits = 3)
