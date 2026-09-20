# 00_Install_Packages.R -- installiert die von der Pipeline benoetigten R-Pakete.
# Einmalig vor dem ersten Lauf ausfuehren.

pakete <- c(
  "readxl", "writexl", "dplyr", "tidyr", "tibble", "purrr",
  "stringr", "lubridate", "zoo",
  "survival", "survminer", "glmnet", "broom", "nlme",
  "JMbayes2", "coda", "bshazard",
  "ggplot2", "gridExtra", "scales", "viridis", "ggcorrplot",
  "ggalluvial", "wesanderson"
)


fehlend <- pakete[!vapply(pakete, requireNamespace, logical(1), quietly = TRUE)]

if (length(fehlend) == 0) {
  cat("Alle", length(pakete), "benoetigten Pakete sind bereits installiert.\n")
} else {
  cat("Fehlend:", paste(fehlend, collapse = ", "), "\n")
  cat("Installiere", length(fehlend), "Paket(e) ...\n\n")
  install.packages(fehlend, repos = "https://cloud.r-project.org")

  nachher <- fehlend[!vapply(fehlend, requireNamespace, logical(1), quietly = TRUE)]
  if (length(nachher) > 0) {
    cat("\nACHTUNG: Diese Pakete konnten nicht installiert werden:\n  ",
        paste(nachher, collapse = ", "), "\n")
    cat("Bitte manuell nachinstallieren. JMbayes2 benoetigt unter Umstaenden\n",
        "zusaetzliche Systembibliotheken beziehungsweise Rtools unter Windows.\n")
  } else {
    cat("\nAlle Pakete erfolgreich installiert.\n")
  }
}

cat("\nR-Version:", R.version.string, "\n")
