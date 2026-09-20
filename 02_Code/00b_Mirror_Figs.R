# 00b_Mirror_Figs.R -- spiegelt 03_Tables_Figures nach 05_Target_Structure/01_figs_tabs;
# das Zielverzeichnis wird vorher geleert.

mirror_figs <- function(fig_dir    = "./03_Tables_Figures",
                        mirror_dir = "./05_Target_Structure/01_figs_tabs",
                        quiet      = FALSE) {

  say <- function(...) if (!quiet) message("[mirror_figs] ", ...)

  # ── Schutzwaelle: erst pruefen, dann loeschen ──
  if (!dir.exists(fig_dir))
    stop("[mirror_figs] ", fig_dir, " nicht gefunden. Falsches Arbeitsverzeichnis?")

  src <- list.files(fig_dir, pattern = "\\.(png|tex)$", full.names = TRUE)
  src <- src[basename(src) != "table_view.tex"]

  if (length(src) == 0)
    stop("[mirror_figs] Keine PNG/TEX in ", fig_dir, ". Spiegel wird NICHT ",
         "geleert, sonst waere das Dokument anschliessend ohne Objekte.")

  if (!dir.exists(mirror_dir)) dir.create(mirror_dir, recursive = TRUE)

  # ── Clean Slate: nur Dateien, Unterverzeichnisse bleiben unberuehrt ──
  old <- list.files(mirror_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  old <- old[!dir.exists(old)]
  n_removed <- if (length(old)) sum(file.remove(old)) else 0L
  if (length(old) && n_removed < length(old))
    warning("[mirror_figs] ", length(old) - n_removed, " Datei(en) liessen sich ",
            "nicht loeschen. Ist main.pdf oder ein Editor offen?")

  # ── Kopieren ──
  ok <- file.copy(src, mirror_dir, overwrite = TRUE, copy.date = TRUE)
  say(sprintf("%d entfernt, %d von %d kopiert -> %s",
              n_removed, sum(ok), length(src), mirror_dir))
  if (any(!ok))
    warning("[mirror_figs] Nicht kopiert: ",
            paste(basename(src[!ok]), collapse = ", "))

  # ── Nachkontrolle: haben sich Drive-Konfliktkopien gebildet? ──
  after <- list.files(mirror_dir)
  konflikt <- grep("\\([0-9]+\\)\\.(png|tex)$", after, value = TRUE)
  if (length(konflikt))
    warning("[mirror_figs] ", length(konflikt), " Konfliktkopie(n) im Spiegel, ",
            "z.B. ", konflikt[1], ". Google Drive hat beim Schreiben nicht ",
            "ueberschrieben. Drive-Synchronisation pausieren und erneut spiegeln.")
  else
    say("Nachkontrolle: keine Konfliktkopien, ", length(after), " Datei(en) im Spiegel.")

  invisible(list(removed = n_removed, copied = sum(ok), conflicts = length(konflikt)))
}

if (!isTRUE(getOption("mirror_figs.define_only", FALSE))) mirror_figs()
