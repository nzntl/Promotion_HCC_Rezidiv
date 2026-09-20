# 14_Static_Reference_Tables.R -- Platzhalter fuer von Hand gepflegte Referenztabellen; schreibt derzeit kein Objekt.

fig_dir <- "./03_Tables_Figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

write_fragment <- function(text, path_name) {
  target <- file.path(fig_dir, path_name)
  con <- file(target, "w", encoding = "UTF-8")
  writeLines(text, con); close(con)
  cat(sprintf("[14_Static_Reference_Tables] wrote %s\n", path_name))
}


# ── Tab_Variable_Composition ──
var_comp <- r"---(% Zusammensetzung der wichtigsten abgeleiteten und zusammengesetzten Variablen (Quotienten, Scores, Slopes, Endpunkte). Regeneriert aus 14_Static_Reference_Tables.R.
\begin{tabular}{>{\raggedright\arraybackslash}p{3.3cm}>{\raggedright\arraybackslash}p{8.0cm}>{\raggedright\arraybackslash}p{2.3cm}}
\toprule
Variable & Zusammensetzung / Formel & Typ \\
\midrule
BMI & Körpergewicht / Körpergröße² & kg/m² \\
FLV/TLV-Ratio & FLV präop / TLV präop & Quotient \\
TTLVR (Lian) & Tumorvolumen / TLV präop & Quotient \\
LVR-Index & TLV (erste postop.) / FLV präop & Quotient \\
Δ TLV vs. FLV & (TLV 1.\,postop $-$ FLV präop) / FLV präop $\times$ 100 & \% \\
De-Ritis & AST (GOT) / ALT (GPT) & Quotient \\
AST/Albumin & AST (GOT) / Albumin & Quotient \\
APRI & (AST / oberer Normwert) / Thrombozyten $\times$ 100 & Index \\
ALBI-Grad & $0{,}66 \times \log_{10}$(Bilirubin) $- 0{,}085 \times$ Albumin; Einteilung in Grad 1--3 nach Cut-offs & Score zu Grad \\
Child-Pugh-Punkte & Summe aus Bilirubin, Albumin, Quick/INR, Aszites und Enzephalopathie & Score 5--15 \\
Labor-Slope POD 1--7 & OLS-Steigung des Markers (Quick, GOT, GPT, CRP, Albumin, Leukozyten, Thrombozyten) über POD 1--7 & Einheit/Tag \\
T-Stadium & AJCC/UICC-Tumorstadium & ordinal 1--4 \\
Major-Hepatektomie & Mindestens drei resezierte Couinaud-Segmente, über getrennte Teileingriffe hinweg gezählt & binär \\
Frührezidiv & Rezidiv innerhalb von 24 Monaten & binär \\
Ätiologie (4 Klassen) & Zusammenfassung der 6-Klassen-Ätiologie zu 4 Klassen (MASLD, viral = HBV oder HCV, ethyltoxisch, Rest = Andere oder Unbekannt). Referenzklasse Rest & kategorial \\
Diabetes mellitus & Diabetes mellitus dokumentiert (ja/nein) & binär \\
TTR & Zeit bis erstes Rezidiv, konkurrierende Todesfälle zensiert (vgl.\ § 2.1.3) & Endpunkt \\
OS & Zeit bis Tod jeglicher Ursache & Endpunkt \\
\bottomrule
\end{tabular})---"
if (FALSE) write_fragment(var_comp, "Tab_Variable_Composition.tex")
.alt_vc <- file.path("03_Tables_Figures", "Tab_Variable_Composition.tex")
if (file.exists(.alt_vc)) file.remove(.alt_vc)
rm(.alt_vc)
