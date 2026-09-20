# 01_Preparation_Selection.R -- liest die Rohdaten (Blatt "combined df"), bereinigt
# Freitext und Zahlen, leitet die Analysegroessen ab und wendet die Ausschlusskette an.
# Schreibt 01_Daten/data_static.xlsx, data_dynamic.xlsx und data_dynamic_surv.xlsx.


# ── 0. Packages ──

pkgs <- c("readxl", "writexl", "dplyr", "ggplot2", "wesanderson",
          "tidyr", "stringr", "purrr", "zoo")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}

library(readxl); library(writexl)
library(dplyr);  library(tidyr);  library(stringr); library(purrr)
library(ggplot2); library(wesanderson)
library(zoo)


# ── 1. Load data & replace string "NA" with true NA ──

raw <- read_excel("./01_Daten/20260527_hcc_combined_final.xlsx",
                  sheet = "combined df")

raw_n <- raw |>
  mutate(across(everything(), ~ if_else(.x == "NA", NA, .x)))


# ── 2. Reshape: Rezidiv_Therapie dates → new long-format rows ──

raw_n <- raw_n |>
  mutate(
    rezidiv_therapie_jahr = case_when(
      is.na(rezidiv_therapie_jahr)             ~ NA_character_,
      as.numeric(rezidiv_therapie_jahr) < 100  ~ as.character(
        as.numeric(rezidiv_therapie_jahr) + 2000
      ),
      TRUE                                     ~ rezidiv_therapie_jahr
    )
  )

therapie_rows <- raw_n |>
  filter(!is.na(rezidiv_therapie_monat)) |>
  mutate(
    obs_Jahr    = rezidiv_therapie_jahr,
    obs_Monat   = rezidiv_therapie_monat,
    obs_Tag     = rezidiv_therapie_tag,
    datasource  = "Rezidiv_Therapie"
  ) |>
  select(-rezidiv_therapie_tag, -rezidiv_therapie_monat, -rezidiv_therapie_jahr)

raw_n <- raw_n |>
  mutate(
    rezidiv_therapie_allgemein = NA_character_,
    rezidiv_therapie_konkret   = NA_character_
  ) |>
  select(-rezidiv_therapie_tag, -rezidiv_therapie_monat, -rezidiv_therapie_jahr) |>
  bind_rows(therapie_rows) |>
  mutate(across(c(obs_Jahr, obs_Monat, obs_Tag),
                ~ suppressWarnings(as.integer(.x)), .names = "sortkey_{.col}")) |>
  arrange(Pseudonym, sortkey_obs_Jahr, sortkey_obs_Monat, sortkey_obs_Tag) |>
  select(-starts_with("sortkey_")) |>
  mutate(across(matches("jahr|monat|tag", ignore.case = TRUE), as.numeric))


# ── 3. Reshape: FU dates → new long-format rows ──

fu_rows <- raw_n |>
  filter(datasource == "konstant", !is.na(FU_Jahr)) |>
  mutate(
    obs_Jahr   = FU_Jahr,
    obs_Monat  = FU_Monat,
    obs_Tag    = FU_Tag,
    datasource = "FU"
  )

raw_n <- raw_n |>
  mutate(FU_Jahr = NA_real_, FU_Monat = NA_real_, FU_Tag = NA_real_) |>
  bind_rows(fu_rows) |>
  mutate(across(c(obs_Jahr, obs_Monat, obs_Tag),
                ~ suppressWarnings(as.integer(.x)), .names = "sortkey_{.col}")) |>
  arrange(Pseudonym, sortkey_obs_Jahr, sortkey_obs_Monat, sortkey_obs_Tag) |>
  select(-starts_with("sortkey_"))


# ── 4. Nebendiagnosen (ND 1–21): binary dummies and category dummies ──

nd_cols <- paste("ND", 1:21)

# ── 4a. Harmonise free-text spellings to a canonical label ──
nd_clean <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all(c(
      "depresssion"                                                  = "depression",
      "thromboyztopenie|thrombozytopen(ie)?"                         = "thrombozytopenie",
      "enzephalopahie"                                               = "enzephalopathie",
      "cholzystolithiasis|cholezystolithiasis|cholecystolithiasis"   = "cholelithiasis",
      "chron hep b|hep b"                                            = "hepatitis_b",
      "chron hep c|hep c"                                            = "hepatitis_c",
      "hep a"                                                        = "hepatitis_a",
      "hep e"                                                        = "hepatitis_e",
      "mitralinsuff$"                                                = "mitralinsuffizienz",
      "aorteninsuff$"                                                = "aorteninsuffizienz",
      "trikuspidalinsuff$"                                           = "trikuspidalinsuffizienz",
      "psoriasis vulgaris"                                           = "psoriasis",
      "hydrop dekompensation|hydropische dekompensation"             = "hydropische_dekompensation",
      "sigmadivertikulitis|sigmadivertikulose|divertikulose"         = "divertikulose",
      "spelnomegalie"                                                = "splenomegalie",
      "monoklonale gammopathie"                                      = "monoklonale_gammopathie",
      "chron pankreatitis|pankreatitis chron"                        = "pankreatitis_chron",
      "lws syndrom|hws syndrom|bws syndrom|ws syndrom"               = "wirbelsaeulen_syndrom",
      "hernia.*"                                                     = "hernie",
      "stent.*"                                                      = "stent",
      "struma.*"                                                     = "struma",
      "ulcus ventriculi"                                             = "ulcus_ventriculi",
      "ulcus duodeni"                                                = "ulcus_duodeni",
      "portale hypertension"                                         = "portale_hypertension",
      "pulmonale hypertonie"                                         = "pulmonale_hypertonie",
      "^(none|na)$"                                                  = NA_character_
    ))
}

# ── 4b. Pivot ND columns long, clean labels, build per-diagnosis dummies ──
nd_long <- raw_n |>
  select(Pseudonym, all_of(nd_cols)) |>
  pivot_longer(cols = all_of(nd_cols), names_to = "nd_col", values_to = "diagnosis") |>
  separate_rows(diagnosis, sep = ",") |>
  mutate(diagnosis = nd_clean(diagnosis)) |>
  filter(!is.na(diagnosis), diagnosis != "") |>
  distinct(Pseudonym, diagnosis)

nd_long |> distinct(diagnosis) |> arrange(diagnosis) |> print(n = Inf)

nd_dummies <- nd_long |>
  mutate(
    value   = 1L,
    varname = paste0("D_ND_", str_replace_all(diagnosis, " ", "_"))
  ) |>
  select(Pseudonym, varname, value) |>
  pivot_wider(names_from = varname, values_from = value, values_fill = 0L)

raw_n <- raw_n |>
  left_join(nd_dummies, by = "Pseudonym")

# ── 4c. Miscellaneous ND scalars ──

raw_n <- raw_n |>
  mutate(
    ND_count = rowSums(
      !is.na(across(matches("^ND \\d+$"))) &
        across(matches("^ND \\d+$")) != "NA"
    )
  )

.nikotin_freitext <- nd_long |>
  filter(diagnosis == "nikotin") |>
  distinct(Pseudonym) |>
  pull(Pseudonym)

raw_n <- raw_n |>
  mutate(ND_Nikotin = if_else(
    (!is.na(ND_Nikotin) & ND_Nikotin != "NA") | Pseudonym %in% .nikotin_freitext,
    1L, 0L))

.nik_gesamt <- unique(raw_n$Pseudonym[raw_n$ND_Nikotin == 1L])
.nik_frei   <- unique(.nikotin_freitext)
cat(sprintf(paste0("Nikotin (Personen, Rohkohorte): %d nur eigene Spalte, ",
                   "%d auch im Freitext, %d gesamt
"),
  length(setdiff(.nik_gesamt, .nik_frei)),
  length(intersect(.nik_frei, .nik_gesamt)),
  length(.nik_gesamt)))

raw_n <- raw_n |>
  mutate(across(starts_with("D_ND_"), ~ replace_na(.x, 0L)))

raw_n |>
  summarise(across(starts_with("D_ND_"), mean, na.rm = TRUE)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "mean") |>
  arrange(desc(mean)) |>
  print(n = Inf)

# ── 4d. Broad ND category dummies → named D_ND_cat_* ──

cardiovascular <- c(
  "khk", "1g khk", "2g khk", "3g khk", "ap", "mi",
  "herzinsuffizienz", "kardiomyopathie", "nyha2 kardiomyopathie",
  "myokarditis", "vhf", "aa vhf", "intermittierende absolute arrythmie",
  "aortenstenose", "aorteninsuffizienz",
  "aortenklappensklerose", "aortensklerose", "aortenthrombus",
  "mitralinsuffizienz", "trikuspidalinsuffizienz", "pulmonalisstenose",
  "pulmonale_hypertonie",
  "arteriosklerose", "pavk", "carotisplaque", "carotisstenose pnp",
  "lsb", "pfo", "stent", "icd sm", "baa", "aneurysma iliaca",
  "milzarterienaneurysma", "lae", "tvt", "cvi",
  "varikosis", "acvb", "mikroangiopathie", "aht"
)
metabolic <- c(
  "hlp", "dyslipidämie", "hypercholesterinämie", "adipositas",
  "hyperurikämie", "porphyrie",
  "nash", "steatosis hepatis", "hämochromatose", "morbus wilson"
)
endokrin <- c(
  "hypothyreose", "hyperthyreose", "struma", "thyreoiditis",
  "hypogonadismus", "conn syndrom"
)
hepatic <- c(
  "autoimmunhepatitis", "psc", "cholangiopathie", "cholelithiasis",
  "chron cholecystitis", "budd-chiari-syndrom", "leberabszess",
  "leberzysten", "biliom", "schrumpfgallenblase", "hepatopathie",
  "ösophagusvarizen", "portale_hypertension", "hypersplenismus",
  "splenomegalie", "pfortaderthrombose", "hydropische_dekompensation",
  "enzephalopathie", "siderose"
)
renal <- c(
  "ckd", "nephropathie", "glomerulonephritis", "nephrolithiasis",
  "nierenzysten", "nephrozirrhose", "stauungsniere"
)
pulmonary <- c(
  "copd", "asthma", "bronchitis", "chron bronchitis", "emphysemthorax",
  "lungenemphysem", "alveolitis", "osas",
  "lungenödem", "mykobakteriose", "tbc", "pneumonie"
)
gastrointestinal <- c(
  "gastritis", "antrumgastritis", "erosive antrumgastritis",
  "gastropathie", "gerd", "barrett", "barret", "ulcus_ventriculi",
  "ulcus_duodeni", "duodenalstenose", "helicobacter", "hernie",
  "divertikulose", "kolitis",
  "colitis ulcerosa", "kolonpolyp", "pankreatitis",
  "pankreatitis_chron", "pankreaszyste", "hämorrhoiden",
  "ogi blutung", "mallory weiss blutung", "obstipation",
  "short segment barrett syndrom"
)
neuropsychiatric <- c(
  "apoplex", "tia", "ponsinfarkt", "sab", "icb", "enzephalitis",
  "epilepsie", "demenz", "migräne", "depression", "bipolar",
  "schizophrenie", "ptbs", "pnp", "parese arm", "restless legs",
  "sht", "carotisstenose pnp"
)
haematological <- c(
  "thrombozytopenie", "thrombopenie", "thrombophilie", "hämophilie",
  "faktor v", "faktor 8 mangel", "hit", "anämie", "leukozytose",
  "polyglobulie", "cll", "lymphom", "monoklonale_gammopathie",
  "myelodysplastisches syndrom", "evans-syndrom", "cvid",
  "kyroglobulinämie"
)
musculoskeletal <- c(
  "arthrose", "arthritis", "osteoporose", "osteopenie",
  "osteomyelitis", "skoliose", "spondylosis deformans",
  "spinalkanalstenose",
  "bwk fraktur", "lwk fraktur", "humerus fraktur", "humerusfraktur",
  "radiusfraktur", "felsenbeinfraktur", "mittelhandknochenfraktur",
  "rippenserienfraktur", "wirbelsaeulen_syndrom"
)
infectious <- c(
  "hiv", "ebv infektion", "malaria", "schistosomiasis", "polio",
  "herpes", "sepsis", "oropharyngeal pilzinfektion", "aspergillom",
  "tbc", "mykobakteriose",
  "hepatitis_a", "hepatitis_b", "hepatitis_c", "hepatitis_e"
)
surgical <- c(
  "ltx", "cce", "splenektomie", "nephrektomie", "hemikolektomie",
  "divertikulose", "sigma resektion", "kolon resektion", "magen resektion",
  "leber resektion", "lunge resektion", "mastektomie", "prostatektomie",
  "thyreoidektomie", "ovarektomie", "orchiektomie", "crossektomie",
  "nukleotomie lws", "kreuzbandplastik", "acvb", "carotis tea",
  "bluttransfusion", "radiusfraktur", "laparotomie"
)
substance <- c("alkohol", "drogen")


aetio_own_labels <- function(x) {
  s <- str_to_lower(str_trim(as.character(x)))
  if (is.na(s) || s == "" || s == "na")                 return(character(0))
  if (str_detect(s, "hep b|hbv"))                       return("hepatitis_b")
  if (str_detect(s, "hep c|hcv"))                       return("hepatitis_c")
  if (str_detect(s, "ethyl|alkohol|aethyl"))            return("alkohol")
  if (str_detect(s, "nash|masld|metabolisch|steatos|nutritiv toxisch"))
                                                        return(c("nash", "steatosis hepatis"))
  if (str_detect(s, "hämochromatose|haemochromatose"))  return("hämochromatose")
  if (str_detect(s, "wilson"))                          return("morbus wilson")
  if (str_detect(s, "porphyrie"))                       return("porphyrie")
  if (str_detect(s, "psc"))                             return("psc")
  if (str_detect(s, "autoimmunhepatitis"))              return("autoimmunhepatitis")
  if (str_detect(s, "hep a"))                           return("hepatitis_a")
  if (str_detect(s, "cholestatische hepatopathie"))     return("hepatopathie")
  character(0)
}

aetio_self <- raw_n |>
  group_by(Pseudonym) |>
  summarise(
    .aetio_raw = {
      v <- Äthiologie[!is.na(Äthiologie)]
      if (length(v) > 0) as.character(v)[1] else NA_character_
    },
    .groups = "drop"
  )

nd_cat_dummies <- raw_n |>
  distinct(Pseudonym) |>
  left_join(
    nd_long |>
      group_by(Pseudonym) |>
      summarise(nd_vals = list(diagnosis), .groups = "drop"),
    by = "Pseudonym"
  ) |>
  left_join(aetio_self, by = "Pseudonym") |>
  mutate(
    nd_vals = map2(nd_vals, .aetio_raw,
                   ~ setdiff(as.character(.x), aetio_own_labels(.y)))
  ) |>
  mutate(
    D_ND_cat_cardiovascular   = as.integer(map_lgl(nd_vals, ~ any(.x %in% cardiovascular))),
    D_ND_cat_metabolic        = as.integer(map_lgl(nd_vals, ~ any(.x %in% metabolic))),
    D_ND_cat_endokrin         = as.integer(map_lgl(nd_vals, ~ any(.x %in% endokrin))),
    D_ND_cat_hepatic          = as.integer(map_lgl(nd_vals, ~ any(.x %in% hepatic))),
    D_ND_cat_renal            = as.integer(map_lgl(nd_vals, ~ any(.x %in% renal))),
    D_ND_cat_pulmonary        = as.integer(map_lgl(nd_vals, ~ any(.x %in% pulmonary))),
    D_ND_cat_gastrointestinal = as.integer(map_lgl(nd_vals, ~ any(.x %in% gastrointestinal))),
    D_ND_cat_neuropsychiatric = as.integer(map_lgl(nd_vals, ~ any(.x %in% neuropsychiatric))),
    D_ND_cat_haematological   = as.integer(map_lgl(nd_vals, ~ any(.x %in% haematological))),
    D_ND_cat_musculoskeletal  = as.integer(map_lgl(nd_vals, ~ any(.x %in% musculoskeletal))),
    D_ND_cat_infectious       = as.integer(map_lgl(nd_vals, ~ any(.x %in% infectious))),
    D_ND_cat_surgical         = as.integer(map_lgl(nd_vals, ~ any(.x %in% surgical))),
    D_ND_cat_substance        = as.integer(map_lgl(nd_vals, ~ any(.x %in% substance)))
  ) |>
  select(Pseudonym, starts_with("D_ND_cat_"))

raw_n <- raw_n |>
  select(-any_of(grep("^D_NDcat_", names(raw_n), value = TRUE))) |>
  left_join(nd_cat_dummies, by = "Pseudonym")

.aetio_abzug <- aetio_self |>
  mutate(.n_abzug = map_int(.aetio_raw, ~ length(aetio_own_labels(.x))))
cat(sprintf(paste0("ND-Personenregel: bei %d von %d Personen der Rohkohorte ",
                   "wird mindestens ein Label als eigene Aetiologie abgezogen\n"),
            sum(.aetio_abzug$.n_abzug > 0), nrow(.aetio_abzug)))
rm(.aetio_abzug)

raw_n |>
  summarise(across(starts_with("D_ND_cat_"), mean, na.rm = TRUE)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "mean") |>
  arrange(desc(mean)) |>
  print(n = Inf)


# ── 4e. Symptom_initial → dummies + Symptom_cat ──

sym_cols <- paste0("Symptom_initial ", 1:4)

sym_clean <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  x[x %in% c("", "na", "none", "nan")] <- NA_character_
  x
}

sym_long <- raw_n |>
  select(Pseudonym, all_of(sym_cols)) |>
  pivot_longer(all_of(sym_cols), names_to = "sym_col", values_to = "sym") |>
  mutate(sym = sym_clean(sym)) |>
  filter(!is.na(sym)) |>
  distinct(Pseudonym, sym)

sym_long |> distinct(sym) |> arrange(sym) |> print(n = Inf)

sym_dummies <- sym_long |>
  mutate(
    value   = 1L,
    varname = paste0("D_Sym_", str_replace_all(sym, "[ ,/]+", "_"))
  ) |>
  select(Pseudonym, varname, value) |>
  pivot_wider(names_from = varname, values_from = value, values_fill = 0L)

raw_n <- raw_n |>
  left_join(sym_dummies, by = "Pseudonym") |>
  mutate(across(starts_with("D_Sym_"), ~ replace_na(.x, 0L)))

sym_class <- function(s) {
  if (is.na(s)) return(NA_character_)
  if (str_detect(s, "^zufall")) return("Asymptomatisch_Zufall")
  if (s %in% c("vorsorge", "ohne bsymp", "ohne b", "keine sypmtome", "histo",
               "bildgebung", "leber pe bei lap cce suspekt"))
    return("Asymptomatisch_Zufall")
  if (str_detect(s, "^verlauf") || str_detect(s, "^nachsorge") ||
      str_detect(s, "^labor") || str_detect(s, "^kontrolle") ||
      s %in% c("routine", "ltx listung", "diagnostik zirrhose",
               "bildgebung progress", "psc"))
    return("Asymptomatisch_Ueberwachung")
  "Symptomatisch"
}

sym_priority <- c("Symptomatisch" = 1,
                  "Asymptomatisch_Zufall" = 2,
                  "Asymptomatisch_Ueberwachung" = 3)

symptom_cat_pat <- sym_long |>
  mutate(cat = vapply(sym, sym_class, character(1))) |>
  filter(!is.na(cat)) |>
  mutate(prio = sym_priority[cat]) |>
  group_by(Pseudonym) |>
  summarise(Symptom_cat = cat[which.min(prio)], .groups = "drop")

raw_n <- raw_n |>
  left_join(symptom_cat_pat, by = "Pseudonym")

# ── 4e-bis. B-Symptomatik (rein deskriptiv) ──
bsym_pat <- sym_long |>
  mutate(
    b_ja   = str_detect(sym, "fieber|nachtschwei|gewichtsverlust|gewichtsabnahme"),
    b_nein = str_detect(sym, "ohne b")
  ) |>
  group_by(Pseudonym) |>
  summarise(
    B_Symptomatik = dplyr::case_when(
      any(b_ja)   ~ "ja",
      any(b_nein) ~ "nein",
      TRUE        ~ NA_character_
    ),
    .groups = "drop"
  )

raw_n <- raw_n |>
  left_join(bsym_pat, by = "Pseudonym")


# ── 4f. rezidiv_ort → per-event Intra/Extrahepatisch ──

ort_cols <- paste0("rezidiv_ort_", 1:5)

ort_clean <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  x[x %in% c("", "na", "none", "nan")] <- NA_character_
  x
}

ort_buckets <- list(
  Intrahepatisch    = c("intrahepatisch", "multifokal", "idem", "absetzungsrand",
                        "intraluminal pfortader"),
  Lymphknoten       = c("lk"),
  Pulmonal          = c("pulmonal"),
  Ossaer            = c("ossaer"),
  Cerebral          = c("cerebral"),
  Peritoneal_Andere = c("pc", "peripankreatisch", "abdominell",
                        "retroperitoneal", "mediastinal", "milz", "sakrum",
                        "bauchwand", "stichkanal", "nebenniere")
)

ort_to_bucket <- unlist(lapply(names(ort_buckets), function(b) {
  setNames(rep(b, length(ort_buckets[[b]])), ort_buckets[[b]])
}))

raw_n <- raw_n |>
  mutate(across(all_of(ort_cols), ort_clean))

bucket_priority <- c("Intrahepatisch", "Intra + extrahepatisch",
                     "Pulmonal", "Lymphknoten", "Ossaer",
                     "Cerebral", "Peritoneal_Andere")

raw_n <- raw_n |>
  rowwise() |>
  mutate(
    D_rezidiv_cat = {
      vals <- c_across(all_of(ort_cols))
      buckets <- unname(ort_to_bucket[vals])
      buckets <- buckets[!is.na(buckets)]
      if (length(buckets) == 0) NA_character_
      else if ("Intrahepatisch" %in% buckets &&
               any(buckets != "Intrahepatisch")) "Intra + extrahepatisch"
      else {
        idx <- match(bucket_priority, buckets)
        bucket_priority[which(!is.na(idx))[1]]
      }
    }
  ) |>
  ungroup() |>
  mutate(D_rezidiv_cat = factor(D_rezidiv_cat, levels = bucket_priority))


# ── 4g. rezidiv_therapie_cat ──

therapie_cat <- function(x) {
  s <- str_to_lower(str_trim(as.character(x)))
  s[s %in% c("", "na", "nan")] <- NA_character_
  case_when(
    is.na(s) ~ NA_character_,
    s %in% c("resektion intrahepatisch", "resektion exrahepatisch",
             "resektion extrahepatisch",
             "resektion intrahepatisch, kein tumor") ~ "Resektion",
    s %in% c("mwa", "rfa", "ire") ~ "Ablation",
    s %in% c("tace", "sirt", "tacp") ~ "Transarteriell",
    s %in% c("radiatio", "sbrt", "ionenstrahl", "stereotaxie") ~ "Bestrahlung",
    s %in% c("tki", "monoklonale ak", "ici", "zytostatika",
             "immuntherapie", "systemtherapie", "tcr-t") ~ "Systemtherapie",
    s == "nsar"    ~ "Sonstige",
    s == "ltx"     ~ "LTX",
    s == "studie"  ~ "Studie",
    s == "none"    ~ "Keine",
    s == "unknown" ~ "Unbekannt",
    TRUE           ~ "Unbekannt"
  )
}

raw_n <- raw_n |>
  mutate(rezidiv_therapie_cat = therapie_cat(rezidiv_therapie_allgemein))


# ── 4h. äthiologie_cat — 5-bucket categorical (Nachsorge-Analyse) ──

aetiologie_cat <- function(x) {
  s <- str_to_lower(str_trim(as.character(x)))
  out <- rep("Andere", length(s))
  out[is.na(s) | s == "" | s == "na"] <- "Unbekannt"
  out[!is.na(s) & str_detect(s, "nash|masld|metabolisch|nutritiv toxisch|steatos")] <- "MASLD"
  out[!is.na(s) & str_detect(s, "ethyl|alkohol|aethyl")] <- "Ethyltox"
  out[!is.na(s) & str_detect(s, "hep c|hcv")] <- "HCV"
  out[!is.na(s) & str_detect(s, "hep b|hbv")] <- "HBV"
  factor(out, levels = c("HBV","HCV","Ethyltox","MASLD","Andere","Unbekannt"))
}
raw_n <- raw_n |> mutate(äthiologie_cat = aetiologie_cat(Äthiologie))

raw_n <- raw_n |>
  mutate(
    aetio_model = factor(
      case_when(
        äthiologie_cat == "MASLD"      ~ "MASLD",
        äthiologie_cat %in% c("HBV", "HCV") ~ "viral",
        äthiologie_cat == "Ethyltox"        ~ "ethyltox",
        TRUE                                ~ "Rest"
      ),
      levels = c("Rest", "MASLD", "viral", "ethyltox")
    )
  )


# ── 4i. lesion_multi_bin — Tumoranzahl: multipel (vs. solitär) ──

lesion_multi_from_count <- function(x) {
  s <- str_to_lower(str_trim(as.character(x)))
  s[s %in% c("", "na", "nan")] <- NA_character_
  n <- suppressWarnings(as.numeric(s))
  case_when(
    is.na(s)                             ~ NA_integer_,
    s %in% c("multipel", "disseminiert") ~ 1L,
    !is.na(n) & n > 1                    ~ 1L,
    !is.na(n) & n %in% c(0, 1)           ~ 0L,
    TRUE                                 ~ NA_integer_
  )
}

raw_n <- raw_n |>
  mutate(lesion_multi_bin = lesion_multi_from_count(Läsionenzahl_post))

.lmb_pat <- raw_n |>
  filter(datasource == "konstant") |>
  distinct(Pseudonym, lesion_multi_bin)
cat("lesion_multi_bin (Patientenebene, Rohkohorte):\n")
print(table(.lmb_pat$lesion_multi_bin, useNA = "always"))


# ── 5. Span static/constant variables across all obs within a Pseudonym ──

span_vars <- raw_n |>
  select(
    starts_with("Geburt"),
    starts_with("OP"),
    starts_with("D_ND"),
    starts_with("D_Sym_"),
    starts_with("ND_Nikotin"),
    starts_with("ND_OP"),
    starts_with("ND_Onko"),
    starts_with("LTX"),
    starts_with("DM"),
    starts_with("Child"),
    starts_with("Symptom"),
    B_Symptomatik,
    Diagnose, Studienausschluss, Äthiologie, äthiologie_cat, aetio_model,
    Vorbehandlung, mwd, KGkg, m, BMI,
    Alter, Leberzirrhose, ClavienDindo,
    Rezidivtumor, T, N, M,
    Läsionenzahl_post, lesion_multi_bin,
    FLV, FUStatus
  ) |>
  names()

raw_n <- raw_n |>
  group_by(Pseudonym) |>
  mutate(across(all_of(span_vars), ~ first(na.omit(.)))) |>
  ungroup()


# ── 6. HU parsing, date construction ──

# ── 6a. Parse "mean ± SD" strings in HU_liver / HU_spleen ──
raw_n <- raw_n |>
  mutate(
    across(c(HU_liver, HU_spleen), ~ str_replace_all(.x, ",", ".")),
    HU_liver_sd  = as.numeric(str_extract(HU_liver,  "[0-9.]+$")),
    HU_spleen_sd = as.numeric(str_extract(HU_spleen, "[0-9.]+$")),
    HU_liver     = as.numeric(str_extract(HU_liver,  "^[^±+-]+")),
    HU_spleen    = as.numeric(str_extract(HU_spleen, "^[^±+-]+")),
    HU_liver_lb  = HU_liver  - HU_liver_sd,
    HU_liver_ub  = HU_liver  + HU_liver_sd,
    HU_spleen_lb = HU_spleen - HU_spleen_sd,
    HU_spleen_ub = HU_spleen + HU_spleen_sd,
    across(
      c(HU_liver, HU_spleen, HU_liver_lb, HU_liver_ub, HU_spleen_lb, HU_spleen_ub),
      ~ if_else(CT_MRT == "MR", NA_real_, .x)
    ),
    across(
      c(HU_liver, HU_liver_lb, HU_liver_ub, HU_spleen, HU_spleen_lb, HU_spleen_ub),
      ~ round(.x, 1)
    )
  ) |>
  select(-HU_liver_sd, -HU_spleen_sd) |>
  relocate(HU_liver_lb,  HU_liver_ub,  .after = HU_liver) |>
  relocate(HU_spleen_lb, HU_spleen_ub, .after = HU_spleen)

# ── 6b. Build Date columns from y/m/d components ──
raw_n <- raw_n |>
  mutate(
    date_Geburt = if_else(
      !is.na(Geburt_Jahr) & !is.na(Geburt_Monat) & !is.na(Geburt_Tag),
      as.Date(paste(Geburt_Jahr, Geburt_Monat, Geburt_Tag, sep = "-")),
      as.Date(NA)
    ),
    date_obs = if_else(
      !is.na(obs_Jahr) & !is.na(obs_Monat) & !is.na(obs_Tag),
      as.Date(paste(obs_Jahr, obs_Monat, obs_Tag, sep = "-")),
      as.Date(NA)
    ),
    date_OP = if_else(
      !is.na(OP_Jahr) & !is.na(OP_Monat) & !is.na(OP_Tag),
      as.Date(paste(OP_Jahr, OP_Monat, OP_Tag, sep = "-")),
      as.Date(NA)
    ),
    date_obs_month = if_else(
      !is.na(obs_Jahr) & !is.na(obs_Monat),
      paste(obs_Jahr, sprintf("%02d", as.integer(obs_Monat)), sep = "-"),
      NA_character_
    ),
    lab_date_POD = as.numeric(date_obs - date_OP)
  )

# ── 6b-neu. Unvollstaendige Beobachtungsdaten aufloesen ──
.vol_tumor <- raw_n |>
  mutate(.tv = suppressWarnings(as.numeric(str_replace_all(tumor_volume, ",", ".")))) |>
  filter(datasource == "volumetry", !is.na(date_obs), !is.na(date_obs_month),
         !is.na(.tv), .tv > 0) |>
  group_by(Pseudonym, date_obs_month) |>
  summarise(.date_bildgebung = min(date_obs), .groups = "drop")

raw_n <- raw_n |>
  left_join(.vol_tumor, by = c("Pseudonym", "date_obs_month")) |>
  mutate(
    .tag_fehlt = is.na(date_obs) & !is.na(obs_Jahr) & !is.na(obs_Monat),
    .quelle_datum = case_when(
      !.tag_fehlt              ~ "dokumentiert",
      !is.na(.date_bildgebung) ~ "Bildgebung",
      TRUE                     ~ "Monatsmitte"
    ),
    date_obs = case_when(
      !.tag_fehlt              ~ date_obs,
      !is.na(.date_bildgebung) ~ .date_bildgebung,
      TRUE ~ as.Date(paste(obs_Jahr, sprintf("%02d", as.integer(obs_Monat)),
                           "15", sep = "-"))
    ),
    lab_date_POD = as.numeric(date_obs - date_OP)
  )

cat("Rezidiv-Beobachtungsdaten (Rohkohorte), Herkunft des Tages:
")
print(as.data.frame(count(filter(raw_n, datasource == "rezidiv"), .quelle_datum)))

raw_n <- raw_n |>
  select(-.tag_fehlt, -.quelle_datum, -.date_bildgebung) |>
  group_by(Pseudonym) |>
  mutate(
    .pod_obs = if_else(datasource == "konstant", NA_real_, lab_date_POD),
    lab_date_POD_imp = if_else(
      datasource == "konstant",
      lab_date_POD,
      if (sum(!is.na(.pod_obs)) >= 2) na.approx(.pod_obs, na.rm = FALSE)
      else .pod_obs
    ),
    .pod_obs = NULL
  ) |>
  ungroup() |>
  select(-matches("(Geburt|obs|OP|FU)_(Jahr|Monat|Tag)"))


# ── 6c. FU_duration_days — static, patient-level ──

fu_dates <- raw_n |>
  filter(datasource == "FU", !is.na(date_obs)) |>
  group_by(Pseudonym) |>
  summarise(
    date_FU = max(date_obs, na.rm = TRUE),
    .groups = "drop"
  )

raw_n <- raw_n |>
  left_join(fu_dates, by = "Pseudonym") |>
  mutate(FU_duration_days = as.numeric(date_FU - date_OP))


# ── 6d. OP segment flags from OP_num (literature: 1, 4, 2/3 relevant) ──

op_seg_flag <- function(num, pattern) {
  s <- str_to_lower(str_trim(as.character(num)))
  s[is.na(s)] <- ""
  as.integer(str_detect(s, pattern))
}

raw_n <- raw_n |>
  mutate(
    D_OP_seg_1   = op_seg_flag(OP_num, "(^|[^0-9a-z])1([^0-9a-z]|$)"),
    D_OP_seg_4   = op_seg_flag(OP_num, "(^|[^0-9a-z])4(a|b)?([^0-9a-z]|$)"),
    D_OP_seg_2_3 = as.integer(
      op_seg_flag(OP_num, "(^|[^0-9a-z])2([^0-9a-z]|$)") +
        op_seg_flag(OP_num, "(^|[^0-9a-z])3([^0-9a-z]|$)") == 2
    )
  )


# ── 6e. Resezierte Segmente und Major-Hepatektomie aus OP_num ──
count_op_segments <- function(x) {
  s <- as.character(x)
  out <- vapply(strsplit(trimws(s), "[,;/ ]+"), function(tk) {
    seg <- tk[grepl("^[1-8](a|b)?$", tk)]
    length(unique(sub("[ab]$", "", seg)))
  }, integer(1))
  out[is.na(s) | trimws(s) == ""] <- NA_integer_
  out
}
raw_n <- raw_n |>
  mutate(n_op_segments = count_op_segments(OP_num),
         D_major       = as.integer(n_op_segments >= 3))


# ── 7. Dynamic variables ──

raw_n <- raw_n |>
  mutate(across(c(FLV, TLV, tumor_volume),
                ~ as.numeric(str_replace_all(.x, ",", "."))))

# ── 6c. Drop empty Volumetry-Placeholder-Rows ──
n_vol_before <- sum(raw_n$datasource == "volumetry", na.rm = TRUE)
raw_n <- raw_n |>
  filter(!(datasource == "volumetry" & is.na(TLV)))
n_vol_after  <- sum(raw_n$datasource == "volumetry", na.rm = TRUE)
cat(sprintf(
  "Volumetry-Placeholder-Drop: %d -> %d rows (-%d leere Placeholders)\n",
  n_vol_before, n_vol_after, n_vol_before - n_vol_after
))

# ── 7a. D_rezidiv_dyn + TLV deltas + d_TLV_rel_FLV_base ──

raw_n <- raw_n |>
  group_by(Pseudonym) |>
  arrange(lab_date_POD_imp, .by_group = TRUE) |>
  mutate(
    D_rezidiv_dyn = as.integer(cumsum(datasource == "rezidiv") > 0),

    .TLV_ref1          = first(TLV[datasource == "volumetry"]),
    .TLV_ref2          = nth(TLV[datasource == "volumetry"], 2),
    .TLV_ref3          = first(TLV[datasource == "volumetry" & !is.na(lab_date_POD_imp) & lab_date_POD_imp > 0]),
    .second_postop_idx = which(datasource == "volumetry" & !is.na(lab_date_POD_imp) & lab_date_POD_imp > 0)[2],
    .row_idx           = row_number(),
    .FLV_baseline      = first(na.omit(FLV)),

    d_TLV_abs      = if_else(datasource == "volumetry", TLV - .TLV_ref1, NA_real_),
    d_TLV_abs_ref2 = if_else(.row_idx == .second_postop_idx, TLV - .TLV_ref2, NA_real_),
    d_TLV_abs_ref3 = if_else(.row_idx == .second_postop_idx, TLV - .TLV_ref3, NA_real_),

    d_TLV_rel      = if_else(datasource == "volumetry", round((TLV - .TLV_ref1) / .TLV_ref1 * 100, 2), NA_real_),
    d_TLV_rel_ref2 = if_else(.row_idx == .second_postop_idx, round((TLV - .TLV_ref2) / .TLV_ref2 * 100, 2), NA_real_),
    d_TLV_rel_ref3 = if_else(.row_idx == .second_postop_idx, round((TLV - .TLV_ref3) / .TLV_ref3 * 100, 2), NA_real_),

    d_TLV_rel_FLV_base = if_else(
      datasource == "volumetry" &
        !is.na(lab_date_POD_imp) & lab_date_POD_imp > 0 &
        !is.na(.FLV_baseline) & .FLV_baseline > 0,
      round((TLV - .FLV_baseline) / .FLV_baseline * 100, 2),
      NA_real_
    ),
    d_TLV_implausibel = if_else(
      !is.na(d_TLV_rel_FLV_base) &
        (d_TLV_rel_FLV_base < -50 | d_TLV_rel_FLV_base > 250),
      1L, 0L
    ),

    lab_date_POD_imp_m = round(lab_date_POD_imp / 30.4375, 2),

    .TLV_ref1 = NULL, .TLV_ref2 = NULL, .TLV_ref3 = NULL,
    .second_postop_idx = NULL, .row_idx = NULL, .FLV_baseline = NULL
  ) |>
  ungroup()

# ── 7a-bis. Tumorfreies Parenchymvolumen ──
raw_n <- raw_n |>
  mutate(TLV_parenchym = if_else(datasource == "volumetry" &
                                   !is.na(TLV) & !is.na(tumor_volume),
                                 TLV - tumor_volume, NA_real_))

# ── 7b. Patient-level recurrence flag ──
raw_n <- raw_n |>
  group_by(Pseudonym) |>
  mutate(D_rezidiv = as.integer(any(D_rezidiv_dyn == 1))) |>
  ungroup()

# ── 7c. Reorder columns ──
core_vars <- c(
  "Pseudonym", "date_Geburt", "date_OP", "date_obs", "date_obs_month",
  "lab_date_POD", "lab_date_POD_imp", "datasource",
  "Rezidivtumor", "Rezidiv",
  "FLV", "TLV", "tumor_volume", "TLV_parenchym",
  "d_TLV_rel", "d_TLV_rel_ref2", "d_TLV_rel_ref3", "d_TLV_rel_FLV_base",
  "d_TLV_abs", "d_TLV_abs_ref2", "d_TLV_abs_ref3",
  "D_rezidiv_dyn",
  "D_rezidiv_cat", "rezidiv_therapie_cat"
)

raw_n <- raw_n |>
  relocate(any_of(core_vars)) |>
  select(
    -all_of(nd_cols),
    -matches("^ND \\d+|^ND_OP|^ND_Onko", ignore.case = FALSE)
  ) |>
  mutate(
    Rezidiv       = if_else(datasource == "konstant", NA, Rezidiv)
  )


# ── 7d. Labor slopes POD 1-7 (patient-level static) ──

lab_slope_vars <- c("Quick", "GOT", "GPT", "Albumin", "CRP", "Leukos", "Thrombos")
lab_slope_vars <- intersect(lab_slope_vars, names(raw_n))

lab_pod17 <- raw_n |>
  filter(datasource == "labor",
         !is.na(lab_date_POD_imp),
         lab_date_POD_imp >= 1, lab_date_POD_imp <= 7) |>
  select(Pseudonym, lab_date_POD_imp, all_of(lab_slope_vars)) |>
  mutate(across(all_of(lab_slope_vars),
                ~ suppressWarnings(as.numeric(.x))))

slope_simple <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 2) return(NA_real_)
  xv <- x[ok]; yv <- y[ok]
  vx <- var(xv)
  if (!is.finite(vx) || vx == 0) return(NA_real_)
  cov(xv, yv) / vx
}

slope_df <- lab_pod17 |>
  group_by(Pseudonym) |>
  summarise(across(all_of(lab_slope_vars),
                   ~ slope_simple(lab_date_POD_imp, .x),
                   .names = "slope_{.col}_POD1_7"),
            .groups = "drop")

raw_n <- raw_n |> left_join(slope_df, by = "Pseudonym")


# ── 7e. Präop Labor-Scalars + TTLVR + Laborquotienten + ALBI ──

ULN_AST_M_UI_PER_L <- 46
ULN_AST_W_UI_PER_L <- 37
BILI_MGDL_TO_UMOL  <- 17.1

preop_labor <- raw_n |>
  filter(datasource == "labor",
         is.na(lab_date_POD_imp) | lab_date_POD_imp <= 0) |>
  group_by(Pseudonym) |>
  arrange(lab_date_POD_imp, .by_group = TRUE) |>
  summarise(
    AST_preop      = suppressWarnings(first(na.omit(as.numeric(GOT)))),
    ALT_preop      = suppressWarnings(first(na.omit(as.numeric(GPT)))),
    Albumin_preop  = suppressWarnings(first(na.omit(as.numeric(Albumin)))),
    CRP_preop      = suppressWarnings(first(na.omit(as.numeric(CRP)))),
    Thrombos_preop = suppressWarnings(first(na.omit(as.numeric(Thrombos)))),
    Leukos_preop   = suppressWarnings(first(na.omit(as.numeric(Leukos)))),
    Quick_preop    = suppressWarnings(first(na.omit(as.numeric(Quick)))),
    Bili_preop_val = suppressWarnings(first(na.omit(as.numeric(Bili_preop)))),
    sex_mwd        = suppressWarnings(first(na.omit(as.character(mwd)))),
    .groups = "drop"
  ) |>
  mutate(
    uln_ast    = if_else(!is.na(sex_mwd) & str_to_lower(sex_mwd) == "w",
                         ULN_AST_W_UI_PER_L, ULN_AST_M_UI_PER_L),
    deritis_preop = if_else(!is.na(AST_preop) & !is.na(ALT_preop)
                            & ALT_preop > 0,
                            AST_preop / ALT_preop, NA_real_),
    astalb_preop  = if_else(!is.na(AST_preop) & !is.na(Albumin_preop)
                            & Albumin_preop > 0,
                            AST_preop / Albumin_preop, NA_real_),
    APRI_preop = if_else(!is.na(AST_preop) & !is.na(Thrombos_preop)
                         & Thrombos_preop > 0,
                         (AST_preop / uln_ast) /
                           Thrombos_preop * 100,
                         NA_real_),
    Bili_umol  = Bili_preop_val * BILI_MGDL_TO_UMOL,
    ALBI_score = if_else(!is.na(Bili_umol) & Bili_umol > 0
                         & !is.na(Albumin_preop),
                         log10(Bili_umol) * 0.66 +
                           Albumin_preop * (-0.085),
                         NA_real_),
    ALBI_grade = factor(
      case_when(
        is.na(ALBI_score)        ~ NA_character_,
        ALBI_score <= -2.60      ~ "Grade 1",
        ALBI_score <= -1.39      ~ "Grade 2",
        TRUE                     ~ "Grade 3"
      ),
      levels = c("Grade 1", "Grade 2", "Grade 3"), ordered = TRUE
    )
  )

.score_summary <- preop_labor |>
  summarise(
    n_AST   = sum(!is.na(AST_preop)),
    n_Alb   = sum(!is.na(Albumin_preop)),
    n_Th    = sum(!is.na(Thrombos_preop)),
    n_Bili  = sum(!is.na(Bili_preop_val)),
    n_deritis = sum(!is.na(deritis_preop)),
    n_astalb  = sum(!is.na(astalb_preop)),
    n_APRI  = sum(!is.na(APRI_preop)),
    n_ALBI  = sum(!is.na(ALBI_score)),
    deritis_med = median(deritis_preop, na.rm = TRUE),
    astalb_med  = median(astalb_preop,  na.rm = TRUE),
    APRI_med = median(APRI_preop, na.rm = TRUE),
    ALBI_med = median(ALBI_score, na.rm = TRUE)
  )
cat("Preop-Labor-Scores Sanity:\n"); print(.score_summary)
.albi_table <- table(preop_labor$ALBI_grade, useNA = "always")
cat("ALBI-Grad-Verteilung:\n"); print(.albi_table)

ttlvr_preop_df <- raw_n |>
  filter(datasource == "volumetry",
         (is.na(lab_date_POD_imp) | lab_date_POD_imp <= 0),
         !is.na(TLV), TLV > 0,
         !is.na(tumor_volume), tumor_volume > 0) |>
  group_by(Pseudonym) |>
  arrange(lab_date_POD_imp, .by_group = TRUE) |>
  summarise(
    TTLVR_preop = first(tumor_volume) / first(TLV),
    .groups = "drop"
  ) |>
  mutate(TTLVR_preop = if_else(TTLVR_preop > 1, NA_real_, TTLVR_preop))
cat(sprintf(
  "TTLVR_preop: %d Patienten mit valider Berechnung (tumor_volume > 0 & < TLV)\n",
  sum(!is.na(ttlvr_preop_df$TTLVR_preop))
))

raw_n <- raw_n |>
  left_join(preop_labor |>
              select(Pseudonym, AST_preop, ALT_preop, Albumin_preop, Thrombos_preop, Leukos_preop, CRP_preop,
                     Quick_preop, Bili_preop_val, deritis_preop, astalb_preop, APRI_preop,
                     ALBI_score, ALBI_grade),
            by = "Pseudonym") |>
  left_join(ttlvr_preop_df, by = "Pseudonym")


# ── 8a. Drop the per-diagnosis D_ND_* dummies; keep only D_ND_cat_* ──
ind_nd_cols <- setdiff(
  grep("^D_ND_",     names(raw_n), value = TRUE),
  grep("^D_ND_cat_", names(raw_n), value = TRUE)
)
raw_n <- raw_n |> select(-all_of(ind_nd_cols))

# ── 8b. Drop per-event raw columns now condensed into categoricals ──
raw_n <- raw_n |>
  select(-any_of(c(paste0("rezidiv_ort_", 1:5),
                   "rezidiv_therapie_allgemein",
                   "rezidiv_therapie_konkret")))

# ── 8c. Drop D_Sym_* dummies (Symptom_cat is the user-facing variable) ──
sym_dummy_cols <- grep("^D_Sym_", names(raw_n), value = TRUE)
if (length(sym_dummy_cols) > 0) raw_n <- raw_n |> select(-all_of(sym_dummy_cols))

# ── 8d. Reorder ──
raw_n <- raw_n |>
  relocate(any_of(c("date_FU", "FU_duration_days", "lab_date_POD_imp_m")),
           .before = "datasource")


# ── 9. Save full prepared dataset ──

raw_n <- raw_n |> arrange(Pseudonym, lab_date_POD_imp)
write_xlsx(raw_n, "./01_Daten/20260527_hcc_combined_final_clean.xlsx")


# ── DATA SELECTION ──


# ── 10a. Eligibility filter ──

studien_excl_labels <- c("no HCC", "no OP", "no preop img", "no postop img")

studien_excl_labels_de <- c(
  "no HCC"        = "keine HCC-Histologie",
  "no OP"         = "keine Resektion erfolgt",
  "no preop img"  = "keine präoperative Bildgebung",
  "no postop img" = "keine postoperative Bildgebung"
)

build_exclusion_table <- function(raw_df, studien_labels) {
  remaining <- unique(raw_df$Pseudonym)
  rows <- list()
  rows[[1]] <- data.frame(
    Schritt = 0L,
    Begruendung = "Initiale Kohorte",
    Ausgeschlossen = NA_integer_,
    Verbleibend = length(remaining)
  )
  for (lbl in studien_labels) {
    drop <- raw_df |>
      filter(Pseudonym %in% remaining, !is.na(Studienausschluss),
             Studienausschluss == lbl) |>
      distinct(Pseudonym) |> pull(Pseudonym)
    remaining <- setdiff(remaining, drop)
    rows[[length(rows) + 1]] <- data.frame(
      Schritt = as.integer(length(rows)),
      Begruendung = studien_excl_labels_de[[lbl]],
      Ausgeschlossen = length(drop),
      Verbleibend = length(remaining)
    )
  }
  drop_rt <- raw_df |>
    filter(Pseudonym %in% remaining,
           !is.na(Rezidivtumor), str_to_lower(Rezidivtumor) == "ja") |>
    distinct(Pseudonym) |> pull(Pseudonym)
  remaining <- setdiff(remaining, drop_rt)
  rows[[length(rows) + 1]] <- data.frame(
    Schritt = as.integer(length(rows)),
    Begruendung = "Resektion eines Rezidivtumors",
    Ausgeschlossen = length(drop_rt),
    Verbleibend = length(remaining)
  )
  list(steps = bind_rows(rows), analysis = remaining)
}

excl <- build_exclusion_table(raw_n, studien_excl_labels)

df_eligible <- raw_n |>
  filter(is.na(Studienausschluss) |
           !Studienausschluss %in% studien_excl_labels)

write_excl_tex <- function(steps_df, path) {
  esc <- function(x) {
    x <- as.character(x); x[is.na(x)] <- "--"
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("([&%$#_{}])", "\\\\\\1", x); x
  }
  out <- c(
    "% Stufenweise Selektion: initiale Kohorte → Analyse-Kohorte",
    "\\begin{tabular}{llrr}",
    "\\toprule",
    "Schritt & Begründung & Ausgeschlossen & Verbleibend \\\\",
    "\\midrule",
    apply(steps_df, 1, function(r)
      paste(esc(r["Schritt"]), esc(r["Begruendung"]),
            esc(r["Ausgeschlossen"]), esc(r["Verbleibend"]),
            sep = " & ")) |>
      paste0(" \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )
  con <- file(path, "w", encoding = "UTF-8"); writeLines(out, con); close(con)
}
write_excl_tex(excl$steps, "./03_Tables_Figures/Tab_Exclusion_Steps.tex")

cat("Eligible (Studienausschluss-Filter):",
    n_distinct(df_eligible$Pseudonym), "patients\n")
cat("Analysis cohort (zusätzlich Rezidivtumor=ja ausgeschlossen):",
    length(excl$analysis), "patients\n")


# ── 10b. De-dynamised per-patient summaries (feed into data_static) ──

rezidiv_summary <- df_eligible |>
  filter(datasource == "rezidiv") |>
  group_by(Pseudonym) |>
  arrange(lab_date_POD_imp, .by_group = TRUE) |>
  summarise(
    n_rezidiv           = n(),
    D_rezidiv_cat_first = first(as.character(D_rezidiv_cat)),
    .groups = "drop"
  )

rez_therapie_indexed <- df_eligible |>
  filter(datasource %in% c("rezidiv", "Rezidiv_Therapie")) |>
  group_by(Pseudonym) |>
  arrange(lab_date_POD_imp, desc(datasource == "rezidiv"), .by_group = TRUE) |>
  mutate(
    .rez_key = if_else(datasource == "rezidiv",
                       as.character(lab_date_POD_imp), NA_character_),
    rez_idx  = pmax(cumsum(datasource == "rezidiv" &
                             !duplicated(.rez_key, incomparables = NA)), 1L)
  ) |>
  select(-.rez_key) |>
  ungroup() |>
  filter(datasource == "Rezidiv_Therapie")

therapie_summary <- rez_therapie_indexed |>
  group_by(Pseudonym) |>
  summarise(
    rezidiv_therapie_cat_first = dplyr::first(
      rezidiv_therapie_cat[rez_idx == 1 &
        !rezidiv_therapie_cat %in% c("Keine", "Unbekannt")],
      default = NA_character_),
    n_rezidiv_therapie         = n(),
    .groups = "drop"
  )

tlv_summary <- df_eligible |>
  filter(datasource == "volumetry",
         !is.na(lab_date_POD_imp), lab_date_POD_imp > 0,
         !is.na(TLV)) |>
  group_by(Pseudonym) |>
  arrange(lab_date_POD_imp, .by_group = TRUE) |>
  summarise(
    TLV_first_postop                = first(TLV),
    POD_first_postop_volumetry      = first(lab_date_POD_imp),
    d_TLV_rel_first_postop          = first(d_TLV_rel),
    d_TLV_rel_FLV_base_first_postop = first(d_TLV_rel_FLV_base),
    n_postop_volumetrien            = n(),
    .groups = "drop"
  )

tlv_preop <- df_eligible |>
  filter(datasource == "volumetry",
         (is.na(lab_date_POD_imp) | lab_date_POD_imp <= 0),
         !is.na(TLV)) |>
  group_by(Pseudonym) |>
  arrange(lab_date_POD_imp, .by_group = TRUE) |>
  summarise(TLV_preop = first(TLV), .groups = "drop")

tlv_nam <- df_eligible |>
  filter(datasource == "volumetry",
         !is.na(lab_date_POD_imp),
         lab_date_POD_imp >= 91, lab_date_POD_imp <= 180,
         !is.na(TLV)) |>
  group_by(Pseudonym) |>
  arrange(lab_date_POD_imp, .by_group = TRUE) |>
  summarise(TLV_nam_91_180 = first(TLV),
            POD_nam_91_180 = first(lab_date_POD_imp),
            .groups = "drop")

# ── Ereignisdatierung (vorgezogen; §10e nutzt dieselben Objekte) ──
TUMOR_D_MIN_CM    <- 1.0
TUMOR_VOL_MIN_CM3 <- (4/3) * pi * (TUMOR_D_MIN_CM / 2)^3

rez_dok <- df_eligible |>
  filter(tolower(datasource) == "rezidiv",
         !is.na(lab_date_POD_imp), lab_date_POD_imp > 0) |>
  group_by(Pseudonym) |>
  summarise(pod_dok  = min(lab_date_POD_imp),
            date_dok = date_obs[which.min(lab_date_POD_imp)],
            .groups = "drop")

rez_img <- df_eligible |>
  filter(tolower(datasource) == "volumetry",
         !is.na(lab_date_POD_imp), lab_date_POD_imp > 0,
         !is.na(tumor_volume), tumor_volume >= TUMOR_VOL_MIN_CM3) |>
  group_by(Pseudonym) |>
  summarise(pod_img  = min(lab_date_POD_imp),
            date_img = date_obs[which.min(lab_date_POD_imp)],
            .groups = "drop")

event_time_lookup <- full_join(rez_dok, rez_img, by = "Pseudonym") |>
  mutate(ev_time = pmin(pod_dok, pod_img, na.rm = TRUE),
         ev_date = if_else(!is.na(pod_dok) &
                             (is.na(pod_img) | pod_dok <= pod_img),
                           date_dok, date_img))

slope_log_tlv <- function(pod, tlv) {
  ok <- !is.na(pod) & !is.na(tlv) & tlv > 0
  if (sum(ok) < 2) return(NA_real_)
  x <- pod[ok]; y <- log(tlv[ok])
  vx <- var(x)
  if (!is.finite(vx) || vx == 0) return(NA_real_)
  cov(x, y) / vx
}
tlv_growth <- df_eligible |>
  filter(datasource == "volumetry",
         !is.na(lab_date_POD_imp), lab_date_POD_imp > 0,
         lab_date_POD_imp <= 180, !is.na(TLV_parenchym)) |>
  left_join(event_time_lookup |> select(Pseudonym, ev_time),
            by = "Pseudonym") |>
  filter(is.na(ev_time) | lab_date_POD_imp <= ev_time) |>
  group_by(Pseudonym) |>
  summarise(TLV_growth_rate = slope_log_tlv(lab_date_POD_imp, TLV_parenchym),
            n_tlv_growth_pts = sum(TLV_parenchym > 0, na.rm = TRUE),
            .groups = "drop")

local({
  vol <- df_eligible |>
    filter(datasource == "volumetry", !is.na(lab_date_POD_imp),
           lab_date_POD_imp > 0, !is.na(TLV_parenchym))
  n_e <- n_distinct(df_eligible$Pseudonym)
  n_ge2 <- function(d) d |> count(Pseudonym) |> filter(n >= 2) |> nrow()
  s1 <- n_ge2(vol)
  s2 <- n_ge2(vol |> filter(lab_date_POD_imp <= 180))
  s3 <- n_ge2(vol |> filter(lab_date_POD_imp <= 180) |>
                left_join(event_time_lookup |> select(Pseudonym, ev_time),
                          by = "Pseudonym") |>
                filter(is.na(ev_time) | lab_date_POD_imp <= ev_time))
  cat(sprintf(paste0("[growthrate] >=2 postop Volumetrien: %d | davon beide ",
                     "<= POD 180: %d | davon beide <= Ereigniszeit: %d ",
                     "(fehlend %d von %d = %.1f %%)\n"),
              s1, s2, s3, n_e - s3, n_e, 100 * (n_e - s3) / n_e))
})


# ── 10c. Static subset: one row per patient (rich, de-dynamised) ──

static_vars <- c(
  "Pseudonym", "date_Geburt", "date_OP", "date_FU", "FU_duration_days",
  "Alter", "m", "mwd", "KGkg", "BMI",
  "Diagnose", "Studienausschluss", "Äthiologie", "äthiologie_cat", "aetio_model",
  "Vorbehandlung",
  "Leberzirrhose", "ChildPugh_Punkte", "ChildPugh_Stadium",
  "ClavienDindo", "T", "N", "M", "Tumordurchmesser",
  "lesion_multi_bin",
  "Rezidivtumor", "FUStatus",
  "ND_Nikotin", "ND_Nikotin_py", "ND_count",
  "DM",
  "OP_Name", "OP_num", "OP_laparoskop", "OP_CCE",
  "D_OP_seg_1", "D_OP_seg_4", "D_OP_seg_2_3",
  "D_major", "n_op_segments",
  "Symptom_cat", "B_Symptomatik",
  "FLV",
  "slope_Quick_POD1_7", "slope_GOT_POD1_7", "slope_GPT_POD1_7",
  "slope_Albumin_POD1_7", "slope_CRP_POD1_7", "slope_Leukos_POD1_7",
  "slope_Thrombos_POD1_7",
  "AST_preop", "ALT_preop", "deritis_preop", "astalb_preop",
  "Albumin_preop", "Thrombos_preop", "Leukos_preop", "CRP_preop", "Quick_preop",
  "Bili_preop_val",
  "APRI_preop", "ALBI_score", "ALBI_grade",
  "TTLVR_preop"
)

static_vars <- intersect(static_vars, names(df_eligible))

static <- df_eligible |>
  group_by(Pseudonym) |>
  slice(1) |>
  ungroup() |>
  select(all_of(static_vars), starts_with("D_ND_cat_"), starts_with("D_Sym_"))

d_rez <- df_eligible |>
  group_by(Pseudonym) |>
  summarise(D_rezidiv = as.integer(any(D_rezidiv_dyn == 1, na.rm = TRUE)),
            .groups = "drop")

static <- static |>
  left_join(d_rez,             by = "Pseudonym") |>
  left_join(rezidiv_summary,   by = "Pseudonym") |>
  left_join(event_time_lookup |>
              transmute(Pseudonym,
                        time_to_first_rezidiv_days = ev_time,
                        date_rezidiv_first         = ev_date),
            by = "Pseudonym") |>
  left_join(therapie_summary,  by = "Pseudonym") |>
  left_join(tlv_summary,       by = "Pseudonym") |>
  left_join(tlv_preop,         by = "Pseudonym") |>
  left_join(tlv_nam,           by = "Pseudonym") |>
  left_join(tlv_growth,        by = "Pseudonym") |>
  mutate(
    LVR_nam = if_else(!is.na(FLV) & FLV > 0 & !is.na(TLV_nam_91_180),
                      TLV_nam_91_180 / FLV, NA_real_)
  ) |>
  mutate(
    OP_laparoskop      = if_else(is.na(OP_laparoskop), "offen", "laparoskopisch"),
    OP_CCE             = if_else(is.na(OP_CCE), "nein", "ja"),
    Vorbehandlung      = if_else(is.na(Vorbehandlung), "keine", Vorbehandlung),
    n_rezidiv          = replace_na(n_rezidiv, 0L),
    n_rezidiv_therapie = replace_na(n_rezidiv_therapie, 0L),
    ChildPugh_Punkte = suppressWarnings(as.numeric(ChildPugh_Punkte)),
    .cp_lab_normal = (is.na(Bili_preop_val) | is.na(Albumin_preop) |
                        is.na(Quick_preop)) |
      (Bili_preop_val < 2 & Albumin_preop > 35 & Quick_preop > 70),
    ChildPugh_Punkte = dplyr::case_when(
      is.na(ChildPugh_Punkte)                                   ~ NA_real_,
      ChildPugh_Punkte == 0 &
        (is.na(Leberzirrhose) | tolower(Leberzirrhose) != "ja") &
        .cp_lab_normal                                          ~ 5,
      ChildPugh_Punkte >= 5 & ChildPugh_Punkte <= 15            ~ ChildPugh_Punkte,
      TRUE                                                       ~ NA_real_
    ),
    .cp_lab_normal = NULL
  )

write_xlsx(static, "./01_Daten/data_static.xlsx")


# ── 10d. Dynamic subset: long format, lean static set ──

dynamic_static_carry <- c(
  "Alter", "m", "D_rezidiv", "OP_Name", "OP_num",
  "D_OP_seg_1", "D_OP_seg_4", "D_OP_seg_2_3",
  "Studienausschluss", "ClavienDindo"
)
dynamic_lab_carry <- c(
  "GOT", "GPT", "Albumin", "CRP", "Bili_preop",
  "Quick", "Leukos", "Thrombos", "Magnesium", "nü_Gluc"
)
dynamic_static_carry <- intersect(dynamic_static_carry, names(df_eligible))

dynamic_vars <- unique(c(
  core_vars,
  "lab_date_POD_imp_m",
  dynamic_static_carry,
  dynamic_lab_carry
))
dynamic_vars <- intersect(dynamic_vars, names(df_eligible))

dynamic <- df_eligible |> select(all_of(dynamic_vars))

dynamic <- dynamic |>
  left_join(d_rez, by = "Pseudonym", suffix = c("", ".y")) |>
  mutate(D_rezidiv = coalesce(D_rezidiv, D_rezidiv.y)) |>
  select(-any_of("D_rezidiv.y"))

write_xlsx(dynamic, "./01_Daten/data_dynamic.xlsx")


# ── 10e. Survival dataset ──

event_df <- event_time_lookup |>
  mutate(time = ev_time, event = 1L)

# ── Diagnose der Regel ──
.shift <- event_df |>
  filter(!is.na(pod_img), is.na(pod_dok) | pod_img < pod_dok) |>
  mutate(vorsprung = pod_dok - pod_img)
.neu <- .shift |> filter(is.na(pod_dok))
cat("\n── Rezidivdatum: fruehere Quelle (Doku vs. Bildgebung) ──\n")
cat(sprintf("  Schwelle: tumor_volume >= %.4f cm3 (= %.1f cm Durchmesser)\n",
            TUMOR_VOL_MIN_CM3, TUMOR_D_MIN_CM))
cat(sprintf("  Ereignisse gesamt: %d\n", nrow(event_df)))
cat(sprintf("  davon Datum aus der Bildgebung (frueher als Doku): %d (Median %.0f Tage, max %.0f Tage)\n",
            sum(!is.na(.shift$vorsprung)),
            suppressWarnings(median(.shift$vorsprung, na.rm = TRUE)),
            suppressWarnings(max(.shift$vorsprung, na.rm = TRUE))))
cat(sprintf("  davon nur durch Bildgebung belegt (keine rezidiv-Zeile): %d%s\n",
            nrow(.neu),
            if (nrow(.neu) > 0)
              paste0(" — ", paste(sprintf("%s (POD %.0f)", .neu$Pseudonym, .neu$pod_img),
                                  collapse = ", ")) else ""))

.klein <- df_eligible |>
  filter(tolower(datasource) == "volumetry",
         !is.na(lab_date_POD_imp), lab_date_POD_imp > 0,
         !is.na(tumor_volume), tumor_volume > 0,
         tumor_volume < TUMOR_VOL_MIN_CM3) |>
  group_by(Pseudonym) |>
  summarise(pod_klein = min(lab_date_POD_imp),
            tv_klein  = tumor_volume[which.min(lab_date_POD_imp)], .groups = "drop") |>
  left_join(event_df |> select(Pseudonym, time), by = "Pseudonym") |>
  filter(!is.na(time), pod_klein < time)
if (nrow(.klein) > 0) {
  cat(sprintf("  unter der Schwelle ignoriert: %d Messung(en), die das Datum um bis zu %.0f Tage vorverlegt haetten\n",
              nrow(.klein), max(.klein$time - .klein$pod_klein)))
  for (i in seq_len(nrow(.klein))) {
    cat(sprintf("    %s: POD %.0f, %.2f cm3 (%.1f mm) — %.0f Tage vor dem geltenden Datum\n",
                .klein$Pseudonym[i], .klein$pod_klein[i], .klein$tv_klein[i],
                10 * 2 * (3 * .klein$tv_klein[i] / (4 * pi))^(1/3),
                .klein$time[i] - .klein$pod_klein[i]))
  }
}
rm(.shift, .neu, .klein)

event_df <- event_df |> select(Pseudonym, time, event)

censored_df <- df_eligible |>
  filter(!Pseudonym %in% event_df$Pseudonym,
         !is.na(lab_date_POD_imp), lab_date_POD_imp > 0) |>
  group_by(Pseudonym) |>
  summarise(time = max(lab_date_POD_imp), .groups = "drop") |>
  mutate(event = 0L)

surv_df <- bind_rows(event_df, censored_df) |>
  arrange(Pseudonym) |>
  mutate(time_years = time / 365.25)

# ── Konkurrenzrisiko-Status + 90-Tage-Mortalitaets-Flag (zentral) ──
death_codes_s1 <- c("DOD", "DOC", "COD UNKNOWN", "COD")
fu_lookup <- static |>
  transmute(Pseudonym,
            .fus = toupper(trimws(as.character(FUStatus))),
            .fud = suppressWarnings(as.numeric(FU_duration_days)))
surv_df <- surv_df |>
  left_join(fu_lookup, by = "Pseudonym") |>
  mutate(
    cr_status = case_when(
      event == 1                        ~ 1L,
      .fus %in% death_codes_s1          ~ 2L,
      TRUE                              ~ 0L
    ),
    death90 = as.integer(cr_status == 2L &
                           coalesce(.fud, time) <= 90)
  ) |>
  select(-.fus, -.fud)

cat("\n── Survival dataset (eligible cohort) ──\n")
cat("  Total patients:", nrow(surv_df), "\n")
cat("  Recurrences:   ", sum(surv_df$event), "\n")
cat("  Censored:      ", sum(surv_df$event == 0), "\n")
cat("  Competing deaths (cr_status=2):", sum(surv_df$cr_status == 2), "\n")
cat("  ... davon <= 90 d (death90):   ", sum(surv_df$death90), "\n")

n_excluded <- n_distinct(df_eligible$Pseudonym) - nrow(surv_df)
if (n_excluded > 0) cat("  No post-OP data:", n_excluded, "(excluded)\n")

undated_rezidiv <- df_eligible |>
  filter(tolower(datasource) == "rezidiv") |>
  group_by(Pseudonym) |>
  summarise(has_pod = any(!is.na(lab_date_POD_imp) & lab_date_POD_imp > 0),
            .groups = "drop") |>
  filter(!has_pod) |>
  pull(Pseudonym)
if (length(undated_rezidiv) > 0) {
  .as_event <- intersect(undated_rezidiv, event_df$Pseudonym)
  .as_cens  <- setdiff(undated_rezidiv, event_df$Pseudonym)
  warning(sprintf(
    paste0("%d Patient(en) mit Rezidiv-Zeilen, aber ohne gültigen positiven ",
           "POD — davon über den Bildgebungsbeleg als Ereignis: %s; ",
           "ohne jeden datierbaren Beleg (zensiert): %s"),
    length(undated_rezidiv),
    if (length(.as_event)) paste(sort(.as_event), collapse = ", ") else "keine",
    if (length(.as_cens))  paste(sort(.as_cens),  collapse = ", ") else "keine"
  ), call. = FALSE)
  rm(.as_event, .as_cens)
}

write_xlsx(surv_df, "./01_Daten/data_dynamic_surv.xlsx")
