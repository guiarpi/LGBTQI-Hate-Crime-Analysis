# =============================================================================
# LGBTQI+ Hate Crime Analysis — Data Pipeline
# =============================================================================
# Author : Guilherme Arpi
# Dataset: FBI UCR Hate Crime Statistics (1991-2024)
#          US Census Gini Index by state (2010-2019)
#          Gallup LGBT Population Estimates by state
#
# Outputs (written to /data/):
#   lgbtq_hate_crimes_full.csv     — 1991-2024, all LGBTQI+ incidents
#   lgbtq_hate_crimes_enriched.csv — 2010-2024, incidents joined with Gini + population (ML-ready)
#   state_year_aggregate.csv       — state × year summary with two-track rates (Tableau-ready)
#
# NOTE — Keeping the dataset current:
#   Download the latest hate_crime.csv from the FBI Crime Data Explorer:
#   https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads
#   Replace data/hate_crime.csv and re-run. The pipeline handles any year range.
#
# NOTE — 2026 SRS Master File (partial year / partial coverage):
#   The FBI distributes a fixed-width SRS Master File for agencies still on the
#   legacy Summary Reporting System.  As of 2024 most agencies report via NIBRS,
#   so this file captures only a fraction of national incidents (~28 LGBTQI+
#   incidents for Jan–May 2026 vs. ~500+ expected from the full annual CSV).
#   A parser for this format is included at the bottom of this script — see
#   Section 7: parse_srs_master_file().  Set INCLUDE_2026_SRS <- TRUE below to
#   append those records to lgbtq_hate_crimes_full.csv.
#
#   Set to FALSE (default) unless you want the partial 2026 data included.
#   File expected at: data/2026_HC_NATIONAL_MASTER_FILE.txt
# =============================================================================

library(tidyverse)
library(lubridate)

# ── Paths ──────────────────────────────────────────────────────────────────────
# BASE_DIR   <- dirname(rstudioapi::getActiveDocumentContext()$path)
BASE_DIR <- if (basename(getwd()) == "R") dirname(getwd()) else getwd()  # works from repo root or from R/

INCLUDE_2026_SRS <- FALSE
SRS_FILE <- file.path(BASE_DIR, "data", "2026_HC_NATIONAL_MASTER_FILE.txt")

RAW_FBI       <- file.path(BASE_DIR, "data", "hate_crime.csv")
RAW_GINI      <- file.path(BASE_DIR, "data", "gini_index_1991_2024.csv")       # built by build_gini_dataset.py
RAW_POP_TOT   <- file.path(BASE_DIR, "data", "total_population_1991_2024.csv") # built by build_population_datasets.py
RAW_POP_LGBT  <- file.path(BASE_DIR, "data", "lgbt_population_2012_2024.csv")  # built by build_population_datasets.py
RAW_LAW_PANEL <- file.path(BASE_DIR, "data", "state_law_panel.csv")            # built by build_law_panel.py
DATA_DIR      <- file.path(BASE_DIR, "data")
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)


# ── 1. Load & filter FBI data ──────────────────────────────────────────────────
message("Loading FBI hate crime dataset…")
fbi <- read_csv(RAW_FBI, show_col_types = FALSE) |>
  rename_with(str_to_upper) |>                       # normalise to uppercase (2024 file ships lowercase)
  rename(any_of(c(                                   # handle column renames across file versions
    POPULATION_GROUP_DESC = "POPULATION_GROUP_DESCRIPTION"
  )))
message(sprintf("  Raw rows: %s", format(nrow(fbi), big.mark = ",")))

# Filter to LGBTQI+ bias motivations
lgbt_terms <- c("gay", "lesbian", "bisexual", "transgender",
                "gender non-conform", "sexual orient", "gender identity")
pattern    <- paste(lgbt_terms, collapse = "|")

df <- fbi |>
  filter(str_detect(str_to_lower(BIAS_DESC), pattern)) |>
  mutate(across(everything(), as.character))  # normalise before type-casting

message(sprintf("  LGBTQI+ rows: %s (%.1f%% of total)",
                format(nrow(df), big.mark = ","),
                100 * nrow(df) / nrow(fbi)))


# ── 2. Clean & type-cast ───────────────────────────────────────────────────────
message("Cleaning data…")

df <- df |>
  mutate(
    # Dates & time features — 2024 file uses ISO (YYYY-MM-DD), older used DD-MMM-YYYY.
    # parse_date_time() tries orders in sequence and does not evaluate dmy() on
    # dates that already parsed as ISO (avoids "All formats failed to parse").
    INCIDENT_DATE   = as_date(parse_date_time(INCIDENT_DATE, orders = c("Ymd", "dmy"))),
    YEAR            = as.integer(DATA_YEAR),
    MONTH           = month(INCIDENT_DATE),
    MONTH_NAME      = month(INCIDENT_DATE, label = TRUE, abbr = TRUE) |> as.character(),

    # Numeric counts — FBI CSV uses the literal string "NULL" (not R NA)
    VICTIM_COUNT            = as.integer(na_if(VICTIM_COUNT, "NULL")),
    TOTAL_OFFENDER_COUNT    = as.integer(na_if(TOTAL_OFFENDER_COUNT, "NULL")),
    ADULT_VICTIM_COUNT      = as.integer(replace_na(na_if(ADULT_VICTIM_COUNT, "NULL"), "0")),
    JUVENILE_VICTIM_COUNT   = as.integer(replace_na(na_if(JUVENILE_VICTIM_COUNT, "NULL"), "0")),
    JUVENILE_OFFENDER_COUNT = as.integer(replace_na(na_if(JUVENILE_OFFENDER_COUNT, "NULL"), "0")),

    # Binary flags
    MULTIPLE_OFFENSE_FLAG = as.integer(MULTIPLE_OFFENSE == "M"),
    MULTIPLE_BIAS_FLAG    = as.integer(MULTIPLE_BIAS    == "M"),
  )


# ── 3. Feature engineering ─────────────────────────────────────────────────────
message("Engineering features…")

# ── 3a. Simplified bias category ──────────────────────────────────────────────
classify_bias <- function(bias) {
  b <- str_to_lower(bias)
  case_when(
    str_detect(b, "gender non-conform")               ~ "Anti-Gender Non-Conforming",
    str_detect(b, "transgender")                       ~ "Anti-Transgender",
    str_detect(b, "bisexual")                          ~ "Anti-Bisexual",
    str_detect(b, "lesbian") & !str_detect(b, "gay")  ~ "Anti-Lesbian",
    str_detect(b, "lesbian") & str_detect(b, "gay")   ~ "Anti-LGBTQ (General)",
    str_detect(b, "gay")                               ~ "Anti-Gay (Male)",
    TRUE                                               ~ "Anti-LGBTQ (Other)"
  )
}

# ── 3b. Offense severity ───────────────────────────────────────────────────────
violent_terms <- c("Murder", "Rape", "Aggravated Assault", "Simple Assault",
                   "Robbery", "Arson", "Human Trafficking")

severity_tier <- function(offense) {
  case_when(
    str_detect(offense, "Murder|Rape")                              ~ 2L,
    str_detect(offense, "Aggravated Assault|Robbery|Arson")         ~ 2L,
    str_detect(offense, "Simple Assault")                           ~ 1L,
    TRUE                                                            ~ 0L
  )
}

# ── 3c. Season ─────────────────────────────────────────────────────────────────
season_from_month <- function(m) {
  case_when(
    m %in% c(12, 1, 2) ~ "Winter",
    m %in% c(3, 4, 5)  ~ "Spring",
    m %in% c(6, 7, 8)  ~ "Summer",
    m %in% c(9, 10, 11) ~ "Fall",
    TRUE               ~ NA_character_
  )
}

# ── 3d. Agency / location simplification ──────────────────────────────────────
simplify_agency <- function(a) {
  case_when(
    str_detect(a, "City")       ~ "City Police",
    str_detect(a, "County")     ~ "County Sheriff",
    str_detect(a, "University") ~ "University/College PD",
    str_detect(a, "State")      ~ "State Police",
    TRUE                        ~ "Other"
  )
}

simplify_location <- function(loc) {
  case_when(
    str_detect(loc, "Residence|Home")                      ~ "Residence/Home",
    str_detect(loc, "Street|Highway|Road|Alley|Sidewalk")  ~ "Street/Highway",
    str_detect(loc, "School|College")                      ~ "School/Campus",
    str_detect(loc, "Church|Synagogue|Mosque|Temple")      ~ "Religious Institution",
    str_detect(loc, "Bar|Restaurant")                      ~ "Bar/Restaurant",
    str_detect(loc, "Park")                                ~ "Park/Outdoor",
    TRUE                                                   ~ "Other"
  )
}

df <- df |>
  mutate(
    BIAS_CATEGORY      = classify_bias(BIAS_DESC),
    IS_VIOLENT         = as.integer(str_detect(OFFENSE_NAME,
                           paste(violent_terms, collapse = "|"))),
    SEVERITY_TIER      = severity_tier(OFFENSE_NAME),
    SEASON             = season_from_month(MONTH),
    JUVENILE_INVOLVED  = as.integer(JUVENILE_VICTIM_COUNT > 0 |
                                    JUVENILE_OFFENDER_COUNT > 0),
    AGENCY_TYPE_SIMPLE = simplify_agency(AGENCY_TYPE_NAME),
    LOCATION_SIMPLE    = simplify_location(LOCATION_NAME)
  )


# ── 4. Save full dataset (1991-2019) ───────────────────────────────────────────
full_cols <- c(
  "INCIDENT_ID", "YEAR", "MONTH", "MONTH_NAME", "SEASON",
  "STATE_NAME", "STATE_ABBR", "REGION_NAME", "DIVISION_NAME",
  "BIAS_DESC", "BIAS_CATEGORY",
  "OFFENSE_NAME", "IS_VIOLENT", "SEVERITY_TIER",
  "VICTIM_COUNT", "TOTAL_OFFENDER_COUNT",
  "ADULT_VICTIM_COUNT", "JUVENILE_VICTIM_COUNT",
  "JUVENILE_INVOLVED",
  "LOCATION_NAME", "LOCATION_SIMPLE",
  "OFFENDER_RACE", "OFFENDER_ETHNICITY",
  "AGENCY_TYPE_NAME", "AGENCY_TYPE_SIMPLE",
  "POPULATION_GROUP_DESC",
  "MULTIPLE_OFFENSE_FLAG", "MULTIPLE_BIAS_FLAG",
  "VICTIM_TYPES"
)

full_out <- file.path(DATA_DIR, "lgbtq_hate_crimes_full.csv")
df |> select(all_of(full_cols)) |> write_csv(full_out)

message(sprintf("\nSaved full dataset → %s", full_out))
message(sprintf("  Rows: %s  |  Years: %d–%d",
                format(nrow(df), big.mark = ","),
                min(df$YEAR), max(df$YEAR)))


# ── 5 / 6. Load auxiliary reference datasets, then build enriched ─────────────
# Note: enriched dataset is built by joining df with Gini + population reference
# data, so we load reference datasets first, then produce both outputs.
# ─────────────────────────────────────────────────────────────────────────────
message("\nLoading population and Gini reference datasets…")

# Year-specific Gini index (1991–2024, all states)
gini_panel <- read_csv(RAW_GINI, show_col_types = FALSE) |>
  rename(STATE_NAME = State, DATA_YEAR = Year, GINI_COEFFICIENT = GINI_Index)

# Track 1 denominator: total state population (1991–2024)
pop_total <- read_csv(RAW_POP_TOT, show_col_types = FALSE) |>
  rename(STATE_NAME = State, DATA_YEAR = Year, TOTAL_POPULATION = Total_Population)

# Track 2 denominator: LGBT adult population % (2012–2024 only)
# Note: pre-2012 data does not exist — see build_population_datasets.py for methodology
pop_lgbt <- read_csv(RAW_POP_LGBT, show_col_types = FALSE) |>
  rename(STATE_NAME = State, DATA_YEAR = Year, LGBT_PCT = LGBT_Pct)

message(sprintf("  Gini panel:         %s rows (%d–%d)",
                format(nrow(gini_panel), big.mark=","),
                min(gini_panel$DATA_YEAR), max(gini_panel$DATA_YEAR)))
message(sprintf("  Total population:   %s rows (%d–%d)",
                format(nrow(pop_total), big.mark=","),
                min(pop_total$DATA_YEAR), max(pop_total$DATA_YEAR)))
message(sprintf("  LGBT population %%: %s rows (%d–%d) — 2012+ only by design",
                format(nrow(pop_lgbt), big.mark=","),
                min(pop_lgbt$DATA_YEAR), max(pop_lgbt$DATA_YEAR)))

# Time-varying hate crime law panel (1991–2024, per state × year)
# LAW_LEVEL: 0 = no coverage, 1 = sexual orientation only, 2 = SO + gender identity
law_panel <- read_csv(RAW_LAW_PANEL, show_col_types = FALSE) |>
  rename(STATE_NAME = State, DATA_YEAR = Year)

message(sprintf("  Law panel:          %s rows (%d–%d)",
                format(nrow(law_panel), big.mark=","),
                min(law_panel$DATA_YEAR), max(law_panel$DATA_YEAR)))


# ── 7. State-year aggregate (Tableau / geographic analysis) ──────────────────
# Two normalisation tracks:
#   RATE_PER_100K_TOTAL  — incidents / 100k total population (1991–2024, full range)
#   RATE_PER_100K_LGBT   — incidents / 100k LGBT adults (2012–2024 only)
# ─────────────────────────────────────────────────────────────────────────────
message("\nBuilding state-year aggregate…")

# Build full incident counts from the complete dataset (1991–2024)
state_year_counts <- df |>
  mutate(DATA_YEAR = as.integer(YEAR)) |>
  group_by(DATA_YEAR, STATE_NAME) |>
  summarise(
    REGION_NAME   = first(REGION_NAME),
    INCIDENTS     = n(),
    TOTAL_VICTIMS = sum(as.integer(VICTIM_COUNT), na.rm = TRUE),
    VIOLENT_COUNT = sum(as.integer(IS_VIOLENT),   na.rm = TRUE),
    AVG_SEVERITY  = mean(as.numeric(SEVERITY_TIER), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(VIOLENT_PCT = round(VIOLENT_COUNT / INCIDENTS * 100, 1))

# Join Gini, total population, LGBT population, and time-varying law panel
state_year <- state_year_counts |>
  left_join(gini_panel, by = c("DATA_YEAR", "STATE_NAME")) |>
  left_join(pop_total,  by = c("DATA_YEAR", "STATE_NAME")) |>
  left_join(pop_lgbt,   by = c("DATA_YEAR", "STATE_NAME")) |>
  left_join(law_panel |> select(STATE_NAME, DATA_YEAR, LAW_LEVEL, HC_LAW_SO, HC_LAW_GI),
            by = c("DATA_YEAR", "STATE_NAME")) |>
  mutate(
    # Track 1: rate per 100k total population (available all years)
    RATE_PER_100K_TOTAL = if_else(
      !is.na(TOTAL_POPULATION) & TOTAL_POPULATION > 0,
      round((INCIDENTS / TOTAL_POPULATION) * 100000, 4),
      NA_real_
    ),
    # Track 2: rate per 100k LGBT adults (2012+ only; NA for earlier years by design)
    LGBT_POPULATION_EST = if_else(
      !is.na(LGBT_PCT) & !is.na(TOTAL_POPULATION),
      round(TOTAL_POPULATION * (LGBT_PCT / 100)),
      NA_real_
    ),
    RATE_PER_100K_LGBT = if_else(
      !is.na(LGBT_POPULATION_EST) & LGBT_POPULATION_EST > 0,
      round((INCIDENTS / LGBT_POPULATION_EST) * 100000, 4),
      NA_real_
    )
  )

sy_out <- file.path(DATA_DIR, "state_year_aggregate.csv")
state_year |> write_csv(sy_out)

message(sprintf("Saved state-year aggregate → %s", sy_out))
message(sprintf("  Rows: %s  |  Years: %d–%d",
                format(nrow(state_year), big.mark=","),
                min(state_year$DATA_YEAR), max(state_year$DATA_YEAR)))
message(sprintf("  RATE_PER_100K_TOTAL: available for all years (Track 1)"))
message(sprintf("  RATE_PER_100K_LGBT:  available 2012+ only (Track 2, by design)"))


# ── 8. Build enriched dataset (2010-2024, incident-level + policy + demographic) ─
# Joins each incident row with year-specific Gini, total population, and
# (where available) LGBT population fraction.  This is the primary ML input.
# ─────────────────────────────────────────────────────────────────────────────
message("\nBuilding enriched dataset (2010–2024)…")

enriched <- df |>
  filter(YEAR >= 2010) |>
  mutate(DATA_YEAR = as.integer(YEAR)) |>
  # Join year-specific Gini
  left_join(gini_panel, by = c("DATA_YEAR", "STATE_NAME")) |>
  # Join total population (Track 1 denominator)
  left_join(pop_total,  by = c("DATA_YEAR", "STATE_NAME")) |>
  # Join LGBT population fraction (Track 2 denominator, 2012+ only)
  left_join(pop_lgbt,   by = c("DATA_YEAR", "STATE_NAME")) |>
  # Join time-varying law panel (LAW_LEVEL specific to each state × year)
  # Source: build_law_panel.py (Movement Advancement Project)
  left_join(law_panel |> select(STATE_NAME, DATA_YEAR, LAW_LEVEL, HC_LAW_SO, HC_LAW_GI),
            by = c("STATE_NAME", "DATA_YEAR")) |>
  mutate(
    GINI_COEFFICIENT = as.numeric(GINI_COEFFICIENT),
    HAS_LAW          = as.integer(LAW_LEVEL > 0),
    FULL_PROTECTION  = as.integer(LAW_LEVEL == 2),
    # LGBT population estimate (absolute count) — NA before 2012
    LGBT_POPULATION  = if_else(
      !is.na(LGBT_PCT) & !is.na(TOTAL_POPULATION),
      round(TOTAL_POPULATION * (LGBT_PCT / 100)),
      NA_real_
    ),
    # Two-track per-incident rates (aggregated at incident level for ML)
    RATE_PER_100K_TOTAL = if_else(
      !is.na(TOTAL_POPULATION) & TOTAL_POPULATION > 0,
      round((1 / TOTAL_POPULATION) * 100000, 8),
      NA_real_
    ),
    RATE_PER_100K_LGBT = if_else(
      !is.na(LGBT_POPULATION) & LGBT_POPULATION > 0,
      round((1 / LGBT_POPULATION) * 100000, 8),
      NA_real_
    )
  )

enrich_out <- file.path(DATA_DIR, "lgbtq_hate_crimes_enriched.csv")
enriched |> write_csv(enrich_out)

message(sprintf("Saved enriched dataset → %s", enrich_out))
message(sprintf("  Rows: %s  |  Years: %d–%d",
                format(nrow(enriched), big.mark = ","),
                min(enriched$DATA_YEAR, na.rm = TRUE),
                max(enriched$DATA_YEAR, na.rm = TRUE)))


message("\n✓ Pipeline complete. Files written to /data/")
message("  lgbtq_hate_crimes_full.csv      — 1991-2024 full dataset")
message("  lgbtq_hate_crimes_enriched.csv  — 2010-2024 incident-level with Gini + population + law")
message("  state_year_aggregate.csv        — state × year summary with two-track rates (Tableau)")


# =============================================================================
# 7. SRS Master File parser (2026 fixed-width format)
# =============================================================================
# The FBI distributes a proprietary fixed-width "SRS National Master File" for
# agencies still on the legacy Summary Reporting System.  This section parses
# that format and optionally appends LGBTQI+ incidents to the full dataset.
#
# File format (IR — Incident Record, 312 chars, 0-indexed field positions):
#   [0:2]   Record type  ("IR" = incident, "BH" = bureau header — skip BH rows)
#   [2:4]   State code   (2-digit numeric, e.g. "50" = Alaska)
#   [4:6]   State abbr   (e.g. "AK")
#   [6:13]  ORI          (7-char agency identifier)
#   [13:25] Incident ID  (12 chars, alphanumeric)
#   [25:33] Date         (YYYYMMDD)
#   [40]    Offender race (W/B/U/I/M/P…)
#   [41:44] Offense code  (NIBRS 3-char, e.g. "13C" = Intimidation)
#   [221:223] Bias motivation code (2-digit numeric; blank = non-hate-crime)
#
# LGBTQI+ bias codes: 41=Anti-Gay(M), 42=Anti-Lesbian, 43=Anti-LGBTQ(Mixed),
#   44=Anti-Heterosexual, 45=Anti-Bisexual, 71=Anti-Transgender,
#   72=Anti-Gender Non-Conforming
# =============================================================================

# ── State code → abbreviation lookup ─────────────────────────────────────────
srs_state_lookup <- tribble(
  ~code, ~abbr, ~name,
  "01", "AL", "Alabama",      "02", "AK", "Alaska",      "03", "AR", "Arkansas",
  "04", "AZ", "Arizona",      "05", "CA", "California",  "06", "CO", "Colorado",
  "07", "CT", "Connecticut",  "08", "DE", "Delaware",    "09", "DC", "District of Columbia",
  "10", "FL", "Florida",      "11", "GA", "Georgia",     "12", "ID", "Idaho",
  "13", "IL", "Illinois",     "14", "IN", "Indiana",     "15", "IA", "Iowa",
  "16", "KS", "Kansas",       "17", "KY", "Kentucky",    "18", "LA", "Louisiana",
  "19", "ME", "Maine",        "20", "MD", "Maryland",    "21", "MA", "Massachusetts",
  "22", "MI", "Michigan",     "23", "MN", "Minnesota",   "24", "MS", "Mississippi",
  "25", "MO", "Missouri",     "26", "MT", "Montana",     "27", "NE", "Nebraska",
  "28", "NV", "Nevada",       "29", "NH", "New Hampshire","30", "NJ", "New Jersey",
  "31", "NM", "New Mexico",   "32", "NY", "New York",    "33", "NC", "North Carolina",
  "34", "ND", "North Dakota", "35", "OH", "Ohio",        "36", "OK", "Oklahoma",
  "37", "OR", "Oregon",       "38", "PA", "Pennsylvania","39", "RI", "Rhode Island",
  "40", "SC", "South Carolina","41", "SD", "South Dakota","42", "TN", "Tennessee",
  "43", "TX", "Texas",        "44", "UT", "Utah",        "45", "VT", "Vermont",
  "46", "VA", "Virginia",     "47", "WA", "Washington",  "48", "WV", "West Virginia",
  "49", "WI", "Wisconsin",    "50", "AK", "Alaska",      "51", "WY", "Wyoming",
  "52", "PR", "Puerto Rico",  "53", "GU", "Guam"
)

# ── NIBRS offense code → readable offense name ────────────────────────────────
nibrs_offense_map <- c(
  "09A" = "Murder/Non-Negligent Homicide", "09B" = "Negligent Manslaughter",
  "09C" = "Justifiable Homicide",          "100" = "Kidnapping/Abduction",
  "11A" = "Forcible Rape",                 "11B" = "Sexual Assault w/Object",
  "11C" = "Fondling",                      "11D" = "Statutory Rape",
  "120" = "Robbery",                       "13A" = "Aggravated Assault",
  "13B" = "Simple Assault",               "13C" = "Intimidation",
  "200" = "Arson",                         "210" = "Extortion/Blackmail",
  "220" = "Burglary/Breaking & Entering",  "23A" = "Pocket-picking",
  "23B" = "Purse-snatching",              "23C" = "Shoplifting",
  "23D" = "Theft From Building",          "23E" = "Theft From Coin-Operated Device",
  "23F" = "Theft From Motor Vehicle",     "23G" = "Theft of Motor Vehicle Parts",
  "23H" = "All Other Larceny",            "240" = "Motor Vehicle Theft",
  "250" = "Counterfeiting/Forgery",       "260" = "False Pretenses/Fraud",
  "270" = "Embezzlement",                 "280" = "Stolen Property Offenses",
  "290" = "Destruction/Damage/Vandalism", "35A" = "Drug Sale/Manufacturing",
  "35B" = "Drug Possession",              "36A" = "Gambling Equipment Violations",
  "370" = "Pornography/Obscene Material", "40A" = "Prostitution",
  "520" = "Weapon Law Violations",        "90A" = "Bad Checks",
  "90C" = "Disorderly Conduct",           "90D" = "Driving Under Influence",
  "90J" = "Trespass",                     "90Z" = "All Other Offenses"
)

# ── Bias code → BIAS_DESC string (matching existing CSV vocabulary) ────────────
bias_code_to_desc <- c(
  "41" = "Anti-Gay (Male)",               "42" = "Anti-Lesbian",
  "43" = "Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed)",
  "44" = "Anti-Heterosexual",             "45" = "Anti-Bisexual",
  "71" = "Anti-Transgender",              "72" = "Anti-Gender Non-Conforming"
)

# ── Main parser function ───────────────────────────────────────────────────────
parse_srs_master_file <- function(filepath) {
  # Validate file exists
  if (!file.exists(filepath)) {
    warning("SRS Master File not found: ", filepath)
    return(NULL)
  }

  message(sprintf("Parsing SRS Master File: %s", basename(filepath)))
  raw_lines <- readLines(filepath, warn = FALSE)

  # Keep only IR records
  ir_lines <- raw_lines[substr(raw_lines, 1, 2) == "IR"]
  message(sprintf("  Total IR records: %s", format(length(ir_lines), big.mark = ",")))

  # LGBTQI+ bias codes (2-digit numeric, at 1-based position 222-223)
  lgbtq_bias_codes <- c("41", "42", "43", "44", "45", "71", "72")

  # Extract fields using fixed-width positions (1-based for substr())
  extract_field <- function(lines, start, end) trimws(substr(lines, start, end))

  bias_codes <- extract_field(ir_lines, 222, 223)
  lgbtq_mask <- bias_codes %in% lgbtq_bias_codes

  lgbtq_ir <- ir_lines[lgbtq_mask]
  n_lgbtq   <- sum(lgbtq_mask)
  n_total_hc <- sum(bias_codes != "")

  message(sprintf("  Total hate crime records: %d", n_total_hc))
  message(sprintf("  LGBTQI+ records: %d", n_lgbtq))
  message(sprintf("  Coverage: partial year / SRS-reporting agencies only"))

  if (n_lgbtq == 0) {
    message("  No LGBTQI+ records found.")
    return(NULL)
  }

  # Extract fields for LGBTQI+ records
  state_abbr  <- extract_field(lgbtq_ir, 5, 6)
  ori         <- extract_field(lgbtq_ir, 7, 13)
  incident_id <- extract_field(lgbtq_ir, 14, 25)
  date_raw    <- extract_field(lgbtq_ir, 26, 33)
  offense_raw <- extract_field(lgbtq_ir, 42, 44)
  bias_raw    <- extract_field(lgbtq_ir, 222, 223)

  # Parse dates
  incident_dates <- as.Date(date_raw, format = "%Y%m%d")
  year_vals  <- as.integer(format(incident_dates, "%Y"))
  month_vals <- as.integer(format(incident_dates, "%m"))

  # Map codes to labels
  offense_names <- nibrs_offense_map[offense_raw]
  offense_names[is.na(offense_names)] <- paste0("Code ", offense_raw[is.na(offense_names)])

  bias_descs    <- bias_code_to_desc[bias_raw]

  # Build tidy data frame using same column structure as lgbtq_hate_crimes_full.csv
  parsed <- tibble(
    INCIDENT_ID     = incident_id,
    YEAR            = year_vals,
    MONTH           = month_vals,
    MONTH_NAME      = as.character(month(incident_dates, label = TRUE, abbr = TRUE)),
    SEASON          = season_from_month(month_vals),
    STATE_ABBR      = state_abbr,
    STATE_NAME      = srs_state_lookup$name[match(state_abbr, srs_state_lookup$abbr)],
    REGION_NAME     = NA_character_,   # not available in SRS format
    DIVISION_NAME   = NA_character_,
    BIAS_DESC       = bias_descs,
    BIAS_CATEGORY   = classify_bias(bias_descs),
    OFFENSE_NAME    = offense_names,
    IS_VIOLENT      = as.integer(str_detect(
                        offense_names, paste(violent_terms, collapse = "|"))),
    SEVERITY_TIER   = severity_tier(offense_names),
    VICTIM_COUNT    = NA_integer_,
    TOTAL_OFFENDER_COUNT  = NA_integer_,
    ADULT_VICTIM_COUNT    = NA_integer_,
    JUVENILE_VICTIM_COUNT = NA_integer_,
    JUVENILE_INVOLVED     = NA_integer_,
    LOCATION_NAME   = NA_character_,
    LOCATION_SIMPLE = NA_character_,
    OFFENDER_RACE   = NA_character_,
    OFFENDER_ETHNICITY    = NA_character_,
    AGENCY_TYPE_NAME      = NA_character_,
    AGENCY_TYPE_SIMPLE    = NA_character_,
    POPULATION_GROUP_DESC = NA_character_,
    MULTIPLE_OFFENSE_FLAG = NA_integer_,
    MULTIPLE_BIAS_FLAG    = NA_integer_,
    VICTIM_TYPES    = NA_character_,
    DATA_SOURCE     = "SRS_MASTER_FILE_2026"   # provenance flag
  )

  message(sprintf("  Parsed %d LGBTQI+ incidents (%d–%d)",
                  nrow(parsed), min(year_vals, na.rm = TRUE),
                  max(year_vals, na.rm = TRUE)))
  parsed
}


# ── Optionally append 2026 SRS data to full dataset ──────────────────────────
if (INCLUDE_2026_SRS && file.exists(SRS_FILE)) {
  srs_data <- parse_srs_master_file(SRS_FILE)
  if (!is.null(srs_data) && nrow(srs_data) > 0) {
    # Add provenance column to existing full dataset for transparency
    df_full_tagged <- df |>
      select(all_of(full_cols)) |>
      mutate(DATA_SOURCE = "FBI_UCR_CSV")

    # Bind — only keep columns present in both (some SRS fields are NA)
    common_cols <- intersect(names(df_full_tagged), names(srs_data))
    df_combined <- bind_rows(
      df_full_tagged |> select(all_of(common_cols)),
      srs_data       |> select(all_of(common_cols))
    )

    combined_out <- file.path(DATA_DIR, "lgbtq_hate_crimes_full.csv")
    df_combined |> write_csv(combined_out)

    message(sprintf("\n2026 SRS data appended to full dataset."))
    message(sprintf("  Combined rows: %s  |  Years: %d–%d",
                    format(nrow(df_combined), big.mark = ","),
                    min(df_combined$YEAR, na.rm = TRUE),
                    max(df_combined$YEAR, na.rm = TRUE)))
  }
} else if (INCLUDE_2026_SRS) {
  message("\nWARNING: INCLUDE_2026_SRS=TRUE but SRS file not found at:")
  message(sprintf("  %s", SRS_FILE))
  message("  Skipping 2026 data — full dataset unchanged.")
}
