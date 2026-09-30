# =============================================================================
# LGBTQI+ Hate Crime Analysis — Exploratory Data Analysis
# =============================================================================
# Author : Guilherme Arpi
# Inputs : data/lgbtq_hate_crimes_full.csv
#          data/state_year_aggregate.csv   ← two-track normalisation
# Outputs: charts/ (10 publication-quality ggplot2 charts, PNG 150 dpi)
#
# Normalisation strategy
# ──────────────────────
#   Track 1  RATE_PER_100K_TOTAL  — incidents per 100k total population
#            Available 1991–2024. Used for full-period trend charts.
#
#   Track 2  RATE_PER_100K_LGBT   — incidents per 100k LGBT adult residents
#            Available 2012–2024 only (pre-2012 self-ID data does not exist).
#            Used for policy and inequality analysis where community exposure
#            is the appropriate denominator.
#
# Run 01_data_pipeline.R first (then build_gini_dataset.py +
# build_population_datasets.py if reference CSVs are missing).
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)
library(RColorBrewer)

# ── Paths ──────────────────────────────────────────────────────────────────────
# BASE_DIR   <- dirname(rstudioapi::getActiveDocumentContext()$path)
BASE_DIR <- if (basename(getwd()) == "R") dirname(getwd()) else getwd()  # works from repo root or from R/
DATA_DIR   <- file.path(BASE_DIR, "data")
CHARTS_DIR <- file.path(BASE_DIR, "charts")
dir.create(CHARTS_DIR, showWarnings = FALSE, recursive = TRUE)


# ── Colour palette ─────────────────────────────────────────────────────────────
BIAS_COLORS <- c(
  "Anti-Gay (Male)"             = "#2196F3",
  "Anti-Lesbian"                = "#E91E63",
  "Anti-LGBTQ (General)"        = "#9C27B0",
  "Anti-Transgender"            = "#FF9800",
  "Anti-Bisexual"               = "#4CAF50",
  "Anti-Gender Non-Conforming"  = "#795548"
)
REGION_COLORS <- c(
  "West"      = "#1565C0",
  "Northeast" = "#C62828",
  "South"     = "#2E7D32",
  "Midwest"   = "#F57F17"
)
LAW_COLORS <- c(
  "No Law"            = "#B71C1C",
  "Sexual Orientation" = "#EF9A9A",
  "Full Protection"   = "#1B5E20"
)

# Shared ggplot2 theme
theme_hate <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, colour = "#1A1A1A"),
      plot.subtitle    = element_text(size = 10, colour = "#555555"),
      axis.text        = element_text(colour = "#444444"),
      axis.title       = element_text(size = 10, colour = "#444444"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#EEEEEE"),
      legend.text      = element_text(size = 9),
      legend.title     = element_text(size = 9, face = "bold"),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "#FAFAFA", colour = NA)
    )
}

save_chart <- function(plot, filename, width = 10, height = 5) {
  path <- file.path(CHARTS_DIR, filename)
  ggsave(path, plot = plot, width = width, height = height,
         dpi = 150, bg = "white")
  message(sprintf("  Saved → %s", filename))
}


# ── Load data ──────────────────────────────────────────────────────────────────
message("Loading datasets…")
full <- read_csv(file.path(DATA_DIR, "lgbtq_hate_crimes_full.csv"),
                 show_col_types = FALSE)

# State-year aggregate — primary source for all normalised charts
# Columns include: DATA_YEAR, STATE_NAME, REGION_NAME, INCIDENTS,
#   VIOLENT_COUNT, VIOLENT_PCT, GINI_COEFFICIENT,
#   TOTAL_POPULATION, LGBT_PCT, LGBT_POPULATION_EST,
#   RATE_PER_100K_TOTAL (Track 1, all years),
#   RATE_PER_100K_LGBT  (Track 2, 2012+ only — NA before 2012 by design)
sy <- read_csv(file.path(DATA_DIR, "state_year_aggregate.csv"),
               show_col_types = FALSE)

# Time-varying law level lookup — one row per state × year (1991–2024)
# Source: state_law_panel.csv (built by build_law_panel.py, MAP data)
# Level 0 = no hate crime law covering LGBTQI+
# Level 1 = covers sexual orientation only
# Level 2 = full protection (sexual orientation + gender identity)
law_lookup <- tryCatch(
  read_csv(file.path(DATA_DIR, "state_law_panel.csv"), show_col_types = FALSE) |>
    rename(STATE_NAME = State, DATA_YEAR = Year) |>
    select(STATE_NAME, DATA_YEAR, LAW_LEVEL, HC_LAW_SO, HC_LAW_GI),
  error = function(e) {
    message("  Note: state_law_panel.csv not found — law-level chart will be skipped.")
    NULL
  }
)

message(sprintf("  Full: %s rows (%d–%d) | State-year: %s rows",
                format(nrow(full), big.mark = ","),
                min(full$YEAR, na.rm = TRUE), max(full$YEAR, na.rm = TRUE),
                format(nrow(sy), big.mark = ",")))
message("\nGenerating charts…")


# ══════════════════════════════════════════════════════════════════════════════
# Chart 01 — Annual incident count 1991–2024 with trend line + key events
# ══════════════════════════════════════════════════════════════════════════════
yearly <- full |> count(YEAR, name = "INCIDENTS")
trend_mod <- lm(INCIDENTS ~ YEAR, data = yearly)
yearly    <- yearly |> mutate(TREND = predict(trend_mod))

events <- tibble(
  YEAR  = c(1998, 2003, 2015, 2016, 2020),
  LABEL = c("Matthew Shepard\nmurder", "Lawrence v.\nTexas",
            "Obergefell\nv. Hodges", "Pulse\nNightclub",
            "COVID-19\npandemic"),
  VJUST = c(-1.2, 2.2, -1.2, 2.2, -1.2)
) |> left_join(yearly, by = "YEAR")

p01 <- ggplot(yearly, aes(x = YEAR, y = INCIDENTS)) +
  geom_area(alpha = 0.12, fill = "#1565C0") +
  geom_line(colour = "#1565C0", linewidth = 1.2) +
  geom_point(colour = "#1565C0", size = 2) +
  geom_line(aes(y = TREND), colour = "#B71C1C", linewidth = 1,
            linetype = "dashed") +
  geom_vline(data = events, aes(xintercept = YEAR),
             colour = "#AAAAAA", linewidth = 0.6, linetype = "dotted") +
  geom_text(data = events, aes(label = LABEL, vjust = VJUST),
            size = 2.8, colour = "#555555", lineheight = 0.9) +
  annotate("text", x = 2021, y = max(yearly$TREND) + 80,
           label = "Linear trend", colour = "#B71C1C", size = 3, fontface = "italic") +
  scale_x_continuous(breaks = seq(1991, 2024, 4)) +
  scale_y_continuous(labels = comma) +
  labs(title    = "LGBTQI+ Hate Crime Incidents in the US (1991–2024)",
       subtitle = "FBI Uniform Crime Reporting Program — absolute incident counts",
       x = "Year", y = "Number of Incidents") +
  theme_hate()

save_chart(p01, "01_annual_trend.png", width = 12, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Chart 02 — Stacked area: incidents by bias category over time
# ══════════════════════════════════════════════════════════════════════════════
cat_year <- full |>
  filter(BIAS_CATEGORY %in% names(BIAS_COLORS)) |>
  count(YEAR, BIAS_CATEGORY, name = "INCIDENTS") |>
  mutate(BIAS_CATEGORY = factor(BIAS_CATEGORY, levels = names(BIAS_COLORS)))

p02 <- ggplot(cat_year, aes(x = YEAR, y = INCIDENTS, fill = BIAS_CATEGORY)) +
  geom_area(position = "stack", alpha = 0.88) +
  scale_fill_manual(values = BIAS_COLORS, name = "Bias Category") +
  scale_x_continuous(breaks = seq(1991, 2024, 4)) +
  scale_y_continuous(labels = comma) +
  labs(title    = "LGBTQI+ Hate Crimes by Subgroup (1991–2024)",
       subtitle = "Stacked by primary bias motivation",
       x = "Year", y = "Incidents") +
  theme_hate() +
  theme(legend.position = "right")

save_chart(p02, "02_subgroup_trend.png", width = 12, height = 6)


# ══════════════════════════════════════════════════════════════════════════════
# Chart 03 — Anti-Transgender & GNC zoom (2013–2024)
# ══════════════════════════════════════════════════════════════════════════════
trans_cats <- c("Anti-Transgender", "Anti-Gender Non-Conforming")

trans_data <- full |>
  filter(BIAS_CATEGORY %in% trans_cats, YEAR >= 2013) |>
  count(YEAR, BIAS_CATEGORY, name = "INCIDENTS")

p03 <- ggplot(trans_data, aes(x = YEAR, y = INCIDENTS,
                               colour = BIAS_CATEGORY, fill = BIAS_CATEGORY)) +
  geom_area(alpha = 0.15, position = "identity") +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3) +
  scale_colour_manual(values = BIAS_COLORS[trans_cats], name = NULL) +
  scale_fill_manual(values   = BIAS_COLORS[trans_cats], name = NULL) +
  scale_x_continuous(breaks = seq(2013, 2024, 2)) +
  scale_y_continuous(labels = comma) +
  labs(title    = "Anti-Transgender & Anti-GNC Hate Crimes (2013–2024)",
       subtitle = "FBI began reporting gender identity separately in 2013",
       x = "Year", y = "Incidents") +
  theme_hate()

save_chart(p03, "03_transgender_trend.png", width = 9, height = 4.5)


# ══════════════════════════════════════════════════════════════════════════════
# Chart 04 — Offense type breakdown
# ══════════════════════════════════════════════════════════════════════════════
offense_counts <- full |>
  mutate(
    OFFENSE_SHORT = OFFENSE_NAME |>
      str_replace("Destruction/Damage/Vandalism of Property", "Vandalism") |>
      str_replace("Murder and Nonnegligent Manslaughter", "Murder"),
    VIOLENT_LABEL = if_else(IS_VIOLENT == 1, "Violent", "Non-violent")
  ) |>
  count(OFFENSE_SHORT, VIOLENT_LABEL, sort = TRUE) |>
  slice_max(n, n = 12) |>
  mutate(OFFENSE_SHORT = fct_reorder(OFFENSE_SHORT, n))

p04 <- ggplot(offense_counts,
              aes(x = n, y = OFFENSE_SHORT, fill = VIOLENT_LABEL)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 3, colour = "#444444") +
  scale_fill_manual(values = c("Violent" = "#B71C1C", "Non-violent" = "#546E7A"),
                    name = NULL) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Top Offense Types in LGBTQI+ Hate Crimes (1991–2024)",
       x = "Number of Incidents", y = NULL) +
  theme_hate() +
  theme(legend.position = "top")

save_chart(p04, "04_offense_types.png", width = 10, height = 6)


# ══════════════════════════════════════════════════════════════════════════════
# Chart 05 — Regional breakdown over time
# ══════════════════════════════════════════════════════════════════════════════
region_year <- full |>
  filter(REGION_NAME %in% names(REGION_COLORS)) |>
  count(YEAR, REGION_NAME, name = "INCIDENTS")

p05 <- ggplot(region_year,
              aes(x = YEAR, y = INCIDENTS, colour = REGION_NAME)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = REGION_COLORS, name = "Region") +
  scale_x_continuous(breaks = seq(1991, 2024, 4)) +
  scale_y_continuous(labels = comma) +
  labs(title    = "LGBTQI+ Hate Crimes by US Region (1991–2024)",
       x = "Year", y = "Incidents") +
  theme_hate()

save_chart(p05, "05_regional_trend.png", width = 12, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Chart 06 — Top 15 states by total incidents (full period)
# ══════════════════════════════════════════════════════════════════════════════
top_states <- sy |>
  group_by(STATE_NAME) |>
  summarise(TOTAL_INCIDENTS = sum(INCIDENTS, na.rm = TRUE), .groups = "drop") |>
  slice_max(TOTAL_INCIDENTS, n = 15) |>
  mutate(STATE_NAME = fct_reorder(STATE_NAME, TOTAL_INCIDENTS))

p06 <- ggplot(top_states, aes(x = TOTAL_INCIDENTS, y = STATE_NAME)) +
  geom_col(fill = "#1565C0", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = comma(TOTAL_INCIDENTS)),
            hjust = -0.1, size = 3, colour = "#444444") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Top 15 States — LGBTQI+ Hate Crime Incidents (1991–2024)",
       x = "Total Incidents", y = NULL) +
  theme_hate()

save_chart(p06, "06_top_states.png", width = 9, height = 7)


# ══════════════════════════════════════════════════════════════════════════════
# Chart 07 — Hate crime law: incident rate by protection level (box plot)
# Track 2 — incidents per 100k LGBT residents (2012–2024)
# Time-varying join: each state × year gets the law level that was in effect
# that year (not a single static snapshot per state)
# ══════════════════════════════════════════════════════════════════════════════
if (!is.null(law_lookup)) {
  sy_law <- sy |>
    filter(DATA_YEAR >= 2012, !is.na(RATE_PER_100K_LGBT)) |>
    # Pipeline already attached LAW_LEVEL; drop it so the panel join is not
    # suffixed to LAW_LEVEL.x / LAW_LEVEL.y.
    select(-any_of(c("LAW_LEVEL", "HC_LAW_SO", "HC_LAW_GI"))) |>
    left_join(law_lookup, by = c("STATE_NAME", "DATA_YEAR")) |>
    filter(!is.na(LAW_LEVEL)) |>
    mutate(LAW_LABEL = factor(
      case_when(
        LAW_LEVEL == 0 ~ "No Law",
        LAW_LEVEL == 1 ~ "Sexual Orientation",
        LAW_LEVEL == 2 ~ "Full Protection"
      ),
      levels = c("No Law", "Sexual Orientation", "Full Protection")
    ))

  p07 <- ggplot(sy_law, aes(x = LAW_LABEL, y = RATE_PER_100K_LGBT,
                              fill = LAW_LABEL)) +
    geom_boxplot(outlier.size = 1.2, outlier.alpha = 0.4,
                 width = 0.55, linewidth = 0.6) +
    scale_fill_manual(values = LAW_COLORS, guide = "none") +
    scale_y_continuous(labels = number_format(accuracy = 0.1)) +
    labs(title    = "LGBTQI+ Hate Crime Rate by State Law Coverage (2012–2024)",
         subtitle = "Track 2 — incidents per 100k LGBT residents (state × year observations)",
         x = NULL, y = "Incidents per 100k LGBT residents") +
    theme_hate()

  save_chart(p07, "07_law_impact_boxplot.png", width = 9, height = 5)
} else {
  message("  Skipping Chart 07 — law lookup unavailable")
}


# ══════════════════════════════════════════════════════════════════════════════
# Chart 08 — Seasonality heatmap: month × bias category
# ══════════════════════════════════════════════════════════════════════════════
month_levels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

heat_data <- full |>
  filter(BIAS_CATEGORY %in% names(BIAS_COLORS), !is.na(MONTH_NAME)) |>
  count(MONTH_NAME, BIAS_CATEGORY, name = "INCIDENTS") |>
  mutate(MONTH_NAME = factor(MONTH_NAME, levels = month_levels))

p08 <- ggplot(heat_data,
              aes(x = MONTH_NAME, y = BIAS_CATEGORY, fill = INCIDENTS)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = comma(INCIDENTS, accuracy = 1)),
            size = 2.8, colour = "white") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       labels = comma, name = "Incidents") +
  scale_y_discrete(limits = rev(names(BIAS_COLORS))) +
  labs(title    = "LGBTQI+ Hate Crime Seasonality (1991–2024)",
       subtitle = "Incidents by month and bias subgroup",
       x = "Month", y = NULL) +
  theme_hate() +
  theme(panel.grid = element_blank())

save_chart(p08, "08_seasonality_heatmap.png", width = 11, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Chart 09 — Gini vs. incident rate: TWO-TRACK comparison
# Left panel  — Track 1: rate per 100k total population (1991–2024)
# Right panel — Track 2: rate per 100k LGBT residents (2012–2024 only)
# ══════════════════════════════════════════════════════════════════════════════

# Track 1 — full period, total population denominator
sy_t1 <- sy |>
  filter(!is.na(GINI_COEFFICIENT), !is.na(RATE_PER_100K_TOTAL),
         !is.na(REGION_NAME), REGION_NAME %in% names(REGION_COLORS))

r1 <- cor(sy_t1$GINI_COEFFICIENT, sy_t1$RATE_PER_100K_TOTAL,
          use = "complete.obs")

p09a <- ggplot(sy_t1, aes(x = GINI_COEFFICIENT, y = RATE_PER_100K_TOTAL,
                            colour = REGION_NAME)) +
  geom_point(alpha = 0.35, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "#333333",
              linewidth = 1, linetype = "dashed", inherit.aes = FALSE,
              aes(x = GINI_COEFFICIENT, y = RATE_PER_100K_TOTAL)) +
  scale_colour_manual(values = REGION_COLORS, name = "Region") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  annotate("label", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
           label = sprintf("r = %.3f", r1), size = 3.5,
           fill = "white", colour = "#333333") +
  labs(title    = "Track 1 — per 100k total population",
       subtitle = "1991–2024, all state-year observations",
       x = "Gini Coefficient", y = "Incidents per 100k population") +
  theme_hate()

# Track 2 — 2012+ only, LGBT population denominator
sy_t2 <- sy |>
  filter(!is.na(GINI_COEFFICIENT), !is.na(RATE_PER_100K_LGBT),
         !is.na(REGION_NAME), REGION_NAME %in% names(REGION_COLORS),
         DATA_YEAR >= 2012)

message(sprintf("  sy_t2 rows: %d", nrow(sy_t2)))

if (nrow(sy_t2) >= 5) {
  r2 <- cor(sy_t2$GINI_COEFFICIENT, sy_t2$RATE_PER_100K_LGBT,
            use = "complete.obs")

  p09b <- ggplot(sy_t2, aes(x = GINI_COEFFICIENT, y = RATE_PER_100K_LGBT,
                              colour = REGION_NAME)) +
    geom_point(alpha = 0.35, size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "#333333",
                linewidth = 1, linetype = "dashed", inherit.aes = FALSE,
                aes(x = GINI_COEFFICIENT, y = RATE_PER_100K_LGBT)) +
    scale_colour_manual(values = REGION_COLORS, name = "Region") +
    scale_y_continuous(labels = number_format(accuracy = 0.1)) +
    annotate("label", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
             label = sprintf("r = %.3f", r2), size = 3.5,
             fill = "white", colour = "#333333") +
    labs(title    = "Track 2 — per 100k LGBT residents",
         subtitle = "2012–2024 only (pre-2012 self-ID data unavailable)",
         x = "Gini Coefficient", y = "Incidents per 100k LGBT residents") +
    theme_hate() +
    theme(legend.position = "none")

  p09 <- (p09a + p09b) +
    plot_annotation(
      title    = "Income Inequality (Gini) vs. LGBTQI+ Hate Crime Rate",
      subtitle = "Two-track normalisation — left: total population; right: LGBT population (2012+ only)",
      theme    = theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "#555555")
      )
    )
  save_chart(p09, "09_gini_vs_rate.png", width = 14, height = 6)

} else {
  # Track 2 data not yet available — save Track 1 only with a note
  message("  Note: Track 2 (RATE_PER_100K_LGBT) unavailable — run build_population_datasets.py to enable.")
  p09 <- p09a +
    plot_annotation(
      title    = "Income Inequality (Gini) vs. LGBTQI+ Hate Crime Rate",
      subtitle = "Track 1 only (per 100k total pop.) — run build_population_datasets.py to add Track 2",
      theme    = theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "#B71C1C")
      )
    )
  save_chart(p09, "09_gini_vs_rate.png", width = 9, height = 6)
}


# ══════════════════════════════════════════════════════════════════════════════
# Chart 10 — Violent crime share over time, by subgroup
# ══════════════════════════════════════════════════════════════════════════════
main_cats <- c("Anti-Gay (Male)", "Anti-Lesbian",
               "Anti-LGBTQ (General)", "Anti-Transgender")

viol_share <- full |>
  filter(BIAS_CATEGORY %in% main_cats) |>
  group_by(YEAR, BIAS_CATEGORY) |>
  summarise(
    TOTAL   = n(),
    VIOLENT = sum(IS_VIOLENT, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(PCT_VIOLENT = VIOLENT / TOTAL * 100)

p10 <- ggplot(viol_share,
              aes(x = YEAR, y = PCT_VIOLENT, colour = BIAS_CATEGORY)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 50, colour = "#AAAAAA",
             linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = 1993, y = 52, label = "50%",
           colour = "#AAAAAA", size = 3) +
  scale_colour_manual(values = BIAS_COLORS[main_cats], name = NULL) +
  scale_x_continuous(breaks = seq(1991, 2024, 4)) +
  scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1),
                     limits = c(0, 100)) +
  labs(title    = "Share of Violent Incidents by LGBTQI+ Subgroup (1991–2024)",
       subtitle = "% of incidents classified as violent (assault, murder, robbery, arson)",
       x = "Year", y = "% Violent Incidents") +
  theme_hate()

save_chart(p10, "10_violent_share_over_time.png", width = 12, height = 5)


message(sprintf("\n✓ EDA complete. 10 charts saved to %s/", CHARTS_DIR))
