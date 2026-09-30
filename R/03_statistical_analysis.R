# =============================================================================
# LGBTQI+ Hate Crime Analysis — Statistical Analysis
# =============================================================================
# Author : Guilherme Arpi
#
# Two-track normalisation strategy
#   Track 1 — RATE_PER_100K_TOTAL  : per 100k total population, 1991–2024
#              (Census intercensal estimates, all years available)
#   Track 2 — RATE_PER_100K_LGBT   : per 100k LGBT adults, 2012–2024 only
#              (Williams Institute / Gallup / BRFSS; pre-2012 data unavailable)
#
# Tests performed:
#   1. Mann-Kendall trend test     — monotonic trend in annual incidents (1991–2024)
#   2. Welch's t-test              — incident rate: states with vs. without law
#                                    (Track 1 all years; Track 2 2012+ only)
#   3. Subgroup growth rates       — CAGR 1991→2024 by bias category
#   4. Spearman correlation matrix — key numeric variables (state × year)
#   5. OLS multiple regression     — primary: Track 1 (all years);
#                                    secondary: Track 2 (2012+)
#   6. Chi-square test             — season × violent/non-violent offense
#
# Outputs:
#   reports/statistical_findings.txt
#   charts/11_correlation_matrix.png
#   charts/12_regression_diagnostics.png
#   charts/13_law_comparison_bar.png
#   charts/14_subgroup_growth_rates.png
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)
library(broom)

# ── Paths ──────────────────────────────────────────────────────────────────────
#BASE_DIR    <- dirname(rstudioapi::getActiveDocumentContext()$path)
BASE_DIR <- if (basename(getwd()) == "R") dirname(getwd()) else getwd()  # works from repo root or from R/

DATA_DIR    <- file.path(BASE_DIR, "data")
CHARTS_DIR  <- file.path(BASE_DIR, "charts")
REPORTS_DIR <- file.path(BASE_DIR, "reports")
dir.create(REPORTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Shared theme
theme_hate <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, colour = "#1A1A1A"),
      plot.subtitle    = element_text(size = 10, colour = "#555555"),
      axis.text        = element_text(colour = "#444444"),
      axis.title       = element_text(size = 10, colour = "#444444"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#EEEEEE"),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "#FAFAFA", colour = NA)
    )
}

save_chart <- function(plot, filename, width = 9, height = 5) {
  ggsave(file.path(CHARTS_DIR, filename), plot = plot,
         width = width, height = height, dpi = 150, bg = "white")
  message(sprintf("  Saved → %s", filename))
}

# Accumulate results for report
report_lines <- c(
  strrep("=", 72),
  "LGBTQI+ HATE CRIME ANALYSIS — STATISTICAL FINDINGS",
  strrep("=", 72), ""
)
add_result <- function(...) {
  report_lines <<- c(report_lines, ..., strrep("-", 72), "")
}


# ── Load data ──────────────────────────────────────────────────────────────────
message("Loading data…")

# Incident-level data (1991–2024)
full <- read_csv(file.path(DATA_DIR, "lgbtq_hate_crimes_full.csv"),
                 show_col_types = FALSE) |>
  rename_with(str_to_upper)

# State × year aggregate with two-track rates (pipeline output)
sy <- read_csv(file.path(DATA_DIR, "state_year_aggregate.csv"),
               show_col_types = FALSE) |>
  rename_with(str_to_upper)

# Time-varying law-level lookup — one row per state × year (1991–2024)
# Source: state_law_panel.csv (built by build_law_panel.py, MAP data)
law_lookup <- tryCatch(
  read_csv(file.path(DATA_DIR, "state_law_panel.csv"), show_col_types = FALSE) |>
    rename(STATE_NAME = State, DATA_YEAR = Year) |>
    select(STATE_NAME, DATA_YEAR, LAW_LEVEL, HC_LAW_SO, HC_LAW_GI),
  error = function(e) {
    message("  Warning: state_law_panel.csv not found — law_lookup unavailable.")
    NULL
  }
)

# Ensure YEAR column is available (pipeline may name it DATA_YEAR)
if (!"YEAR" %in% names(full) && "DATA_YEAR" %in% names(full)) {
  full <- full |> rename(YEAR = DATA_YEAR)
}

message(sprintf("  Loaded %d incident rows (full), %d state-year rows (sy)",
                nrow(full), nrow(sy)))


# ══════════════════════════════════════════════════════════════════════════════
# 1. MANN-KENDALL TREND TEST
#    Non-parametric test for monotonic trend in annual incident counts.
#    Uses full incident dataset, 1991–2024.
# ══════════════════════════════════════════════════════════════════════════════
message("1. Mann-Kendall trend test…")

yearly <- full |> count(YEAR, name = "INCIDENTS") |> arrange(YEAR)
x <- yearly$INCIDENTS
n <- length(x)

# Kendall's S statistic
s_val <- sum(sapply(seq_len(n - 1), function(i) {
  sum(sign(x[(i + 1):n] - x[i]))
}))

var_s  <- n * (n - 1) * (2 * n + 5) / 18
z_mk   <- ifelse(s_val > 0, (s_val - 1) / sqrt(var_s),
          ifelse(s_val < 0, (s_val + 1) / sqrt(var_s), 0))
p_mk   <- 2 * pnorm(-abs(z_mk))

# Sen's slope — median of all pairwise slopes
slopes     <- outer(seq_len(n), seq_len(n),
                    FUN = function(j, i) ifelse(j > i, (x[j] - x[i]) / (j - i), NA))
sens_slope <- median(slopes, na.rm = TRUE)

mk_lines <- c(
  sprintf("1. Mann-Kendall Trend Test (Annual LGBTQI+ Incidents, 1991–%d)",
          max(yearly$YEAR, na.rm = TRUE)),
  sprintf("   S statistic  : %d",       s_val),
  sprintf("   Z score      : %.4f",     z_mk),
  sprintf("   p-value      : %.6f  %s", p_mk,
          ifelse(p_mk < 0.001, "*** Significant at 0.001",
          ifelse(p_mk < 0.01,  "** Significant at 0.01",
          ifelse(p_mk < 0.05,  "* Significant at 0.05", "Not significant")))),
  sprintf("   Sen's slope  : %+.2f incidents/year", sens_slope),
  sprintf("   Interpretation: %s",
          ifelse(s_val > 0 & p_mk < 0.05,
                 "Statistically significant upward trend",
                 "No significant monotonic trend"))
)
message(paste(mk_lines, collapse = "\n"))
add_result(mk_lines)


# ══════════════════════════════════════════════════════════════════════════════
# 2. WELCH'S T-TEST — Hate crime law impact
#    Track 1: RATE_PER_100K_TOTAL, all years
#    Track 2: RATE_PER_100K_LGBT,  2012+ only
# ══════════════════════════════════════════════════════════════════════════════
message("\n2. Policy impact — Welch's t-test…")

run_ttest <- function(data, rate_col, label) {
  d <- data |>
    filter(!is.na(.data[[rate_col]]), is.finite(.data[[rate_col]])) |>
    rename(RATE = all_of(rate_col))

  if (is.null(law_lookup)) {
    message("  Skipping t-test (no law_lookup).")
    return(NULL)
  }

  d <- d |>
    select(-any_of(c("LAW_LEVEL", "HC_LAW_SO", "HC_LAW_GI"))) |>
    left_join(law_lookup, by = c("STATE_NAME", "DATA_YEAR")) |>
    filter(!is.na(LAW_LEVEL)) |>
    mutate(HAS_LAW = as.integer(LAW_LEVEL > 0))

  no_law  <- d |> filter(HAS_LAW == 0) |> pull(RATE)
  has_law <- d |> filter(HAS_LAW == 1) |> pull(RATE)

  if (length(no_law) < 2 || length(has_law) < 2) {
    message(sprintf("  Insufficient data for t-test (%s).", label))
    return(NULL)
  }

  tt        <- t.test(RATE ~ HAS_LAW, data = d, var.equal = FALSE)
  pooled_sd <- sqrt((var(no_law) + var(has_law)) / 2)
  cohens_d  <- (mean(no_law) - mean(has_law)) / pooled_sd

  law_medians <- d |>
    group_by(LAW_LEVEL) |>
    summarise(MEDIAN = median(RATE, na.rm = TRUE), .groups = "drop")

  lines <- c(
    sprintf("2. Policy Impact — Welch's t-Test: %s", label),
    sprintf("   No hate crime law  : n=%d,  mean=%.3f", length(no_law),  mean(no_law)),
    sprintf("   Has hate crime law : n=%d, mean=%.3f",  length(has_law), mean(has_law)),
    sprintf("   t-statistic : %.4f", tt$statistic),
    sprintf("   df          : %.1f", tt$parameter),
    sprintf("   p-value     : %.4f  %s", tt$p.value,
            ifelse(tt$p.value < 0.05, "* Significant", "Not significant")),
    sprintf("   Cohen's d   : %.3f (%s effect size)", cohens_d,
            ifelse(abs(cohens_d) > 0.8, "large",
            ifelse(abs(cohens_d) > 0.5, "medium", "small"))),
    "", "   Rate by law level (medians):",
    sprintf("     No law (0)               : %.3f",
            law_medians$MEDIAN[law_medians$LAW_LEVEL == 0]),
    sprintf("     Covers sexual orient. (1): %.3f",
            law_medians$MEDIAN[law_medians$LAW_LEVEL == 1]),
    sprintf("     Full protection (2)      : %.3f",
            law_medians$MEDIAN[law_medians$LAW_LEVEL == 2])
  )
  message(paste(lines, collapse = "\n"))
  add_result(lines)
  list(data = d, law_medians = law_medians)
}

# Track 1 (all years)
tt1 <- run_ttest(sy, "RATE_PER_100K_TOTAL",
                 "Incidents per 100k total population (Track 1, 1991–2024)")

# Track 2 (2012+ only)
sy_t2 <- sy |> filter(DATA_YEAR >= 2012)
tt2 <- run_ttest(sy_t2, "RATE_PER_100K_LGBT",
                 "Incidents per 100k LGBT adults (Track 2, 2012–2024)")

# ── Chart 13: side-by-side law comparison (both tracks) ──────────────────────
make_law_bar <- function(result, rate_col, title, subtitle) {
  if (is.null(result)) return(NULL)
  d <- result$data |>
    mutate(LAW_LABEL = factor(
      case_when(LAW_LEVEL == 0 ~ "No Law",
                LAW_LEVEL == 1 ~ "Sexual\nOrientation Only",
                LAW_LEVEL == 2 ~ "Full Protection\n(incl. Gender ID)"),
      levels = c("No Law", "Sexual\nOrientation Only",
                 "Full Protection\n(incl. Gender ID)")
    )) |>
    group_by(LAW_LABEL) |>
    summarise(MEDIAN = median(RATE, na.rm = TRUE),
              SD     = sd(RATE, na.rm = TRUE),
              .groups = "drop")

  ggplot(d, aes(x = LAW_LABEL, y = MEDIAN, fill = LAW_LABEL)) +
    geom_col(width = 0.55, alpha = 0.85) +
    geom_errorbar(aes(ymin = pmax(0, MEDIAN - SD), ymax = MEDIAN + SD),
                  width = 0.15, linewidth = 0.8, colour = "#555555") +
    geom_text(aes(label = number(MEDIAN, accuracy = 0.01)),
              vjust = -0.7, size = 3.8, fontface = "bold") +
    scale_fill_manual(
      values = c("No Law" = "#B71C1C",
                 "Sexual\nOrientation Only" = "#EF9A9A",
                 "Full Protection\n(incl. Gender ID)" = "#1B5E20"),
      guide = "none"
    ) +
    scale_y_continuous(labels = number_format(accuracy = 0.1),
                       expand = expansion(mult = c(0, 0.18))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Rate per 100k") +
    theme_hate()
}

p13a <- make_law_bar(
  tt1, "RATE_PER_100K_TOTAL",
  "Track 1 — Total Population",
  "Per 100k total pop., all years"
)

p13b <- make_law_bar(
  tt2, "RATE_PER_100K_LGBT",
  "Track 2 — LGBT Population",
  "Per 100k LGBT adults, 2012–2024"
)

if (!is.null(p13a) && !is.null(p13b)) {
  p13 <- p13a + p13b +
    plot_annotation(
      title    = "Median LGBTQI+ Hate Crime Rate by State Law Coverage",
      subtitle = "Error bars = ±1 SD",
      theme    = theme(plot.title    = element_text(face = "bold", size = 13),
                       plot.subtitle = element_text(size = 10, colour = "#555555"))
    )
  save_chart(p13, "13_law_comparison_bar.png", width = 12, height = 5)
} else if (!is.null(p13a)) {
  save_chart(p13a, "13_law_comparison_bar.png", width = 8, height = 5)
}


# ══════════════════════════════════════════════════════════════════════════════
# 3. SUBGROUP GROWTH RATES (1991 → latest year)
# ══════════════════════════════════════════════════════════════════════════════
message("\n3. Subgroup growth rates…")

main_cats <- c("Anti-Gay (Male)", "Anti-Lesbian", "Anti-LGBTQ (General)",
               "Anti-Transgender", "Anti-Bisexual")

yr_min <- min(full$YEAR, na.rm = TRUE)
yr_max <- max(full$YEAR, na.rm = TRUE)

cat_ends <- full |>
  filter(BIAS_CATEGORY %in% main_cats) |>
  count(YEAR, BIAS_CATEGORY, name = "INCIDENTS") |>
  filter(YEAR %in% c(yr_min, yr_max)) |>
  pivot_wider(names_from = YEAR, values_from = INCIDENTS, values_fill = 0) |>
  rename(FIRST = 2, LAST = 3) |>
  mutate(
    FIRST        = pmax(FIRST, 1),
    TOTAL_GROWTH = round((LAST - FIRST) / FIRST * 100, 1),
    YEARS_SPAN   = yr_max - yr_min,
    CAGR         = round(((LAST / FIRST)^(1 / YEARS_SPAN) - 1) * 100, 2)
  )

growth_lines <- c(
  sprintf("3. Subgroup Growth Rates (%d → %d)", yr_min, yr_max),
  capture.output(print(cat_ends |> select(BIAS_CATEGORY, FIRST, LAST,
                                          TOTAL_GROWTH, CAGR)))
)
message(paste(growth_lines, collapse = "\n"))
add_result(growth_lines)

# Chart 14
p14 <- ggplot(cat_ends,
              aes(x = fct_reorder(BIAS_CATEGORY, TOTAL_GROWTH),
                  y = TOTAL_GROWTH,
                  fill = TOTAL_GROWTH > 0)) +
  geom_col(width = 0.65, alpha = 0.85) +
  geom_text(aes(label = sprintf("%+.1f%%", TOTAL_GROWTH),
                hjust = ifelse(TOTAL_GROWTH >= 0, -0.15, 1.15)),
            size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#1565C0", "FALSE" = "#B71C1C"),
                    guide  = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.15, 0.2))) +
  labs(title    = "Total Growth in LGBTQI+ Hate Crime Incidents by Subgroup",
       subtitle = sprintf("%d → %d", yr_min, yr_max),
       x = NULL, y = "% Change") +
  theme_hate()

save_chart(p14, "14_subgroup_growth_rates.png", width = 9, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# 4. SPEARMAN CORRELATION MATRIX
#    Two versions:
#      a) Track 1 — RATE_PER_100K_TOTAL, all years
#      b) Track 2 — RATE_PER_100K_LGBT,  2012+ only
# ══════════════════════════════════════════════════════════════════════════════
message("\n4. Spearman correlation matrix…")

build_corr_data <- function(data, rate_col) {
  base <- data |>
    select(STATE_NAME, DATA_YEAR,
           INCIDENTS, GINI_COEFFICIENT,
           TOTAL_POPULATION, all_of(rate_col)) |>
    rename(RATE = all_of(rate_col))

  if (!is.null(law_lookup)) {
    base <- base |> left_join(law_lookup, by = c("STATE_NAME", "DATA_YEAR"))
  } else {
    base <- base |> mutate(LAW_LEVEL = NA_integer_)
  }

  base |>
    mutate(LOG_TOTAL_POP = log1p(TOTAL_POPULATION)) |>
    select(RATE, GINI_COEFFICIENT, LOG_TOTAL_POP, LAW_LEVEL) |>
    drop_na()
}

corr_t1 <- build_corr_data(sy, "RATE_PER_100K_TOTAL")
corr_t2 <- build_corr_data(sy |> filter(DATA_YEAR >= 2012), "RATE_PER_100K_LGBT")

plot_corr_matrix <- function(data, subtitle) {
  corr_labels <- c("Incident Rate", "Gini Index", "Log Total Pop.", "Law Level")
  mat <- cor(data, method = "spearman")
  colnames(mat) <- corr_labels; rownames(mat) <- corr_labels

  mat |>
    as_tibble(rownames = "Var1") |>
    pivot_longer(-Var1, names_to = "Var2", values_to = "RHO") |>
    mutate(
      Var1 = factor(Var1, levels = corr_labels),
      Var2 = factor(Var2, levels = corr_labels)
    ) |>
    ggplot(aes(x = Var2, y = Var1, fill = RHO)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label  = sprintf("%.2f", RHO),
                  colour = abs(RHO) > 0.5),
              size = 3.2, fontface = "bold") +
    scale_fill_distiller(palette = "RdBu", direction = 1,
                         limits = c(-1, 1), name = "Spearman ρ") +
    scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "#333333"),
                        guide = "none") +
    scale_x_discrete(limits = rev(corr_labels)) +
    labs(subtitle = subtitle, x = NULL, y = NULL) +
    theme_hate() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          panel.grid  = element_blank())
}

p11a <- plot_corr_matrix(corr_t1, "Track 1 — per 100k total pop. (1991–2024)")
p11b <- plot_corr_matrix(corr_t2, "Track 2 — per 100k LGBT adults (2012–2024)")

p11 <- p11a + p11b +
  plot_annotation(
    title = "Spearman Rank Correlation — Key Variables (State × Year)",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )

save_chart(p11, "11_correlation_matrix.png", width = 13, height = 6)

corr_mat1 <- cor(corr_t1, method = "spearman")
corr_mat2 <- cor(corr_t2, method = "spearman")
corr_labels <- c("Incident Rate", "Gini Index", "Log Total Pop.", "Law Level")
colnames(corr_mat1) <- corr_labels; rownames(corr_mat1) <- corr_labels
colnames(corr_mat2) <- corr_labels; rownames(corr_mat2) <- corr_labels

corr_lines <- c(
  "4. Spearman Rank Correlations — Track 1 (1991–2024, total pop.):",
  capture.output(print(round(corr_mat1, 3))),
  "",
  "4. Spearman Rank Correlations — Track 2 (2012–2024, LGBT adults):",
  capture.output(print(round(corr_mat2, 3)))
)
add_result(corr_lines)


# ══════════════════════════════════════════════════════════════════════════════
# 5. OLS MULTIPLE REGRESSION
#    Primary   (Track 1): RATE_PER_100K_TOTAL ~ GINI + LOG_TOTAL_POP +
#                                               LAW_LEVEL + YEAR_C  (1991–2024)
#    Secondary (Track 2): RATE_PER_100K_LGBT  ~ GINI + LOG_TOTAL_POP +
#                                               LAW_LEVEL + YEAR_C  (2012–2024)
# ══════════════════════════════════════════════════════════════════════════════
message("\n5. OLS regression…")

build_reg_data <- function(data, rate_col) {
  base <- data |>
    select(STATE_NAME, DATA_YEAR, GINI_COEFFICIENT,
           TOTAL_POPULATION, all_of(rate_col)) |>
    rename(RATE = all_of(rate_col)) |>
    filter(!is.na(RATE), is.finite(RATE))

  if (!is.null(law_lookup)) {
    base <- base |> left_join(law_lookup, by = c("STATE_NAME", "DATA_YEAR"))
  } else {
    base <- base |> mutate(LAW_LEVEL = 0L)
  }

  base |>
    mutate(
      LOG_TOTAL_POP = log1p(TOTAL_POPULATION),
      YEAR_C        = DATA_YEAR - mean(DATA_YEAR, na.rm = TRUE)
    ) |>
    drop_na(RATE, GINI_COEFFICIENT, LOG_TOTAL_POP, LAW_LEVEL)
}

run_ols <- function(data, label) {
  m     <- lm(RATE ~ GINI_COEFFICIENT + LOG_TOTAL_POP + LAW_LEVEL + YEAR_C,
              data = data)
  tidy_ <- tidy(m)
  gl_   <- glance(m)

  lines <- c(
    sprintf("5. OLS Regression — %s", label),
    sprintf("   R²      : %.4f", gl_$r.squared),
    sprintf("   Adj. R² : %.4f", gl_$adj.r.squared),
    sprintf("   n       : %d",   nobs(m)),
    "",
    capture.output(
      tidy_ |>
        mutate(sig = case_when(p.value < 0.001 ~ "***",
                               p.value < 0.01  ~ "**",
                               p.value < 0.05  ~ "*",
                               TRUE            ~ "")) |>
        print(n = Inf)
    )
  )
  message(paste(lines, collapse = "\n"))
  add_result(lines)
  m
}

reg_t1 <- build_reg_data(sy, "RATE_PER_100K_TOTAL")
reg_t2 <- build_reg_data(sy |> filter(DATA_YEAR >= 2012), "RATE_PER_100K_LGBT")

ols_t1 <- run_ols(reg_t1, "Track 1 — per 100k total pop. (1991–2024)")
ols_t2 <- run_ols(reg_t2, "Track 2 — per 100k LGBT adults (2012–2024)")

# Chart 12 — Regression diagnostics (Track 1 primary model)
reg_diag <- augment(ols_t1)

p12a <- ggplot(reg_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.3, colour = "#1565C0", size = 1.5) +
  geom_hline(yintercept = 0, colour = "#B71C1C",
             linewidth = 1, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, colour = "#FF6D00",
              linewidth = 0.9) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(title = "Residuals vs. Fitted", x = "Fitted values", y = "Residuals") +
  theme_hate()

p12b <- ggplot(reg_diag, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.3, colour = "#1565C0", size = 1.5) +
  stat_qq_line(colour = "#B71C1C", linewidth = 1, linetype = "dashed") +
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles", y = "Standardised Residuals") +
  theme_hate()

p12 <- p12a + p12b +
  plot_annotation(
    title    = "OLS Regression Diagnostics — Track 1 (per 100k total pop.)",
    subtitle = "1991–2024",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13),
                     plot.subtitle = element_text(size = 10, colour = "#555555"))
  )
save_chart(p12, "12_regression_diagnostics.png", width = 12, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# 6. CHI-SQUARE TEST — Season × Violent/Non-violent
# ══════════════════════════════════════════════════════════════════════════════
message("\n6. Chi-square: season vs. offense type…")

season_tbl <- full |>
  filter(SEASON %in% c("Spring","Summer","Fall","Winter")) |>
  mutate(
    SEASON      = factor(SEASON, levels = c("Spring","Summer","Fall","Winter")),
    VIOLENT_LBL = if_else(IS_VIOLENT == 1, "Violent", "Non-violent")
  ) |>
  count(SEASON, VIOLENT_LBL) |>
  pivot_wider(names_from = VIOLENT_LBL, values_from = n, values_fill = 0L)

ct_matrix <- season_tbl |> select(-SEASON) |> as.matrix()
rownames(ct_matrix) <- season_tbl$SEASON

chi_test <- chisq.test(ct_matrix)

chi_lines <- c(
  "6. Chi-Square Test — Season × Violent/Non-violent Offense",
  sprintf("   χ² = %.4f  |  df = %d  |  p = %.6f",
          chi_test$statistic, chi_test$parameter, chi_test$p.value),
  sprintf("   %s", ifelse(chi_test$p.value < 0.05,
                          "Significant seasonal variation in violence pattern",
                          "No significant seasonal variation")),
  "", "   Observed counts:",
  capture.output(print(ct_matrix))
)
message(paste(chi_lines, collapse = "\n"))
add_result(chi_lines)


# ── Write report ───────────────────────────────────────────────────────────────
report_path <- file.path(REPORTS_DIR, "statistical_findings.txt")
writeLines(report_lines, report_path)

message(sprintf("\n✓ Statistical analysis complete."))
message(sprintf("  Report  → %s", report_path))
message("  Charts  → 11_correlation_matrix.png, 12_regression_diagnostics.png,")
message("             13_law_comparison_bar.png, 14_subgroup_growth_rates.png")
