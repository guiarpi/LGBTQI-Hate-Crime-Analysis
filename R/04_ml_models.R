# =============================================================================
# LGBTQI+ Hate Crime Analysis — Machine Learning Models
# =============================================================================
# Author : Guilherme Arpi
#
# Two-track normalisation strategy (consistent with 02_eda.R / 03_statistical_analysis.R)
#   Track 1 — RATE_PER_100K_TOTAL : per 100k total population (all years)
#   Track 2 — RATE_PER_100K_LGBT  : per 100k LGBT adults (2012+ only)
#
#   K-Means clustering uses Track 1 (RATE_PER_100K_TOTAL) so all years are included.
#   ML classification uses LOG_TOTAL_POP as the population feature (all years),
#   with LOG_LGBT_POP included as an optional secondary feature for 2012+ rows.
#
# Models (all via tidymodels framework):
#   1. K-Means clustering  — state-year risk profiles
#   2. Decision Tree       — predict violent vs. non-violent incident
#   3. Random Forest       — same prediction, ensemble approach
#   4. Permutation feature importance
#
# Required packages:
#   tidymodels, tidyverse, rpart, rpart.plot, randomForest,
#   cluster, factoextra, scales, patchwork
#
# NOTE: caret equivalents are shown in comments throughout.
# =============================================================================

# install packages if not already installed
# install.packages(c("tidymodels", "rpart", "rpart.plot", "randomForest", "cluster", "factoextra"))

# load packages if not already loaded
library(tidyverse)
library(tidymodels) #load tidyverse and tidymodels
library(rpart) #load rpart
library(rpart.plot) #load rpart.plot
library(randomForest) #load randomForest
library(cluster) #load cluster
library(factoextra) #load factoextra
library(scales) #load scales
library(patchwork) #load patchwork

# ── Paths ──────────────────────────────────────────────────────────────────────
#BASE_DIR    <- dirname(rstudioapi::getActiveDocumentContext()$path)
BASE_DIR <- if (basename(getwd()) == "R") dirname(getwd()) else getwd()  # works from repo root or from R/

DATA_DIR    <- file.path(BASE_DIR, "data")
CHARTS_DIR  <- file.path(BASE_DIR, "charts")
REPORTS_DIR <- file.path(BASE_DIR, "reports")

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

report_lines <- c(strrep("=", 72),
                  "LGBTQI+ HATE CRIME ANALYSIS — MACHINE LEARNING RESULTS",
                  strrep("=", 72), "")
add_result <- function(...) {
  report_lines <<- c(report_lines, ..., strrep("-", 72), "")
}


# ── Load & prepare ─────────────────────────────────────────────────────────────
message("Loading data…")

# State-year aggregate (pre-computed two-track rates from pipeline)
sy <- read_csv(file.path(DATA_DIR, "state_year_aggregate.csv"),
               show_col_types = FALSE) |>
  rename_with(str_to_upper) |>
  mutate(
    LOG_TOTAL_POP = log1p(TOTAL_POPULATION),
    LOG_LGBT_POP  = log1p(coalesce(LGBT_POPULATION_EST, NA_real_)),  # NA before 2012
    VIOLENT_PCT   = coalesce(VIOLENT_COUNT / INCIDENTS * 100, 0)
  )

# Time-varying law panel — authoritative LAW_LEVEL per state × year
# Source: state_law_panel.csv (built by build_law_panel.py, MAP data)
law_panel_ml <- tryCatch(
  read_csv(file.path(DATA_DIR, "state_law_panel.csv"), show_col_types = FALSE) |>
    rename(STATE_NAME = State, DATA_YEAR = Year) |>
    select(STATE_NAME, DATA_YEAR, LAW_LEVEL, HC_LAW_SO, HC_LAW_GI),
  error = function(e) {
    message("  Warning: state_law_panel.csv not found — LAW_LEVEL will default to 0.")
    NULL
  }
)

# Incident-level enriched data for ML classification (2010–2024)
# Drop any stale text-derived LAW_LEVEL from enriched CSV; replace with panel join
enriched_base <- read_csv(file.path(DATA_DIR, "lgbtq_hate_crimes_enriched.csv"),
                           show_col_types = FALSE) |>
  rename_with(str_to_upper) |>
  mutate(DATA_YEAR = as.integer(DATA_YEAR)) |>
  select(-any_of(c("LAW_LEVEL", "HC_LAW_SO", "HC_LAW_GI")))

enriched_joined <- if (!is.null(law_panel_ml)) {
  left_join(enriched_base, law_panel_ml, by = c("STATE_NAME", "DATA_YEAR"))
} else {
  enriched_base
}

enriched <- enriched_joined |>
  mutate(
    LAW_LEVEL      = coalesce(as.integer(LAW_LEVEL), 0L),
    IS_VIOLENT     = as.integer(str_detect(
                       OFFENSE_NAME, "Assault|Murder|Robbery|Rape|Arson")),
    GINI           = as.numeric(GINI_COEFFICIENT),
    TOTAL_POP      = as.numeric(TOTAL_POPULATION),
    LOG_TOTAL_POP  = log1p(as.numeric(TOTAL_POPULATION)),
    # LGBT pop available 2012+ only; log-transform for use as optional feature
    LGBT_POP       = as.numeric(coalesce(as.character(LGBT_POPULATION), NA_character_)),
    LOG_LGBT_POP   = log1p(coalesce(as.numeric(LGBT_POPULATION), NA_real_))
  ) |>
  filter(!is.na(TOTAL_POP), TOTAL_POP > 0)

message(sprintf("  Enriched: %s rows (%d–%d)",
                format(nrow(enriched), big.mark = ","),
                min(as.integer(enriched$DATA_YEAR), na.rm = TRUE),
                max(as.integer(enriched$DATA_YEAR), na.rm = TRUE)))


# ══════════════════════════════════════════════════════════════════════════════
# 1. K-MEANS CLUSTERING (k = 4)
#    Cluster states by risk profile: rate, Gini, law level, violent %
# ══════════════════════════════════════════════════════════════════════════════
message("\n1. K-Means clustering…")

# Use Track 1 rate (total pop) so all years 1991–2024 are available for clustering
cluster_feats <- c("RATE_PER_100K_TOTAL", "GINI_COEFFICIENT", "VIOLENT_PCT")
X_cl <- sy |> select(all_of(cluster_feats)) |> drop_na() |> scale()

set.seed(42)
km <- kmeans(X_cl, centers = 4, nstart = 25, iter.max = 300)
sy_cl <- sy |>
  drop_na(all_of(cluster_feats)) |>
  mutate(CLUSTER = factor(km$cluster))

# Cluster profiles
profiles <- sy_cl |>
  group_by(CLUSTER) |>
  summarise(across(all_of(cluster_feats), \(x) mean(x, na.rm = TRUE),
                   .names = "MEAN_{.col}"),
            N = n())
message("Cluster profiles:\n"); print(profiles)
add_result("1. K-Means Cluster Profiles (k=4):",
           capture.output(print(profiles)))

# Elbow plot (WSS)
wss_vals <- map_dbl(1:8, ~{
  kmeans(X_cl, centers = .x, nstart = 10)$tot.withinss
})

p_elbow <- ggplot(tibble(K = 1:8, WSS = wss_vals),
                  aes(x = K, y = WSS)) +
  geom_line(colour = "#1565C0", linewidth = 1.2) +
  geom_point(colour = "#1565C0", size = 3) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(labels = comma) +
  labs(title    = "K-Means: Elbow Method for Optimal k",
       subtitle = "Total within-cluster sum of squares",
       x = "Number of Clusters (k)", y = "Total WSS") +
  theme_hate()

# Cluster scatter
CLUSTER_COLORS <- c("1"="#1565C0","2"="#B71C1C","3"="#2E7D32","4"="#F57F17")

p_cl1 <- ggplot(sy_cl, aes(x = GINI_COEFFICIENT, y = RATE_PER_100K_TOTAL,
                            colour = CLUSTER)) +
  geom_point(alpha = 0.55, size = 2.2) +
  scale_colour_manual(values = CLUSTER_COLORS, name = "Cluster") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(title = "Clusters: Gini vs. Rate per 100k",
       subtitle = "Track 1 — per 100k total population",
       x = "Gini Coefficient", y = "Incidents per 100k total pop.") +
  theme_hate()

p_cl2 <- ggplot(sy_cl,
                aes(x = GINI_COEFFICIENT, y = VIOLENT_PCT, colour = CLUSTER)) +
  geom_point(alpha = 0.55, size = 2.2) +
  scale_colour_manual(values = CLUSTER_COLORS, name = "Cluster") +
  labs(title = "Clusters: Gini vs. % Violent",
       x = "Gini Coefficient", y = "% Violent Incidents") +
  theme_hate()

p15 <- (p_elbow | p_cl1 | p_cl2) +
  plot_annotation(
    title    = "K-Means State Risk Clustering (k=4)",
    subtitle = "Features: Track 1 rate (per 100k total pop.), Gini coefficient, % violent",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13),
                     plot.subtitle = element_text(size = 10, colour = "#555555"))
  )
save_chart(p15, "15_kmeans_clusters.png", width = 15, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# 2. DECISION TREE & RANDOM FOREST via tidymodels
#    Target: IS_VIOLENT (violent vs. non-violent incident)
#    Features: BIAS_CATEGORY, LAW_LEVEL, GINI, LOG_TOTAL_POP, DATA_YEAR
#    Note: LOG_TOTAL_POP (Track 1) used so all years 2010–2024 are included.
#          LOG_LGBT_POP (Track 2) available only 2012+ — excluded from primary
#          model to avoid dropping 2010–2011 rows.
# ══════════════════════════════════════════════════════════════════════════════
message("\n2. Preparing ML dataset…")

BIAS_CATS <- c("Anti-Gay (Male)", "Anti-Lesbian", "Anti-LGBTQ (General)",
               "Anti-Transgender", "Anti-Bisexual", "Anti-Gender Non-Conforming")

classify_bias <- function(b) {
  b <- str_to_lower(b)
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

ml_data <- enriched |>
  mutate(
    BIAS_CATEGORY = classify_bias(BIAS_DESC),
    IS_VIOLENT    = factor(IS_VIOLENT, levels = c(0, 1),
                           labels = c("Non-violent", "Violent")),
    DATA_YEAR     = as.numeric(DATA_YEAR)
  ) |>
  select(IS_VIOLENT, BIAS_CATEGORY, LAW_LEVEL,
         GINI, LOG_TOTAL_POP, DATA_YEAR) |>
  drop_na()

message(sprintf("  ML dataset: %s rows", format(nrow(ml_data), big.mark = ",")))

# ── tidymodels: split + recipe ─────────────────────────────────────────────────
set.seed(99)
splits   <- initial_split(ml_data, prop = 0.80, strata = IS_VIOLENT)
df_train <- training(splits)
df_test  <- testing(splits)

rec <- recipe(IS_VIOLENT ~ ., data = df_train) |>
  step_string2factor(BIAS_CATEGORY) |>
  step_novel(BIAS_CATEGORY) |>
  step_dummy(BIAS_CATEGORY, one_hot = FALSE) |>
  step_normalize(GINI, LOG_TOTAL_POP, DATA_YEAR)

# NOTE (caret equivalent):
#   ctrl <- trainControl(method = "cv", number = 5)
#   model <- train(IS_VIOLENT ~ ., data = df_train,
#                  method = "rpart", trControl = ctrl,
#                  preProcess = c("center","scale"))


# ══════════════════════════════════════════════════════════════════════════════
# Decision Tree
# ══════════════════════════════════════════════════════════════════════════════
message("3. Decision Tree…")

dt_spec <- decision_tree(
  mode       = "classification",
  cost_complexity = tune(),
  tree_depth      = tune(),
  min_n           = 30
) |> set_engine("rpart")

dt_wf <- workflow() |>
  add_recipe(rec) |>
  add_model(dt_spec)

# Cross-validation for hyperparameter tuning
set.seed(42)
folds   <- vfold_cv(df_train, v = 5, strata = IS_VIOLENT)
dt_grid <- grid_regular(cost_complexity(), tree_depth(range = c(3, 8)),
                        levels = 4)

dt_res <- tune_grid(dt_wf, resamples = folds, grid = dt_grid,
                    metrics = metric_set(accuracy, roc_auc, f_meas))

best_dt   <- select_best(dt_res, metric = "f_meas")
final_dt  <- finalize_workflow(dt_wf, best_dt)
dt_fitted <- fit(final_dt, data = df_train)

dt_preds  <- predict(dt_fitted, df_test) |>
  bind_cols(predict(dt_fitted, df_test, type = "prob")) |>
  bind_cols(df_test |> select(IS_VIOLENT))

dt_metrics <- dt_preds |>
  metrics(truth = IS_VIOLENT, estimate = .pred_class,
          .pred_Violent) |>
  filter(.metric %in% c("accuracy", "roc_auc"))

dt_cm <- dt_preds |>
  conf_mat(truth = IS_VIOLENT, estimate = .pred_class)

dt_lines <- c(
  "2. Decision Tree (tuned via 5-fold CV)",
  sprintf("   Best cost_complexity : %.6f", best_dt$cost_complexity),
  sprintf("   Best tree_depth      : %d",   best_dt$tree_depth),
  capture.output(print(dt_metrics)),
  "", "   Confusion Matrix:", capture.output(print(dt_cm))
)
message(paste(dt_lines, collapse = "\n"))
add_result(dt_lines)


# ══════════════════════════════════════════════════════════════════════════════
# Random Forest (tuned)
# ══════════════════════════════════════════════════════════════════════════════
message("\n4. Random Forest…")

rf_spec <- rand_forest(
  mode   = "classification",
  trees  = 200,
  mtry   = tune(),
  min_n  = tune()
) |> set_engine("randomForest", importance = TRUE)

rf_wf <- workflow() |>
  add_recipe(rec) |>
  add_model(rf_spec)

rf_grid <- grid_regular(
  mtry(range = c(2L, 4L)),
  min_n(range = c(20L, 50L)),
  levels = 3
)

rf_res <- tune_grid(rf_wf, resamples = folds, grid = rf_grid,
                    metrics = metric_set(accuracy, roc_auc, f_meas))

best_rf   <- select_best(rf_res, metric = "f_meas")
final_rf  <- finalize_workflow(rf_wf, best_rf)
rf_fitted <- fit(final_rf, data = df_train)

rf_preds  <- predict(rf_fitted, df_test) |>
  bind_cols(predict(rf_fitted, df_test, type = "prob")) |>
  bind_cols(df_test |> select(IS_VIOLENT))

rf_metrics <- rf_preds |>
  metrics(truth = IS_VIOLENT, estimate = .pred_class,
          .pred_Violent) |>
  filter(.metric %in% c("accuracy", "roc_auc"))

rf_cm <- rf_preds |>
  conf_mat(truth = IS_VIOLENT, estimate = .pred_class)

rf_lines <- c(
  "3. Random Forest (200 trees, tuned via 5-fold CV)",
  sprintf("   Best mtry  : %d",  best_rf$mtry),
  sprintf("   Best min_n : %d",  best_rf$min_n),
  capture.output(print(rf_metrics)),
  "", "   Confusion Matrix:", capture.output(print(rf_cm))
)
message(paste(rf_lines, collapse = "\n"))
add_result(rf_lines)


# ══════════════════════════════════════════════════════════════════════════════
# Feature Importance
# ══════════════════════════════════════════════════════════════════════════════
message("\n5. Feature importance (VIP)…")

rf_engine <- extract_fit_engine(rf_fitted)
imp_raw   <- importance(rf_engine, type = 1)   # MeanDecreaseAccuracy

imp_df <- tibble(
  Feature    = rownames(imp_raw),
  Importance = imp_raw[, 1]
) |>
  # Clean up dummy-encoded names
  mutate(Feature = str_replace_all(Feature, "_X\\.","_") |>
           str_replace_all("BIAS_CATEGORY_", "") |>
           str_replace_all("\\.", " ") |>
           str_to_title()) |>
  arrange(desc(Importance))

add_result("4. Random Forest Feature Importance (Mean Decrease Accuracy):",
           capture.output(print(imp_df)))

p17 <- ggplot(imp_df |> slice_max(Importance, n = 10) |>
                mutate(Feature = fct_reorder(Feature, Importance)),
              aes(x = Importance, y = Feature,
                  fill = Importance > 0)) +
  geom_col(alpha = 0.85, width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "#B71C1C", "FALSE" = "#546E7A"),
                    guide = "none") +
  scale_x_continuous(labels = number_format(accuracy = 0.001),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Random Forest Feature Importance (Top 10)",
       subtitle = "Mean Decrease in Accuracy | LOG_TOTAL_POP = log(total state population)",
       x = "Mean Decrease Accuracy", y = NULL) +
  theme_hate()

save_chart(p17, "17_feature_importance.png", width = 9, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Chart 16 — Model comparison
# ══════════════════════════════════════════════════════════════════════════════
compare_df <- bind_rows(
  dt_preds |>
    metrics(truth = IS_VIOLENT, estimate = .pred_class) |>
    mutate(Model = "Decision Tree"),
  rf_preds |>
    metrics(truth = IS_VIOLENT, estimate = .pred_class) |>
    mutate(Model = "Random Forest (200 trees)")
) |>
  filter(.metric %in% c("accuracy", "kap")) |>
  select(Model, Metric = .metric, Value = .estimate)

# Add F1 for both
f1_dt <- f_meas(dt_preds, truth = IS_VIOLENT, estimate = .pred_class)$.estimate
f1_rf <- f_meas(rf_preds, truth = IS_VIOLENT, estimate = .pred_class)$.estimate

compare_df <- bind_rows(
  compare_df,
  tibble(Model = "Decision Tree",             Metric = "f1", Value = f1_dt),
  tibble(Model = "Random Forest (200 trees)", Metric = "f1", Value = f1_rf)
) |>
  mutate(Metric = factor(Metric, levels = c("accuracy","kap","f1"),
                         labels = c("Accuracy","Kappa","F1 Score")))

p16 <- ggplot(compare_df, aes(x = Metric, y = Value, fill = Model)) +
  geom_col(position = "dodge", width = 0.55, alpha = 0.85) +
  geom_text(aes(label = number(Value, accuracy = 0.001)),
            position = position_dodge(width = 0.55),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c("Decision Tree" = "#1565C0",
                                "Random Forest (200 trees)" = "#E65100"),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Model Performance — Predicting Violent vs. Non-Violent Incident",
       subtitle = "Test set (20% holdout), stratified split",
       x = NULL, y = "Score") +
  theme_hate() +
  theme(legend.position = "top")

save_chart(p16, "16_model_comparison.png", width = 9, height = 5)


# ── Write report ───────────────────────────────────────────────────────────────
report_path <- file.path(REPORTS_DIR, "ml_results.txt")
writeLines(report_lines, report_path)

message("\n✓ ML analysis complete.")
message(sprintf("  Report → %s", report_path))
message("  Charts → 15_kmeans_clusters.png, 16_model_comparison.png,")
message("             17_feature_importance.png")

# =============================================================================
# caret EQUIVALENTS (uncomment to use instead of tidymodels)
# =============================================================================
# library(caret)
#
# ctrl <- trainControl(method = "cv", number = 5,
#                      classProbs = TRUE, summaryFunction = twoClassSummary)
#
# # Decision tree
# dt_caret <- train(IS_VIOLENT ~ BIAS_CATEGORY + LAW_LEVEL + GINI +
#                     LOG_LGBT_POP + DATA_YEAR,
#                   data      = df_train,
#                   method    = "rpart",
#                   trControl = ctrl,
#                   metric    = "ROC",
#                   preProcess = c("center", "scale"))
# confusionMatrix(predict(dt_caret, df_test), df_test$IS_VIOLENT)
#
# # Random Forest
# rf_caret <- train(IS_VIOLENT ~ .,
#                   data      = df_train,
#                   method    = "rf",
#                   ntree     = 200,
#                   trControl = ctrl,
#                   metric    = "ROC")
# confusionMatrix(predict(rf_caret, df_test), df_test$IS_VIOLENT)
# varImp(rf_caret)
