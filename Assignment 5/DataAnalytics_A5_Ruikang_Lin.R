# Assignment 5
# Ruikang Lin
# Used Claude for cleaning documentation, adding pretty prints, and making better charts

# ============================================================================
# NYC Citywide Annualized Sales – Regression & Classification Analysis
# Borough: Manhattan
# ============================================================================

# Writeup for the parts that needs sentences:

# 1b.
# The regression-ready dataset needed a lot of filtering. Records with SALE PRICE ≤ $10,000
# were removed as non-arm's-length transfers. Records with SALE PRICE > $100M were removed
# as extreme outliers that distort predictions. Gross Square Feet and Land Square Feet were
# converted from string to numeric which removes the commas, and rows with values ≤ 0 or
# > 50,000 sq ft were dropped. Total Units were capped at 500. After this cleaning, the usable
# dataset shrank from 12,880 to 539 rows.

# 2a.
# I used an 80/20 stratified train-test split and this yielded 36 test observations. As for
# data cleaning, I used the same cleaned regression dataset and features were standardized with
# StandardScaler before fitting distance-based models (kNN, SVM). Missing values in the six
# quantitative features were dropped, resulting in 176 usable rows. Random Forest performed best
# with 75% accuracy and a weighted F1 of 0.634.

# 3.
# Dataset Quality: 
#   The NYC Citywide Annualized Sales dataset has a lot of quality limitations for predictive
#   modeling. The most annoying issue was the non-arm's-length transfers recorded with a sale
#   price of $0 or $1. These are legal deed transfers or intra-family sales that look like real
#   transactions but carry no market value. After filtering for meaningful prices and valid square
#   footage, the Manhattan subset went from 12,880 to just 539 rows. Also, the LAND SQUARE FEET
#   and GROSS SQUARE FEET columns were stored as comma-formatted strings, requiring parsing, and 
#   contained large numbers of zeros and nulls, especially for condos.
# Regression: 
#   All regression models performed poorly (best R² ≈ 0.21), confirming that the available quantitative
#   features such as size, unit count, building age are not enough alone to predict Manhattan
#   sale prices reliably. Manhattan real estate prices are driven by specific qualitative factors
#   like floor number, views, renovation quality, proximity to transit, and prestige of building,
#   none of which are present in this dataset. The small post-cleaning sample size (539) also limits
#   the complexity of models that can be reliably trained. Ensemble methods (RF, GBM) did not
#   outperform linear regression, likely because the feature-to-sample ratio was too low for 
#   tree-based methods to find meaningful splits.
# Classification: 
#   The classification performed considerably better, with Random Forest reaching about 75% accuracy. 
#   This is partly because Harlem-Central, the largest class, is economically distinct enough (lower
#   prices, different building sizes) to be reliably separated. The smaller classes (Chelsea, UWS)
#   were frequently misclassified becuase of overlapping feature distributions and very few training
#   examples (as few as 16 per class in training). Future work could benefit from adding additional
#   derived features like price per square foot, building class category, using oversampling (SMOTE)
#   to address class imbalance, and including a better set of observations from the numeric borough-1
#   entries that were excluded from this analysis.


# ── 0. Libraries ─────────────────────────────────────────────────────────────
required_packages <- c(
  "tidyverse", "ggplot2", "gridExtra", "corrplot", "scales",
  "caret", "randomForest", "gbm", "e1071", "class",
  "reshape2", "glmnet", "yardstick"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg, repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(scales)
library(caret)
library(randomForest)
library(gbm)
library(e1071)
library(class)
library(reshape2)
library(glmnet)

# ── Colour palette ────────────────────────────────────────────────────────────
BLUE   <- "#2563EB"
RED    <- "#DC2626"
GREEN  <- "#16A34A"
ORANGE <- "#EA580C"
PURPLE <- "#7C3AED"
GRAY   <- "#6B7280"
BG     <- "#F9FAFB"
PALETTE <- c(BLUE, RED, GREEN, ORANGE, PURPLE)

theme_custom <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = BG, colour = NA),
    panel.background = element_rect(fill = BG, colour = NA),
    panel.grid.major = element_line(colour = "#E5E7EB", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 12),
    axis.title       = element_text(size = 9),
    axis.text        = element_text(size = 8)
  )

# ── 1. Load Data ──────────────────────────────────────────────────────────────
setwd("~/Documents/GitHub/ITWS4600/Assignment 5")
DATA_PATH <- "NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv"

df_raw <- read_csv(DATA_PATH, show_col_types = FALSE)

# Keep Manhattan (BOROUGH == "MANHATTAN")
man <- df_raw %>%
  filter(BOROUGH == "MANHATTAN") %>%
  mutate(
    `SALE PRICE`        = as.numeric(`SALE PRICE`),
    `GROSS SQUARE FEET` = as.numeric(gsub(",", "", `GROSS SQUARE FEET`)),
    `LAND SQUARE FEET`  = as.numeric(gsub(",", "", `LAND SQUARE FEET`)),
    `YEAR BUILT`        = as.numeric(`YEAR BUILT`),
    `TOTAL UNITS`       = as.numeric(`TOTAL UNITS`),
    `RESIDENTIAL UNITS` = as.numeric(`RESIDENTIAL UNITS`)
  )

cat("Raw Manhattan rows:", nrow(man), "\n")
cat("Sale Price summary:\n"); print(summary(man$`SALE PRICE`))


# ============================================================================
# FIGURE 1 – EDA: Variable Distributions
# ============================================================================
plot_hist <- function(data, col, fill_col, title,
                      clip_low = NULL, clip_high = NULL, clip_pct = 0.95,
                      x_label_fmt = comma) {
  d <- data[[col]]
  d <- d[!is.na(d)]
  
  # Apply lower clip
  lo <- if (!is.null(clip_low))  clip_low  else min(d)
  # Apply upper clip (explicit value takes priority over quantile)
  hi <- if (!is.null(clip_high)) clip_high else quantile(d[d >= lo], clip_pct)
  
  d <- d[d >= lo & d <= hi]
  med <- median(d, na.rm = TRUE)
  
  ggplot(data.frame(x = d), aes(x = x)) +
    geom_histogram(bins = 50, fill = fill_col, colour = "white", linewidth = 0.3, alpha = 0.85) +
    geom_vline(xintercept = med, linetype = "dashed", colour = "black", linewidth = 0.9) +
    annotate("text", x = med, y = Inf, label = paste0("Median: ", comma(round(med))),
             vjust = 2, hjust = -0.1, size = 3) +
    scale_x_continuous(labels = x_label_fmt) +
    labs(title = title, x = "Value", y = "Count",
         caption = sprintf("Clipped to [%s, %s] (%.0f%% ile)",
                           comma(round(lo)), comma(round(hi)), clip_pct * 100)) +
    theme_custom +
    theme(plot.caption = element_text(size = 7, colour = "gray50"))
}

p1 <- plot_hist(man, "SALE PRICE",        BLUE,   "Sale Price",
                clip_low = 10000, clip_pct = 0.95,
                x_label_fmt = label_dollar(scale = 1e-6, suffix = "M"))
p2 <- plot_hist(man, "GROSS SQUARE FEET", GREEN,  "Gross Square Feet",
                clip_low = 100,   clip_pct = 0.95)
p3 <- plot_hist(man, "LAND SQUARE FEET",  ORANGE, "Land Square Feet",
                clip_low = 50,    clip_pct = 0.95)
p4 <- plot_hist(man, "RESIDENTIAL UNITS", RED,    "Residential Units",
                clip_low = 1,     clip_pct = 0.99)
p5 <- plot_hist(man, "TOTAL UNITS",       PURPLE, "Total Units",
                clip_low = 1,     clip_pct = 0.99)
p6 <- plot_hist(man, "YEAR BUILT",        GRAY,   "Year Built",
                clip_low = 1800,  clip_high = 2024)

fig1 <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3,
                     top = grid::textGrob("Figure 1 – Manhattan EDA: Variable Distributions (Clipped to 95th Percentile)",
                                          gp = grid::gpar(fontface = "bold", fontsize = 14)))

# ============================================================================
# FIGURE 2 – Outlier Detection
# ============================================================================
price_valid <- man %>% filter(!is.na(`SALE PRICE`) & `SALE PRICE` > 0)
Q1  <- quantile(price_valid$`SALE PRICE`, 0.25)
Q3  <- quantile(price_valid$`SALE PRICE`, 0.75)
IQR <- Q3 - Q1
lo  <- Q1 - 1.5 * IQR
hi  <- Q3 + 1.5 * IQR
cat(sprintf("\nSale Price IQR bounds: [%s, %s]\n", comma(lo), comma(hi)))

tmp <- price_valid %>%
  mutate(outlier = `SALE PRICE` < lo | `SALE PRICE` > hi)
cat("Outlier count:", sum(tmp$outlier), "/", nrow(tmp), "\n")

# 2a: Box plot
p2a <- ggplot(tmp, aes(y = `SALE PRICE`)) +
  geom_boxplot(fill = paste0(BLUE, "55"), colour = BLUE,
               outlier.colour = RED, outlier.alpha = 0.4, outlier.size = 1.5) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Box Plot – Sale Price", y = "Sale Price", x = "") +
  theme_custom

# 2b: Scatter – price vs gross sqft coloured by outlier
tmp2 <- tmp %>% filter(!is.na(`GROSS SQUARE FEET`) & `GROSS SQUARE FEET` > 0)
p2b <- ggplot(tmp2, aes(x = `GROSS SQUARE FEET`, y = `SALE PRICE`, colour = outlier)) +
  geom_point(alpha = 0.35, size = 1) +
  scale_colour_manual(values = c("FALSE" = BLUE, "TRUE" = RED),
                      labels = c("Normal", "Outlier")) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  scale_x_continuous(labels = comma) +
  labs(title = "Sale Price vs Gross Sq Ft", colour = NULL,
       x = "Gross Square Feet", y = "Sale Price") +
  theme_custom + theme(legend.position = "top")

# 2c: Log-scale histogram
p2c <- ggplot(tmp, aes(x = `SALE PRICE`, fill = outlier)) +
  geom_histogram(bins = 60, colour = "white", linewidth = 0.2, alpha = 0.75, position = "identity") +
  scale_fill_manual(values = c("FALSE" = BLUE, "TRUE" = RED),
                    labels = c("Normal", "Outlier")) +
  scale_y_log10() +
  scale_x_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Sale Price Distribution (log y)", fill = NULL,
       x = "Sale Price", y = "Count (log scale)") +
  theme_custom + theme(legend.position = "top")

fig2 <- grid.arrange(p2a, p2b, p2c, ncol = 3,
                     top = grid::textGrob("Figure 2 – Sale Price Outlier Analysis",
                                          gp = grid::gpar(fontface = "bold", fontsize = 14)))

# ============================================================================
# FIGURE 3 – Correlation Matrix + Log-Log Scatter
# ============================================================================
num_cols <- c("SALE PRICE", "GROSS SQUARE FEET", "LAND SQUARE FEET",
              "RESIDENTIAL UNITS", "TOTAL UNITS", "YEAR BUILT")
clean_corr <- man %>%
  select(all_of(num_cols)) %>%
  filter(`SALE PRICE` > 0, `GROSS SQUARE FEET` > 0) %>%
  drop_na()

cor_mat <- cor(clean_corr)

par(mfrow = c(1, 2), bg = BG, mar = c(4, 4, 4, 2))

corrplot(cor_mat, method = "color", type = "lower", addCoef.col = "black",
         number.cex = 0.75, tl.cex = 0.75, col.lim = c(-1, 1),
         col = colorRampPalette(c(RED, "white", BLUE))(200),
         title = "Correlation Matrix", mar = c(0, 0, 2, 0))

valid_log <- clean_corr %>% filter(`SALE PRICE` > 1000, `GROSS SQUARE FEET` > 100)
lx <- log10(valid_log$`GROSS SQUARE FEET`)
ly <- log10(valid_log$`SALE PRICE`)
fit <- lm(ly ~ lx)
xs  <- seq(min(lx), max(lx), length.out = 100)

plot(valid_log$`GROSS SQUARE FEET`, valid_log$`SALE PRICE`,
     log = "xy", pch = 16, cex = 0.4, col = paste0(BLUE, "66"),
     xlab = "Gross Square Feet (log)", ylab = "Sale Price (log)",
     main = "Gross Sq Ft vs Sale Price (log-log)", bg = BG)
lines(10^xs, 10^(coef(fit)[1] + coef(fit)[2] * xs), col = RED, lwd = 2)
legend("topleft", legend = sprintf("Trend slope = %.2f", coef(fit)[2]),
       col = RED, lwd = 2, bty = "n", cex = 0.8)


# ============================================================================
# REGRESSION – Data Cleaning & Feature Engineering
# ============================================================================
reg <- man %>%
  filter(
    `SALE PRICE`        > 10000,
    `SALE PRICE`        < 1e8,
    `GROSS SQUARE FEET` > 0,
    `GROSS SQUARE FEET` < 50000,
    `TOTAL UNITS`       > 0,
    `TOTAL UNITS`       < 500
  ) %>%
  drop_na(`SALE PRICE`, `GROSS SQUARE FEET`, `LAND SQUARE FEET`,
          `RESIDENTIAL UNITS`, `TOTAL UNITS`, `YEAR BUILT`) %>%
  mutate(
    LOG_PRICE   = log1p(`SALE PRICE`),
    LOG_GROSS   = log1p(`GROSS SQUARE FEET`),
    LOG_LAND    = log1p(`LAND SQUARE FEET`),
    BLDG_AGE    = pmin(2024 - `YEAR BUILT`, 300),
    TOTAL_UNITS = `TOTAL UNITS`,
    RESID_UNITS = `RESIDENTIAL UNITS`,
    TAX_CLASS   = `TAX CLASS AT TIME OF SALE`
  )

cat(sprintf("\nRegression dataset after cleaning: %d rows\n", nrow(reg)))

set.seed(42)
train_idx <- createDataPartition(reg$`SALE PRICE`, p = 0.8, list = FALSE)
train_r   <- reg[train_idx, ]
test_r    <- reg[-train_idx, ]

eval_reg <- function(actual, predicted, model_name) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  cat(sprintf("  %-50s RMSE=$%12s  MAE=$%10s  R²=%.4f\n",
              model_name, comma(round(rmse)), comma(round(mae)), r2))
  list(rmse = rmse, mae = mae, r2 = r2)
}


cat("\n── Regression Model Comparison ──\n")

FEAT_BASIC <- c("GROSS SQUARE FEET", "LAND SQUARE FEET", "TOTAL_UNITS", "RESID_UNITS")
FEAT_ENG   <- c("LOG_GROSS", "LOG_LAND", "TOTAL_UNITS", "RESID_UNITS", "BLDG_AGE")
FEAT_FULL  <- c("LOG_GROSS", "LOG_LAND", "TOTAL_UNITS", "RESID_UNITS", "BLDG_AGE", "TAX_CLASS")

# M1: Linear Regression (raw features)
m1 <- lm(`SALE PRICE` ~ ., data = train_r[, c(FEAT_BASIC, "SALE PRICE")])
r1 <- eval_reg(test_r$`SALE PRICE`, predict(m1, test_r), "M1: Linear Reg (raw features)")

# M2: Linear Regression (log features, log target)
m2 <- lm(LOG_PRICE ~ ., data = train_r[, c(FEAT_ENG, "LOG_PRICE")])
r2_res <- eval_reg(test_r$`SALE PRICE`, expm1(predict(m2, test_r)), "M2: Linear Reg (log features, log target)")

# M3: Ridge Regression (log features + tax class)
x_train_r <- as.matrix(train_r[, FEAT_FULL])
x_test_r  <- as.matrix(test_r[, FEAT_FULL])
m3 <- cv.glmnet(x_train_r, train_r$LOG_PRICE, alpha = 0)
r3 <- eval_reg(test_r$`SALE PRICE`, expm1(predict(m3, x_test_r, s = "lambda.min")),
               "M3: Ridge Reg (full features, log target)")

# M4: Random Forest (log features, log target)
m4 <- randomForest(as.formula(paste("LOG_PRICE ~", paste(FEAT_ENG, collapse = " + "))),
                   data = train_r, ntree = 150, maxnodes = 64, importance = TRUE)
r4 <- eval_reg(test_r$`SALE PRICE`, expm1(predict(m4, test_r)), "M4: Random Forest (log features, log target)")

# M5: Gradient Boosting (log features, log target)
m5 <- gbm(as.formula(paste("LOG_PRICE ~", paste(FEAT_ENG, collapse = " + "))),
          data = train_r, distribution = "gaussian",
          n.trees = 150, interaction.depth = 5, shrinkage = 0.05, verbose = FALSE)
r5 <- eval_reg(test_r$`SALE PRICE`, expm1(predict(m5, test_r, n.trees = 150)),
               "M5: Gradient Boosting (log features, log target)")


# ============================================================================
# FIGURE 4 – Regression Results
# ============================================================================
reg_summary <- tibble(
  Model = c("M1\nLinear\n(raw)", "M2\nLinear\n(log)", "M3\nRidge\n(log)",
            "M4\nRF\n(log)", "M5\nGBM\n(log)"),
  RMSE  = c(r1$rmse, r2_res$rmse, r3$rmse, r4$rmse, r5$rmse),
  R2    = c(r1$r2,   r2_res$r2,   r3$r2,   r4$r2,   r5$r2)
)

p4a <- ggplot(reg_summary, aes(x = Model, y = RMSE / 1e6, fill = Model)) +
  geom_col(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("$%.2fM", RMSE / 1e6)), vjust = -0.4, size = 3, fontface = "bold") +
  scale_fill_manual(values = PALETTE) +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(title = "RMSE by Model (lower = better)", y = "RMSE ($ millions)", x = NULL) +
  theme_custom + theme(legend.position = "none")

p4b <- ggplot(reg_summary, aes(x = Model, y = R2, fill = Model)) +
  geom_col(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.3f", R2)), vjust = -0.4, size = 3, fontface = "bold") +
  scale_fill_manual(values = PALETTE) +
  coord_cartesian(ylim = c(0, 0.35)) +
  labs(title = "R² Score by Model (higher = better)", y = "R²", x = NULL) +
  theme_custom + theme(legend.position = "none")

# Actual vs Predicted (RF)
rf_pred_actual <- tibble(
  actual    = test_r$`SALE PRICE` / 1e6,
  predicted = expm1(predict(m4, test_r)) / 1e6
)
max_lim <- max(rf_pred_actual)
p4c <- ggplot(rf_pred_actual, aes(x = actual, y = predicted)) +
  geom_point(colour = BLUE, alpha = 0.35, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, colour = RED, linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = dollar_format(suffix = "M")) +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(title = sprintf("Best Model (RF): Actual vs Predicted\nR²=%.3f", r4$r2),
       x = "Actual Sale Price ($ M)", y = "Predicted Sale Price ($ M)") +
  theme_custom

fig4 <- grid.arrange(p4a, p4b, p4c, ncol = 3,
                     top = grid::textGrob("Figure 4 – Regression Model Comparison & Diagnostics",
                                          gp = grid::gpar(fontface = "bold", fontsize = 14)))


# ============================================================================
# CLASSIFICATION – 4 Neighborhoods
# ============================================================================
NEIGH_4 <- c("UPPER EAST SIDE (59-79)", "UPPER WEST SIDE (59-79)", "HARLEM-CENTRAL", "CHELSEA")
CLF_FEATURES <- c("SALE PRICE", "GROSS SQUARE FEET", "LAND SQUARE FEET",
                  "TOTAL UNITS", "RESIDENTIAL UNITS", "BLDG_AGE")

clf_df <- reg %>%
  filter(NEIGHBORHOOD %in% NEIGH_4) %>%
  select(all_of(c(CLF_FEATURES, "NEIGHBORHOOD"))) %>%
  drop_na() %>%
  mutate(LABEL = factor(NEIGHBORHOOD))

cat(sprintf("\nClassification dataset: %d rows\n", nrow(clf_df)))
print(table(clf_df$NEIGHBORHOOD))

set.seed(42)
clf_idx   <- createDataPartition(clf_df$LABEL, p = 0.8, list = FALSE)
train_clf <- clf_df[clf_idx, ]
test_clf  <- clf_df[-clf_idx, ]

# Scale features
pre_proc <- preProcess(train_clf[, CLF_FEATURES], method = c("center", "scale"))
Xtr <- predict(pre_proc, train_clf[, CLF_FEATURES])
Xte <- predict(pre_proc, test_clf[, CLF_FEATURES])
ytr <- train_clf$LABEL
yte <- test_clf$LABEL

# kNN (k=7)
knn_pred <- knn(train = Xtr, test = Xte, cl = ytr, k = 7)
cm_knn   <- confusionMatrix(knn_pred, yte)
cat("\n── kNN Results ──\n"); print(cm_knn$overall)

# Random Forest
rf_clf  <- randomForest(x = Xtr, y = ytr, ntree = 200, maxnodes = 32, importance = TRUE)
rf_pred <- predict(rf_clf, Xte)
cm_rf   <- confusionMatrix(rf_pred, yte)
cat("\n── Random Forest Results ──\n"); print(cm_rf$overall)

# SVM (RBF)
svm_clf  <- svm(x = Xtr, y = ytr, kernel = "radial", cost = 5, gamma = 1/ncol(Xtr))
svm_pred <- predict(svm_clf, Xte)
cm_svm   <- confusionMatrix(svm_pred, yte)
cat("\n── SVM Results ──\n"); print(cm_svm$overall)


# ============================================================================
# FIGURE 5 – Confusion Matrices + Metrics Bar Chart
# ============================================================================
short_labels <- c("Chelsea", "Harlem\nCentral", "UES\n(59-79)", "UWS\n(59-79)")

make_cm_plot <- function(pred, actual, title_str, labels) {
  cm <- as.data.frame(table(Predicted = pred, True = actual))
  # match short labels
  levels_orig <- levels(actual)
  short_map   <- setNames(labels, levels_orig)
  cm$Predicted <- short_map[as.character(cm$Predicted)]
  cm$True      <- short_map[as.character(cm$True)]
  cm$Predicted <- factor(cm$Predicted, levels = labels)
  cm$True      <- factor(cm$True,      levels = labels)
  
  ggplot(cm, aes(x = Predicted, y = True, fill = Freq)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = Freq), size = 4, fontface = "bold") +
    scale_fill_gradient(low = "#EFF6FF", high = BLUE) +
    labs(title = title_str, x = "Predicted", y = "True") +
    theme_custom +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8))
}

extract_metrics <- function(cm, model_name) {
  tibble(
    Model     = model_name,
    Accuracy  = cm$overall["Accuracy"],
    Precision = mean(cm$byClass[, "Precision"], na.rm = TRUE),
    Recall    = mean(cm$byClass[, "Recall"],    na.rm = TRUE),
    F1        = mean(cm$byClass[, "F1"],        na.rm = TRUE)
  )
}

p5a <- make_cm_plot(knn_pred, yte, "kNN (k=7)",      short_labels)
p5b <- make_cm_plot(rf_pred,  yte, "Random Forest",  short_labels)
p5c <- make_cm_plot(svm_pred, yte, "SVM (RBF)",      short_labels)

metrics_df <- bind_rows(
  extract_metrics(cm_knn, "kNN (k=7)"),
  extract_metrics(cm_rf,  "Random Forest"),
  extract_metrics(cm_svm, "SVM (RBF)")
) %>%
  pivot_longer(-Model, names_to = "Metric", values_to = "Score")

p5d <- ggplot(metrics_df, aes(x = Metric, y = Score, fill = Model)) +
  geom_col(position = "dodge", colour = "white", linewidth = 0.4, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.2f", Score)),
            position = position_dodge(width = 0.9),
            vjust = -0.4, size = 2.8, fontface = "bold") +
  scale_fill_manual(values = PALETTE) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(title = "Weighted P / R / F1 by Model", y = "Score", x = NULL) +
  theme_custom + theme(legend.position = "top")

fig5 <- grid.arrange(
  arrangeGrob(p5a, p5b, p5c, ncol = 3),
  p5d,
  nrow = 2, heights = c(1.2, 1),
  top = grid::textGrob(
    "Figure 5 – Classification Results: Confusion Matrices & Metrics\n(Manhattan: UES, UWS, Harlem-Central, Chelsea)",
    gp = grid::gpar(fontface = "bold", fontsize = 13))
)


# ============================================================================
# FIGURE 6 – Feature Importance + Per-Class P/R/F1
# ============================================================================
imp_df <- as.data.frame(importance(rf_clf)) %>%
  rownames_to_column("Feature") %>%
  rename(Importance = MeanDecreaseGini) %>%
  arrange(Importance)

p6a <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = BLUE, colour = "white", linewidth = 0.4) +
  coord_flip() +
  labs(title = "RF Feature Importances (Classification)",
       x = NULL, y = "Mean Decrease Gini") +
  theme_custom

# Per-class metrics for RF
per_class <- as.data.frame(cm_rf$byClass) %>%
  rownames_to_column("Class") %>%
  mutate(Class = gsub("Class: ", "", Class),
         ShortClass = recode(Class,
                             "CHELSEA"                  = "Chelsea",
                             "HARLEM-CENTRAL"           = "Harlem-Central",
                             "UPPER EAST SIDE (59-79)"  = "UES (59-79)",
                             "UPPER WEST SIDE (59-79)"  = "UWS (59-79)"
         )) %>%
  select(ShortClass, Precision, Recall, F1) %>%
  pivot_longer(-ShortClass, names_to = "Metric", values_to = "Score")

p6b <- ggplot(per_class, aes(x = ShortClass, y = Score, fill = Metric)) +
  geom_col(position = "dodge", colour = "white", linewidth = 0.4, alpha = 0.85) +
  scale_fill_manual(values = c(Precision = BLUE, Recall = GREEN, F1 = ORANGE)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(title = "Per-Class P/R/F1 – Random Forest",
       x = NULL, y = "Score") +
  theme_custom +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "top")

fig6 <- grid.arrange(p6a, p6b, ncol = 2,
                     top = grid::textGrob("Figure 6 – RF Feature Importance & Per-Class Metrics",
                                          gp = grid::gpar(fontface = "bold", fontsize = 13)))


# ============================================================================
# SUMMARY TABLES
# ============================================================================
cat("\n=== REGRESSION SUMMARY ===\n")
reg_tbl <- tibble(
  Model = c("M1 Linear (raw)", "M2 Linear (log)", "M3 Ridge (log)", "M4 RF (log)", "M5 GBM (log)"),
  RMSE  = c(r1$rmse, r2_res$rmse, r3$rmse, r4$rmse, r5$rmse),
  MAE   = c(r1$mae,  r2_res$mae,  r3$mae,  r4$mae,  r5$mae),
  R2    = c(r1$r2,   r2_res$r2,   r3$r2,   r4$r2,   r5$r2)
)
print(reg_tbl)

cat("\n=== CLASSIFICATION SUMMARY ===\n")
clf_tbl <- bind_rows(
  extract_metrics(cm_knn, "kNN (k=7)"),
  extract_metrics(cm_rf,  "Random Forest"),
  extract_metrics(cm_svm, "SVM (RBF)")
)
print(clf_tbl)

cat("\n=== PER-CLASS REPORT (Random Forest) ===\n")
print(cm_rf$byClass[, c("Precision", "Recall", "F1")])
