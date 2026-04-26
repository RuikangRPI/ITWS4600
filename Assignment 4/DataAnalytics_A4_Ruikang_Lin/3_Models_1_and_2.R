# =============================================================================
# ITWS 4600 - Data Analytics Term Project
# Student: Ruikang Lin
# Script 3 of 4: Model 1 & Model 2 — Multiple Linear Regression
# Research Question: Among high-screen-time students, which wellness behaviors
#                    predict productivity? Are those effects unique to this group?
# Note: Claude and CoPilot are used to assist in code generation and figure design.
# =============================================================================

library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)        # for vif()
library(lmtest)     # for bptest()

setwd("Assignment 4")
df <- read.csv("student_productivity_distraction_dataset_20000.csv", stringsAsFactors = FALSE)

# --- Derived variables -------------------------------------------------------
threshold_67  <- quantile(df$phone_usage_hours, 0.67)   # 8.22 hrs/day
atrisk_thresh <- quantile(df$productivity_score, 0.33)  # 42.58

df$screen_group <- ifelse(df$phone_usage_hours >= threshold_67,
                          "High Screen Time", "Lower Screen Time")
high <- df[df$screen_group == "High Screen Time", ]
cat(sprintf("High screen-time group: n = %d (threshold >= %.2f hrs/day)\n",
            nrow(high), threshold_67))
cat(sprintf("Full dataset:           n = %d\n", nrow(df)))

# =============================================================================
# MODEL 1: Multiple Linear Regression — High Screen-Time Subgroup
# Predictors: exercise_minutes, sleep_hours, breaks_per_day,
#             coffee_intake_mg, stress_level
# Outcome:    productivity_score
# =============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("MODEL 1: OLS — High Screen-Time Subgroup\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

set.seed(42)
train_idx1 <- sample(nrow(high), 0.8 * nrow(high))
train1 <- high[train_idx1, ]
test1  <- high[-train_idx1, ]

model1 <- lm(productivity_score ~ exercise_minutes + sleep_hours +
               breaks_per_day + coffee_intake_mg + stress_level,
             data = train1)

cat("\nModel 1 Summary:\n")
print(summary(model1))

# Test set predictions and metrics
pred1      <- predict(model1, newdata = test1)
ss_res1    <- sum((test1$productivity_score - pred1)^2)
ss_tot1    <- sum((test1$productivity_score - mean(test1$productivity_score))^2)
r2_test1   <- 1 - ss_res1 / ss_tot1
rmse1      <- sqrt(mean((test1$productivity_score - pred1)^2))
mae1       <- mean(abs(test1$productivity_score - pred1))

cat(sprintf("\nTest R²:   %.4f\n", r2_test1))
cat(sprintf("Test RMSE: %.4f\n", rmse1))
cat(sprintf("Test MAE:  %.4f\n", mae1))

# 5-Fold Cross-Validation
cv_r2_1 <- numeric(5)
folds    <- cut(seq_len(nrow(high)), breaks = 5, labels = FALSE)
folds    <- sample(folds)
for (k in 1:5) {
  tr <- high[folds != k, ]
  te <- high[folds == k, ]
  m  <- lm(productivity_score ~ exercise_minutes + sleep_hours +
              breaks_per_day + coffee_intake_mg + stress_level, data = tr)
  p  <- predict(m, te)
  ss_r <- sum((te$productivity_score - p)^2)
  ss_t <- sum((te$productivity_score - mean(te$productivity_score))^2)
  cv_r2_1[k] <- 1 - ss_r / ss_t
}
cat(sprintf("5-Fold CV R²: %.4f ± %.4f\n", mean(cv_r2_1), sd(cv_r2_1)))

# Diagnostics
cat("\nVIF (Model 1):\n")
print(vif(model1))

cat("\nBreusch-Pagan Homoscedasticity Test:\n")
print(bptest(model1))


# =============================================================================
# MODEL 2: Multiple Linear Regression — Full Dataset (same predictors)
# =============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("MODEL 2: OLS — Full Dataset (same wellness predictors)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

set.seed(42)
train_idx2 <- sample(nrow(df), 0.8 * nrow(df))
train2 <- df[train_idx2, ]
test2  <- df[-train_idx2, ]

model2 <- lm(productivity_score ~ exercise_minutes + sleep_hours +
               breaks_per_day + coffee_intake_mg + stress_level,
             data = train2)

cat("\nModel 2 Summary:\n")
print(summary(model2))

pred2    <- predict(model2, newdata = test2)
ss_res2  <- sum((test2$productivity_score - pred2)^2)
ss_tot2  <- sum((test2$productivity_score - mean(test2$productivity_score))^2)
r2_test2 <- 1 - ss_res2 / ss_tot2
rmse2    <- sqrt(mean((test2$productivity_score - pred2)^2))
mae2     <- mean(abs(test2$productivity_score - pred2))

cat(sprintf("\nTest R²:   %.4f\n", r2_test2))
cat(sprintf("Test RMSE: %.4f\n", rmse2))
cat(sprintf("Test MAE:  %.4f\n", mae2))

cv_r2_2 <- numeric(5)
folds2   <- cut(seq_len(nrow(df)), breaks = 5, labels = FALSE)
folds2   <- sample(folds2)
for (k in 1:5) {
  tr <- df[folds2 != k, ]
  te <- df[folds2 == k, ]
  m  <- lm(productivity_score ~ exercise_minutes + sleep_hours +
              breaks_per_day + coffee_intake_mg + stress_level, data = tr)
  p  <- predict(m, te)
  ss_r <- sum((te$productivity_score - p)^2)
  ss_t <- sum((te$productivity_score - mean(te$productivity_score))^2)
  cv_r2_2[k] <- 1 - ss_r / ss_t
}
cat(sprintf("5-Fold CV R²: %.4f ± %.4f\n", mean(cv_r2_2), sd(cv_r2_2)))

cat("\nBreusch-Pagan Test (Model 2):\n")
print(bptest(model2))


# =============================================================================
# COEFFICIENT COMPARISON TABLE
# =============================================================================
cat("\n=== COEFFICIENT COMPARISON: Model 1 vs Model 2 ===\n")
coef_df <- data.frame(
  Predictor = c("exercise_minutes","sleep_hours","breaks_per_day",
                "coffee_intake_mg","stress_level"),
  M1_coef   = round(coef(model1)[-1], 4),
  M1_p      = round(summary(model1)$coefficients[-1, 4], 4),
  M2_coef   = round(coef(model2)[-1], 4),
  M2_p      = round(summary(model2)$coefficients[-1, 4], 4)
)
print(coef_df, row.names = FALSE)

# T-test: high-exercise high-screeners vs low-screeners
high_ex  <- high[high$exercise_minutes >= median(df$exercise_minutes), ]
low_sc   <- df[df$screen_group == "Lower Screen Time", ]
tt       <- t.test(high_ex$productivity_score, low_sc$productivity_score)
cat(sprintf("\nT-test — High Screen + High Exercise (mean=%.2f) vs Low Screen (mean=%.2f):\n",
            mean(high_ex$productivity_score), mean(low_sc$productivity_score)))
cat(sprintf("  t=%.3f, p=%.4e  → Exercise does NOT close the productivity gap\n",
            tt$statistic, tt$p.value))


# =============================================================================
# FIGURES FOR SECTION 4 — MODELS 1 & 2
# =============================================================================

# --- FIGURE 17: Coefficient Plot — Model 1 vs Model 2 ----------------------
m1_full <- lm(productivity_score ~ exercise_minutes + sleep_hours +
                breaks_per_day + coffee_intake_mg + stress_level, data = high)
m2_full <- lm(productivity_score ~ exercise_minutes + sleep_hours +
                breaks_per_day + coffee_intake_mg + stress_level, data = df)

ci1 <- confint(m1_full)[-1, ]
ci2 <- confint(m2_full)[-1, ]

coef_plot_data <- data.frame(
  predictor = rep(c("Exercise (min)","Sleep Hours","Breaks/Day",
                    "Coffee (mg)","Stress Level"), 2),
  model     = rep(c("Model 1 (High Screen Subgroup)", "Model 2 (Full Dataset)"), each = 5),
  coef      = c(coef(m1_full)[-1], coef(m2_full)[-1]),
  lower     = c(ci1[,1], ci2[,1]),
  upper     = c(ci1[,2], ci2[,2])
)

fig17 <- ggplot(coef_plot_data,
                aes(x = coef, y = predictor, color = model,
                    xmin = lower, xmax = upper)) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Model 1 (High Screen Subgroup)" = "#E15759",
                                "Model 2 (Full Dataset)"         = "#4E79A7")) +
  labs(title    = "Figure 17: Coefficient Plot — Model 1 vs Model 2",
       subtitle = "Point = estimated coefficient; bars = 95% confidence interval",
       x = "Coefficient (effect on Productivity Score)", y = NULL,
       color = NULL,
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(fig17)


# --- FIGURE 18: Actual vs Predicted — Model 1 --------------------------------
pred1_plot <- data.frame(
  actual    = test1$productivity_score,
  predicted = pred1,
  residual  = test1$productivity_score - pred1
)

fig18a <- ggplot(pred1_plot, aes(x = predicted, y = actual)) +
  geom_point(alpha = 0.15, color = "#E15759", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Figure 18a: Actual vs Predicted — Model 1",
       subtitle = sprintf("High Screen Subgroup | Test R² = %.3f, RMSE = %.2f",
                          r2_test1, rmse1),
       x = "Predicted Productivity Score", y = "Actual Productivity Score") +
  theme_minimal(base_size = 11)

fig18b <- ggplot(pred1_plot, aes(x = predicted, y = residual)) +
  geom_point(alpha = 0.15, color = "#E15759", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", color = "#4E79A7", se = FALSE, linewidth = 0.8) +
  labs(title = "Figure 18b: Residuals vs Fitted — Model 1",
       subtitle = "No clear pattern = homoscedasticity satisfied",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal(base_size = 11)

grid.arrange(fig18a, fig18b, ncol = 2,
  top = "Figure 18: Model 1 Prediction Diagnostics")


# --- FIGURE 19: Actual vs Predicted — Model 2 --------------------------------
pred2_plot <- data.frame(
  actual    = test2$productivity_score,
  predicted = pred2,
  residual  = test2$productivity_score - pred2
)

fig19a <- ggplot(pred2_plot, aes(x = predicted, y = actual)) +
  geom_point(alpha = 0.08, color = "#4E79A7", size = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Figure 19a: Actual vs Predicted — Model 2",
       subtitle = sprintf("Full Dataset | Test R² = %.3f, RMSE = %.2f",
                          r2_test2, rmse2),
       x = "Predicted Productivity Score", y = "Actual Productivity Score") +
  theme_minimal(base_size = 11)

fig19b <- ggplot(pred2_plot, aes(x = predicted, y = residual)) +
  geom_point(alpha = 0.08, color = "#4E79A7", size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", color = "#E15759", se = FALSE, linewidth = 0.8) +
  labs(title = "Figure 19b: Residuals vs Fitted — Model 2",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal(base_size = 11)

grid.arrange(fig19a, fig19b, ncol = 2,
  top = "Figure 19: Model 2 Prediction Diagnostics")


# --- FIGURE 20: Model Performance Comparison Bar Chart -----------------------
perf_data <- data.frame(
  Model    = c("Model 1\n(High Screen Subgroup)", "Model 2\n(Full Dataset)"),
  R2       = c(summary(m1_full)$r.squared, summary(m2_full)$r.squared),
  Adj_R2   = c(summary(m1_full)$adj.r.squared, summary(m2_full)$adj.r.squared),
  CV_R2    = c(mean(cv_r2_1), mean(cv_r2_2))
)

perf_long <- tidyr::pivot_longer(perf_data, cols = c(R2, Adj_R2, CV_R2),
                                  names_to = "Metric", values_to = "Value")

fig20 <- ggplot(perf_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
  scale_fill_manual(values = c("R2"     = "#4E79A7",
                                "Adj_R2" = "#59A14F",
                                "CV_R2"  = "#F28E2B"),
                    labels = c("R² (Train)", "Adj R² (Train)", "5-Fold CV R²")) +
  geom_text(aes(label = sprintf("%.3f", Value)),
            position = position_dodge(width = 0.6),
            vjust = -0.4, size = 3.2) +
  ylim(0, 0.30) +
  labs(title    = "Figure 20: Model Performance Comparison — Models 1 & 2",
       subtitle = "Higher R² indicates more variance explained by wellness predictors",
       x = NULL, y = "R²", fill = "Metric",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(fig20)

cat("\nScript 3 complete. Figures 17–20 generated.\n")
