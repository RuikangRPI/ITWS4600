# =============================================================================
# ITWS 4600 - Data Analytics Term Project
# Student: Ruikang Lin
# Script 4 of 4: Model 3 — Logistic Regression (At-Risk Classification)
# Research Question: Can academic engagement & distraction type predict
#                    whether a student falls into the at-risk productivity zone?
# Note: Claude and CoPilot are used to assist in code generation and figure design.
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(pROC)        # for ROC curve

setwd("Assignment 4")
df <- read.csv("student_productivity_distraction_dataset_20000.csv", stringsAsFactors = FALSE)

# --- Derived variables -------------------------------------------------------
atrisk_thresh <- quantile(df$productivity_score, 0.33)   # 42.58
df$at_risk    <- as.integer(df$productivity_score <= atrisk_thresh)
cat(sprintf("At-risk threshold: %.2f | At-risk students: %d (%.1f%%)\n",
            atrisk_thresh, sum(df$at_risk), mean(df$at_risk) * 100))

# --- Feature scaling (required for logistic regression coefficients) ---------
feat3 <- c("study_hours_per_day", "assignments_completed", "attendance_percentage",
           "focus_score", "social_media_hours", "youtube_hours", "gaming_hours")

df_scaled <- df
df_scaled[feat3] <- scale(df[feat3])

# =============================================================================
# MODEL 3: Logistic Regression — At-Risk Classification
# =============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("MODEL 3: Logistic Regression — At-Risk Productivity Classification\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

set.seed(42)
train_idx <- sample(nrow(df_scaled), 0.8 * nrow(df_scaled))
train3    <- df_scaled[train_idx, ]
test3     <- df_scaled[-train_idx, ]

model3 <- glm(at_risk ~ study_hours_per_day + assignments_completed +
                attendance_percentage + focus_score +
                social_media_hours + youtube_hours + gaming_hours,
              data   = train3,
              family = binomial(link = "logit"))

cat("\nModel 3 Summary:\n")
print(summary(model3))

# Odds ratios and 95% CIs
cat("\n=== ODDS RATIOS (exp(coef)) with 95% CI ===\n")
or_table <- data.frame(
  Feature    = feat3,
  Coef       = coef(model3)[-1],
  OddsRatio  = exp(coef(model3)[-1]),
  CI_lower   = exp(suppressMessages(confint(model3))[-1, 1]),
  CI_upper   = exp(suppressMessages(confint(model3))[-1, 2]),
  p_value    = summary(model3)$coefficients[-1, 4]
)
or_table_print <- or_table
or_table_print[, -1] <- round(or_table_print[, -1], 4)
print(or_table_print, row.names = FALSE)

# --- Predictions and Metrics -------------------------------------------------
prob3  <- predict(model3, newdata = test3, type = "response")
pred3  <- as.integer(prob3 >= 0.5)

acc    <- mean(pred3 == test3$at_risk)
cm     <- table(Predicted = pred3, Actual = test3$at_risk)
TP     <- cm[2, 2]; TN <- cm[1, 1]; FP <- cm[2, 1]; FN <- cm[1, 2]
prec   <- TP / (TP + FP)
rec    <- TP / (TP + FN)
f1     <- 2 * prec * rec / (prec + rec)
auc    <- as.numeric(pROC::auc(pROC::roc(test3$at_risk, prob3, quiet = TRUE)))

cat(sprintf("\nAccuracy:  %.4f\n", acc))
cat(sprintf("Precision: %.4f\n", prec))
cat(sprintf("Recall:    %.4f\n", rec))
cat(sprintf("F1 Score:  %.4f\n", f1))
cat(sprintf("AUC-ROC:   %.4f\n", auc))

cat("\nConfusion Matrix:\n")
print(cm)

# --- 5-Fold Stratified Cross-Validation --------------------------------------
set.seed(42)
folds   <- sample(cut(seq_len(nrow(df_scaled)), breaks = 5, labels = FALSE))
cv_acc  <- numeric(5); cv_auc <- numeric(5)
for (k in 1:5) {
  tr <- df_scaled[folds != k, ]
  te <- df_scaled[folds == k, ]
  m  <- glm(at_risk ~ study_hours_per_day + assignments_completed +
              attendance_percentage + focus_score +
              social_media_hours + youtube_hours + gaming_hours,
            data = tr, family = binomial)
  p_prob <- predict(m, te, type = "response")
  p_pred <- as.integer(p_prob >= 0.5)
  cv_acc[k] <- mean(p_pred == te$at_risk)
  cv_auc[k] <- as.numeric(pROC::auc(pROC::roc(te$at_risk, p_prob, quiet = TRUE)))
}
cat(sprintf("\n5-Fold CV Accuracy: %.4f ± %.4f\n", mean(cv_acc), sd(cv_acc)))
cat(sprintf("5-Fold CV AUC-ROC:  %.4f ± %.4f\n", mean(cv_auc), sd(cv_auc)))

# --- Model with vs without focus_score (mediation check) ---------------------
model3_nf <- glm(at_risk ~ study_hours_per_day + assignments_completed +
                   attendance_percentage +
                   social_media_hours + youtube_hours + gaming_hours,
                 data = train3, family = binomial)
prob3_nf  <- predict(model3_nf, newdata = test3, type = "response")
auc_nf    <- as.numeric(pROC::auc(pROC::roc(test3$at_risk, prob3_nf, quiet = TRUE)))
acc_nf    <- mean(as.integer(prob3_nf >= 0.5) == test3$at_risk)
cat(sprintf("\nModel WITHOUT focus_score: Accuracy=%.4f, AUC=%.4f\n", acc_nf, auc_nf))
cat(sprintf("Model WITH    focus_score: Accuracy=%.4f, AUC=%.4f\n", acc, auc))
cat(sprintf("Focus score adds %.2f%% accuracy and %.4f AUC\n",
            (acc - acc_nf) * 100, auc - auc_nf))

# =============================================================================
# FIGURES FOR SECTION 4 — MODEL 3
# =============================================================================

# --- FIGURE 21: Odds Ratio Plot ----------------------------------------------
or_plot <- data.frame(
  Feature   = c("Study Hours/Day","Assignments\nCompleted",
                "Attendance %","Focus Score",
                "Social Media\nHours","YouTube Hours","Gaming Hours"),
  OR        = exp(coef(model3)[-1]),
  lower     = exp(suppressMessages(confint(model3))[-1, 1]),
  upper     = exp(suppressMessages(confint(model3))[-1, 2]),
  sig       = summary(model3)$coefficients[-1, 4] < 0.05
) %>%
  arrange(OR) %>%
  mutate(Feature = factor(Feature, levels = Feature),
         direction = ifelse(OR < 1, "Reduces At-Risk Odds", "Increases At-Risk Odds"))

fig21 <- ggplot(or_plot, aes(x = OR, y = Feature, color = direction)) +
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.8) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  scale_color_manual(values = c("Reduces At-Risk Odds"   = "#4E79A7",
                                "Increases At-Risk Odds" = "#E15759")) +
  scale_x_log10() +
  labs(title    = "Figure 21: Odds Ratios — Model 3 (Logistic Regression)",
       subtitle = "Odds ratio < 1 (left of dashed line) = reduces probability of being at-risk\nBars = 95% confidence intervals | x-axis on log scale",
       x = "Odds Ratio (log scale)", y = NULL, color = NULL,
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(fig21)


# --- FIGURE 22: ROC Curve — Model 3 ------------------------------------------
roc_obj  <- pROC::roc(test3$at_risk, prob3, quiet = TRUE)
roc_df   <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

# Also ROC without focus_score
roc_nf   <- pROC::roc(test3$at_risk, prob3_nf, quiet = TRUE)
roc_nf_df <- data.frame(
  fpr = 1 - roc_nf$specificities,
  tpr = roc_nf$sensitivities
)

fig22 <- ggplot() +
  geom_line(data = roc_df,    aes(x = fpr, y = tpr, color = "With Focus Score"),    linewidth = 1.1) +
  geom_line(data = roc_nf_df, aes(x = fpr, y = tpr, color = "Without Focus Score"), linewidth = 1.1, linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray60") +
  scale_color_manual(values = c("With Focus Score"    = "#4E79A7",
                                "Without Focus Score" = "#E15759")) +
  annotate("text", x = 0.65, y = 0.20,
           label = sprintf("AUC (with focus)    = %.4f\nAUC (without focus) = %.4f",
                           as.numeric(pROC::auc(roc_obj)),
                           as.numeric(pROC::auc(roc_nf))),
           size = 3.5, hjust = 0, family = "mono") +
  labs(title    = "Figure 22: ROC Curve — Model 3",
       subtitle = "Comparing model with and without focus_score as a predictor",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       color = NULL,
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(fig22)


# --- FIGURE 23: Confusion Matrix Heatmap -------------------------------------
cm_df <- data.frame(
  Predicted = factor(c("Productive","Productive","At-Risk","At-Risk"),
                     levels = c("Productive","At-Risk")),
  Actual    = factor(c("Productive","At-Risk","Productive","At-Risk"),
                     levels = c("Productive","At-Risk")),
  Count     = c(TN, FN, FP, TP),
  Label     = c(sprintf("TN\n%d\n(%.1f%%)", TN, TN/nrow(test3)*100),
                sprintf("FN\n%d\n(%.1f%%)", FN, FN/nrow(test3)*100),
                sprintf("FP\n%d\n(%.1f%%)", FP, FP/nrow(test3)*100),
                sprintf("TP\n%d\n(%.1f%%)", TP, TP/nrow(test3)*100))
)

fig23 <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Label), size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#EAF2FB", high = "#2E75B6") +
  labs(title    = "Figure 23: Confusion Matrix — Model 3 (Test Set, n = 4,000)",
       subtitle = sprintf("Accuracy = %.1f%% | Precision = %.1f%% | Recall = %.1f%% | F1 = %.3f",
                          acc*100, prec*100, rec*100, f1),
       x = "Actual Class", y = "Predicted Class", fill = "Count",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_text(size = 12))

print(fig23)


# --- FIGURE 24: Predicted Probability Density by Actual Class ----------------
prob_df <- data.frame(
  probability  = prob3,
  actual_class = ifelse(test3$at_risk == 1, "At-Risk", "Productive")
)

fig24 <- ggplot(prob_df, aes(x = probability, fill = actual_class)) +
  geom_density(alpha = 0.65, color = "white") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = 0.52, y = 5.5, label = "Decision\nThreshold\n(0.5)", size = 3.2, hjust = 0) +
  scale_fill_manual(values = c("At-Risk"    = "#E15759",
                                "Productive" = "#4E79A7")) +
  labs(title    = "Figure 24: Predicted Probability Distribution — Model 3",
       subtitle = "Well-separated distributions indicate strong class discrimination",
       x = "Predicted Probability of Being At-Risk", y = "Density",
       fill = "Actual Class",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(fig24)

cat("\nScript 4 complete. Figures 21–24 generated.\n")