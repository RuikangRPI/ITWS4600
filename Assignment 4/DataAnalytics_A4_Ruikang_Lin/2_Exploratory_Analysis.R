# =============================================================================
# ITWS 4600 - Data Analytics Term Project
# Student: Ruikang Lin
# Script 2 of 4: Exploratory Analysis (Section 3)
# Dataset: Student Productivity & Behavior Dataset (20K)
# Note: Claude and CoPilot are used to assist in code generation and figure design.
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(gridExtra)
library(ggridges)

setwd("Assignment 4")
df <- read.csv("student_productivity_distraction_dataset_20000.csv", stringsAsFactors = FALSE)

# --- Derived variables used throughout exploratory analysis -----------------
threshold_67 <- quantile(df$phone_usage_hours, 0.67)   # high screen-time cutoff
atrisk_thresh <- quantile(df$productivity_score, 0.33) # at-risk cutoff

df$screen_group <- ifelse(df$phone_usage_hours >= threshold_67,
                          "High Screen Time (top 33%)",
                          "Lower Screen Time (bottom 67%)")
df$at_risk <- ifelse(df$productivity_score <= atrisk_thresh, "At-Risk", "Productive")


# =============================================================================
# FIGURE 9: QQ-Plots for Key Variables
# Confirms all variables deviate from normality (uniform distribution shape)
# =============================================================================
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
qq_vars <- list(
  "Productivity Score"    = df$productivity_score,
  "Study Hours/Day"       = df$study_hours_per_day,
  "Phone Usage Hours"     = df$phone_usage_hours,
  "Sleep Hours"           = df$sleep_hours,
  "Focus Score"           = df$focus_score,
  "Stress Level"          = df$stress_level
)
for (name in names(qq_vars)) {
  qqnorm(qq_vars[[name]], main = paste("Q-Q Plot:", name),
         col = "#4E79A7", pch = 16, cex = 0.3)
  qqline(qq_vars[[name]], col = "#E15759", lwd = 2)
}
mtext("Figure 9: Q-Q Plots for Key Variables (deviation from normality confirms uniform generation)",
      side = 3, line = -1.5, outer = TRUE, cex = 0.85)
par(mfrow = c(1, 1))


# =============================================================================
# FIGURE 10: Density Plots — All Continuous Variables
# Visually confirms near-uniform distributions across predictors
# =============================================================================
continuous_vars <- df %>%
  select(study_hours_per_day, sleep_hours, phone_usage_hours, social_media_hours,
         youtube_hours, gaming_hours, exercise_minutes, attendance_percentage,
         focus_score, final_grade, productivity_score) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
    study_hours_per_day   = "Study Hours/Day",
    sleep_hours           = "Sleep Hours",
    phone_usage_hours     = "Phone Usage",
    social_media_hours    = "Social Media",
    youtube_hours         = "YouTube",
    gaming_hours          = "Gaming",
    exercise_minutes      = "Exercise (min)",
    attendance_percentage = "Attendance (%)",
    focus_score           = "Focus Score",
    final_grade           = "Final Grade",
    productivity_score    = "Productivity Score"
  ))

fig10 <- ggplot(continuous_vars, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.7, color = "white") +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  scale_fill_viridis_d(option = "D") +
  labs(title = "Figure 10: Density Plots of All Continuous Variables",
       subtitle = "Nearly all predictors follow a uniform distribution — a hallmark of synthetic data generation",
       x = "Value", y = "Density",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", strip.text = element_text(size = 8))

print(fig10)


# =============================================================================
# FIGURE 11: Partial Regression — Isolating Each Predictor's Effect
# on Productivity After Controlling for Study Hours (the dominant predictor)
# =============================================================================

# Residualize productivity and each predictor against study_hours
resid_prod <- residuals(lm(productivity_score ~ study_hours_per_day, data = df))

partials <- data.frame(
  phone_resid    = residuals(lm(phone_usage_hours   ~ study_hours_per_day, data = df)),
  sleep_resid    = residuals(lm(sleep_hours         ~ study_hours_per_day, data = df)),
  stress_resid   = residuals(lm(stress_level        ~ study_hours_per_day, data = df)),
  focus_resid    = residuals(lm(focus_score         ~ study_hours_per_day, data = df)),
  exercise_resid = residuals(lm(exercise_minutes    ~ study_hours_per_day, data = df)),
  prod_resid     = resid_prod
)

pa <- ggplot(partials, aes(x = phone_resid, y = prod_resid)) +
  geom_point(alpha = 0.05, color = "#E15759", size = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 0.9) +
  labs(title = "Phone Usage | Study Hrs", x = "Phone (residualized)", y = "Productivity (residualized)") +
  theme_minimal(base_size = 10)

pb <- ggplot(partials, aes(x = sleep_resid, y = prod_resid)) +
  geom_point(alpha = 0.05, color = "#4E79A7", size = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 0.9) +
  labs(title = "Sleep | Study Hrs", x = "Sleep (residualized)", y = "Productivity (residualized)") +
  theme_minimal(base_size = 10)

pc <- ggplot(partials, aes(x = stress_resid, y = prod_resid)) +
  geom_point(alpha = 0.05, color = "#F28E2B", size = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 0.9) +
  labs(title = "Stress | Study Hrs", x = "Stress (residualized)", y = "Productivity (residualized)") +
  theme_minimal(base_size = 10)

pd <- ggplot(partials, aes(x = focus_resid, y = prod_resid)) +
  geom_point(alpha = 0.05, color = "#59A14F", size = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 0.9) +
  labs(title = "Focus Score | Study Hrs", x = "Focus (residualized)", y = "Productivity (residualized)") +
  theme_minimal(base_size = 10)

pe <- ggplot(partials, aes(x = exercise_resid, y = prod_resid)) +
  geom_point(alpha = 0.05, color = "#76B7B2", size = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 0.9) +
  labs(title = "Exercise | Study Hrs", x = "Exercise (residualized)", y = "Productivity (residualized)") +
  theme_minimal(base_size = 10)

fig11 <- grid.arrange(pa, pb, pc, pd, pe, ncol = 3,
  top = "Figure 11: Partial Regression Plots (effect on Productivity after controlling for Study Hours)")
print(fig11)


# =============================================================================
# FIGURE 12: Ridge Density — Productivity Score by Stress Level
# Shows productivity distribution across each stress level (1–10)
# =============================================================================
df$stress_f <- factor(df$stress_level, levels = 10:1)

fig12 <- ggplot(df, aes(x = productivity_score, y = stress_f, fill = stress_level)) +
  geom_density_ridges(alpha = 0.75, scale = 1.5, color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#4E79A7", mid = "#F0E442", high = "#E15759",
                       midpoint = 5.5, name = "Stress") +
  geom_vline(xintercept = atrisk_thresh, linetype = "dashed",
             color = "black", linewidth = 0.7) +
  annotate("text", x = atrisk_thresh + 1.5, y = 0.6,
           label = "At-Risk\nThreshold", size = 3, hjust = 0) +
  labs(title = "Figure 12: Productivity Score Distribution by Stress Level",
       subtitle = "Ridge plot reveals that productivity distributions are nearly identical across all stress levels",
       x = "Productivity Score", y = "Stress Level",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12)

print(fig12)


# =============================================================================
# FIGURE 13: Scatter Matrix — Key Variables Colored by Screen Group
# Shows pairwise relationships for Model 1's main variables
# =============================================================================
# Using base R pairs() for compact multi-panel scatter
key_vars <- df %>%
  select(productivity_score, study_hours_per_day, phone_usage_hours,
         sleep_hours, stress_level, focus_score, exercise_minutes) %>%
  rename(Productivity = productivity_score,
         Study        = study_hours_per_day,
         Phone        = phone_usage_hours,
         Sleep        = sleep_hours,
         Stress       = stress_level,
         Focus        = focus_score,
         Exercise     = exercise_minutes)

colors <- ifelse(df$screen_group == "High Screen Time (top 33%)", 
                 adjustcolor("#E15759", alpha.f = 0.15),
                 adjustcolor("#4E79A7", alpha.f = 0.15))

pairs(key_vars, pch = 16, cex = 0.3, col = colors,
      main = "Figure 13: Scatter Matrix of Key Variables\n(Red = High Screen Time, Blue = Lower Screen Time)")
legend("topright", legend = c("High Screen Time", "Lower Screen Time"),
       col = c("#E15759", "#4E79A7"), pch = 16, cex = 0.8, bty = "n")


# =============================================================================
# FIGURE 14: Violin Plot — Productivity by Screen Group × High/Low Exercise
# Explores the interaction between screen time and exercise compensation
# =============================================================================
df$exercise_group <- ifelse(df$exercise_minutes >= median(df$exercise_minutes),
                            "Higher Exercise", "Lower Exercise")
df$combo_group <- paste(df$screen_group, "\n+", df$exercise_group)

fig14 <- ggplot(df, aes(x = combo_group, y = productivity_score, fill = screen_group)) +
  geom_violin(alpha = 0.7, trim = FALSE, color = "white") +
  geom_boxplot(width = 0.1, fill = "white", color = "gray30",
               outlier.size = 0.3, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("High Screen Time (top 33%)"   = "#E15759",
                                "Lower Screen Time (bottom 67%)" = "#4E79A7")) +
  labs(title = "Figure 14: Productivity Score by Screen Group and Exercise Level",
       subtitle = "Explores whether exercise meaningfully compensates for high screen time",
       x = NULL, y = "Productivity Score", fill = "Screen Group",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(size = 8), legend.position = "bottom")

print(fig14)


# =============================================================================
# FIGURE 15: Heatmap — Mean Productivity by Study Hours × Phone Usage Bins
# Reveals the interaction structure between the two strongest predictors
# =============================================================================
df$study_bin <- cut(df$study_hours_per_day,
                    breaks = c(0, 2.5, 5, 7.5, 10),
                    labels = c("Low (0–2.5h)", "Med-Low (2.5–5h)",
                               "Med-High (5–7.5h)", "High (7.5–10h)"))
df$phone_bin <- cut(df$phone_usage_hours,
                    breaks = c(0, 3, 6, 9, 12),
                    labels = c("Low (0–3h)", "Med-Low (3–6h)",
                               "Med-High (6–9h)", "High (9–12h)"))

heatmap_data <- df %>%
  group_by(study_bin, phone_bin) %>%
  summarise(mean_productivity = mean(productivity_score), .groups = "drop")

fig15 <- ggplot(heatmap_data, aes(x = phone_bin, y = study_bin, fill = mean_productivity)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(mean_productivity, 1)), size = 4, color = "white", fontface = "bold") +
  scale_fill_gradient2(low = "#E15759", mid = "#F0E442", high = "#4E79A7",
                       midpoint = 50, name = "Mean\nProductivity") +
  labs(title = "Figure 15: Mean Productivity Score by Study Hours × Phone Usage",
       subtitle = "Each cell = average productivity for students in that study/phone usage combination",
       x = "Phone Usage (hours/day)", y = "Study Hours (per day)",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(fig15)


# =============================================================================
# FIGURE 16: At-Risk Distribution — Point-Biserial Correlations Bar Chart
# Highlights which variables most strongly predict at-risk classification
# =============================================================================
atrisk_cors <- data.frame(
  variable = c("Study Hours", "Productivity Score", "Focus Score",
                "Sleep Hours", "Phone Usage", "Stress Level",
                "Attendance %", "Final Grade", "Assignments",
                "Exercise", "Social Media", "YouTube", "Gaming",
                "Coffee", "Breaks"),
  r = c(-0.6012, -0.7859, -0.3039, -0.2544, 0.2461, 0.1491,
        -0.1295, -0.0072, -0.0039, 0.0011, -0.0007, 0.0008,
        -0.0037, -0.0055, 0.0002)
) %>%
  arrange(r) %>%
  mutate(variable = factor(variable, levels = variable),
         direction = ifelse(r < 0, "Reduces Risk", "Increases Risk"))

fig16 <- ggplot(atrisk_cors, aes(x = r, y = variable, fill = direction)) +
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("Reduces Risk" = "#4E79A7", "Increases Risk" = "#E15759")) +
  labs(title = "Figure 16: Point-Biserial Correlations with At-Risk Classification",
       subtitle = "Positive r = associated with being at-risk; negative r = protective",
       x = "Point-Biserial Correlation (r)", y = NULL, fill = NULL,
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(fig16)


# =============================================================================
# Print summary of key findings for reference
# =============================================================================
cat("\n=== KEY EXPLORATORY FINDINGS SUMMARY ===\n")
cat(sprintf("High screen-time threshold (67th pct):  %.2f hrs/day\n", threshold_67))
cat(sprintf("At-risk threshold (33rd pct):           %.2f productivity score\n", atrisk_thresh))
cat(sprintf("At-risk students:                       %d (%.1f%%)\n",
            sum(df$at_risk == "At-Risk"), mean(df$at_risk == "At-Risk") * 100))

# R² of productivity ~ study + phone + sleep + stress + focus
model_full <- lm(productivity_score ~ study_hours_per_day + phone_usage_hours +
                   sleep_hours + stress_level + focus_score, data = df)
cat(sprintf("\nR² (productivity ~ study+phone+sleep+stress+focus): %.4f\n",
            summary(model_full)$r.squared))
cat("Coefficients:\n")
print(round(coef(model_full), 4))

cat("\nScript 2 complete. Figures 9–16 generated.\n")
