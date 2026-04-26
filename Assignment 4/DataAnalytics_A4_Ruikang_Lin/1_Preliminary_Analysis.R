# =============================================================================
# ITWS 4600 - Data Analytics Term Project
# Student: Ruikang Lin
# Script 1 of 4: Preliminary Analysis (Section 2)
# Dataset: Student Productivity & Behavior Dataset (20K)
# Note: Claude and CoPilot are used to assist in code generation and figure design.
# =============================================================================

# --- Load required libraries -------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(gridExtra)

# --- Load dataset ------------------------------------------------------------
setwd("Assignment 4")
df <- read.csv("student_productivity_distraction_dataset_20000.csv", stringsAsFactors = FALSE)

# --- Basic inspection --------------------------------------------------------
cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Column names:\n")
print(colnames(df))
cat("\nMissing values per column:\n")
print(colSums(is.na(df)))
cat("\nSummary statistics:\n")
print(summary(df))


# =============================================================================
# FIGURE 1: Distribution of Productivity Score
# Shows the overall spread and shape; roughly normal centered ~50
# =============================================================================
fig1 <- ggplot(df, aes(x = productivity_score)) +
  geom_histogram(binwidth = 3, fill = "#4E79A7", color = "white", alpha = 0.85) +
  geom_vline(xintercept = quantile(df$productivity_score, 0.33),
             linetype = "dashed", color = "#E15759", linewidth = 0.8) +
  annotate("text", x = quantile(df$productivity_score, 0.33) + 2, y = 700,
           label = "At-Risk\nThreshold\n(33rd pct)", color = "#E15759", size = 3.2, hjust = 0) +
  labs(title = "Figure 1: Distribution of Productivity Score",
       subtitle = "Red dashed line marks the at-risk threshold (bottom 33% ≈ score ≤ 42.6)",
       x = "Productivity Score", y = "Count",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12)

print(fig1)


# =============================================================================
# FIGURE 2: Scatter Plot — Phone Usage vs. Productivity Score
# Expected negative trend; motivates the high-screen-time subgroup
# =============================================================================
fig2 <- ggplot(df, aes(x = phone_usage_hours, y = productivity_score)) +
  geom_point(alpha = 0.08, color = "#4E79A7", size = 0.9) +
  geom_smooth(method = "lm", color = "#E15759", se = TRUE, linewidth = 1.1) +
  geom_vline(xintercept = quantile(df$phone_usage_hours, 0.67),
             linetype = "dashed", color = "#59A14F", linewidth = 0.8) +
  annotate("text", x = quantile(df$phone_usage_hours, 0.67) + 0.2, y = 95,
           label = "Top 33%\nthreshold\n(~8.2 hrs)", color = "#59A14F", size = 3.2, hjust = 0) +
  labs(title = "Figure 2: Phone Usage Hours vs. Productivity Score",
       subtitle = "Each point = one student. Red line = linear fit. Green dashed = high screen-time cutoff.",
       x = "Phone Usage (hours/day)", y = "Productivity Score",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12)

print(fig2)


# =============================================================================
# FIGURE 3: Box Plots — Screen Time Variables by Gender
# Checks for gender-based differences in digital usage patterns
# =============================================================================
screen_vars <- df %>%
  select(gender, phone_usage_hours, social_media_hours, youtube_hours, gaming_hours) %>%
  pivot_longer(cols = -gender, names_to = "variable", values_to = "hours") %>%
  mutate(variable = recode(variable,
    phone_usage_hours   = "Phone Usage",
    social_media_hours  = "Social Media",
    youtube_hours       = "YouTube",
    gaming_hours        = "Gaming"
  ))

fig3 <- ggplot(screen_vars, aes(x = gender, y = hours, fill = gender)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.4, outlier.alpha = 0.3) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = c("Female" = "#F28E2B", "Male" = "#4E79A7", "Other" = "#59A14F")) +
  labs(title = "Figure 3: Screen Time Variables by Gender",
       subtitle = "Faceted box plots for each type of digital distraction",
       x = "Gender", y = "Hours per Day",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(fig3)


# =============================================================================
# FIGURE 4: Scatter Plot — Sleep Hours vs. Stress Level
# Examines the relationship between sleep deprivation and stress
# =============================================================================
fig4 <- ggplot(df, aes(x = sleep_hours, y = stress_level)) +
  geom_jitter(alpha = 0.07, color = "#E15759", size = 0.8, width = 0.1, height = 0.2) +
  geom_smooth(method = "lm", color = "#4E79A7", se = TRUE, linewidth = 1.1) +
  labs(title = "Figure 4: Sleep Hours vs. Stress Level",
       subtitle = "Each point = one student (jittered). Blue line = linear fit.",
       x = "Sleep Hours per Night", y = "Stress Level (1–10)",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12)

print(fig4)


# =============================================================================
# FIGURE 5: Correlation Heatmap — All Numeric Variables
# Overview of pairwise relationships; highlights surprising near-zero
# correlations (especially final_grade)
# =============================================================================
numeric_vars <- df %>%
  select(study_hours_per_day, sleep_hours, phone_usage_hours, social_media_hours,
         youtube_hours, gaming_hours, breaks_per_day, coffee_intake_mg,
         exercise_minutes, assignments_completed, attendance_percentage,
         stress_level, focus_score, final_grade, productivity_score)

cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Use corrplot for a clean heatmap
corrplot(cor_matrix,
         method   = "color",
         type     = "lower",
         tl.cex   = 0.75,
         tl.col   = "black",
         addCoef.col = "black",
         number.cex  = 0.5,
         col      = colorRampPalette(c("#E15759", "white", "#4E79A7"))(200),
         title    = "Figure 5: Correlation Matrix of All Numeric Variables",
         mar      = c(0, 0, 2, 0))


# =============================================================================
# FIGURE 6: Bar Chart — Stress Level Distribution
# Shows the nearly uniform distribution of stress (1–10), which is surprising
# =============================================================================
stress_counts <- df %>%
  count(stress_level) %>%
  mutate(stress_level = factor(stress_level))

fig6 <- ggplot(stress_counts, aes(x = stress_level, y = n, fill = stress_level)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  geom_hline(yintercept = mean(stress_counts$n), linetype = "dashed",
             color = "black", linewidth = 0.7) +
  annotate("text", x = 0.7, y = mean(stress_counts$n) + 40,
           label = "Mean count", size = 3.2, hjust = 0) +
  labs(title = "Figure 6: Distribution of Self-Reported Stress Level",
       subtitle = "Stress is nearly uniformly distributed from 1 to 10 across all 20,000 students",
       x = "Stress Level (1 = Low, 10 = High)", y = "Number of Students",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(fig6)


# =============================================================================
# FIGURE 7: Scatter — Exercise Minutes vs. Productivity Score
# Checks whether exercise is a visible compensatory factor
# =============================================================================
fig7 <- ggplot(df, aes(x = exercise_minutes, y = productivity_score)) +
  geom_point(alpha = 0.08, color = "#59A14F", size = 0.9) +
  geom_smooth(method = "lm", color = "#E15759", se = TRUE, linewidth = 1.1) +
  labs(title = "Figure 7: Exercise Minutes vs. Productivity Score",
       subtitle = "Preliminary check on whether physical activity compensates for screen time effects",
       x = "Exercise (minutes/day)", y = "Productivity Score",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12)

print(fig7)


# =============================================================================
# FIGURE 8: Box Plots — Productivity Score by High vs. Low Screen Time
# Directly visualizes the productivity gap between screen time groups
# =============================================================================
df$screen_group <- ifelse(df$phone_usage_hours >= quantile(df$phone_usage_hours, 0.67),
                          "High Screen Time\n(top 33%)",
                          "Lower Screen Time\n(bottom 67%)")

fig8 <- ggplot(df, aes(x = screen_group, y = productivity_score, fill = screen_group)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("High Screen Time\n(top 33%)"   = "#E15759",
                                "Lower Screen Time\n(bottom 67%)" = "#4E79A7")) +
  labs(title = "Figure 8: Productivity Score by Screen Time Group",
       subtitle = "High screen-time students (≥ 8.2 hrs/day) show noticeably lower median productivity",
       x = "Screen Time Group", y = "Productivity Score",
       caption = "Source: Student Productivity & Behavior Dataset (20K)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(fig8)

# =============================================================================
# End of Script 1
# =============================================================================
cat("\nScript 1 complete. All 8 preliminary figures generated.\n")
