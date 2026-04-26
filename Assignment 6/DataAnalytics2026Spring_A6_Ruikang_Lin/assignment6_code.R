# =============================================================================
# Predictive and Prescriptive Analytics – Absenteeism at Work
# Models: Random Forest Regression, Random Forest Classification, K-Means Clustering
# Author: Ruikang Lin
# Note: Claude and CoPilot are used to assist in code generation and figure design.
# =============================================================================

# ── 0. Load libraries ─────────────────────────────────────────────────────────
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(randomForest)
library(caret)
library(cluster)
library(factoextra)
library(gridExtra)
library(corrplot)

setwd("Assignment 6")
set.seed(42)

# ── 1. Load data ──────────────────────────────────────────────────────────────
df <- read.csv("Absenteeism_at_work.csv", sep = ";", stringsAsFactors = FALSE)
colnames(df) <- trimws(colnames(df))  # removes trailing space on "Absenteeism.time.in.hours "

cat("Dataset dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Missing values:", sum(is.na(df)), "\n")
summary(df$Absenteeism.time.in.hours)

# ── 2. Exploratory Data Analysis ─────────────────────────────────────────────

# 2a. Outlier removal using 3×IQR rule
target <- "Absenteeism.time.in.hours"
Q1  <- quantile(df[[target]], 0.25)
Q3  <- quantile(df[[target]], 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 3 * IQR
df_clean <- df[df[[target]] <= upper_bound, ]
cat("After outlier removal:", nrow(df_clean), "rows (removed:", nrow(df) - nrow(df_clean), ")\n")

# 2b. Distribution histogram
p1 <- ggplot(df_clean, aes_string(x = target)) +
  geom_histogram(bins = 30, fill = "#2C5F8A", color = "white") +
  geom_vline(aes(xintercept = mean(df_clean[[target]])),
             color = "#E8863A", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Absenteeism Hours",
       x = "Hours Absent", y = "Frequency") +
  theme_minimal()

# 2c. Boxplot by Season
season_map <- c("1" = "Summer", "2" = "Autumn", "3" = "Winter", "4" = "Spring")
df_clean$Season_lbl <- season_map[as.character(df_clean$Seasons)]

p2 <- ggplot(df_clean, aes(x = Season_lbl, y = .data[[target]], fill = Season_lbl)) +
  geom_boxplot(alpha = 0.75, outlier.size = 1) +
  scale_fill_manual(values = c("Summer"="#2C5F8A","Autumn"="#E8863A",
                                "Winter"="#4CAF7D","Spring"="#C0392B")) +
  labs(title = "Absenteeism by Season", x = "", y = "Hours Absent") +
  theme_minimal() + theme(legend.position = "none")

# 2d. Correlation heatmap
key_vars <- c("Age", "Transportation.expense",
              "Distance.from.Residence.to.Work", "Service.time",
              "Work.load.Average.day", "Body.mass.index",
              "Social.drinker", "Reason.for.absence",
              "Absenteeism.time.in.hours")
corr_mat <- cor(df_clean[, key_vars], use = "complete.obs")
corrplot(corr_mat, method = "color", type = "lower",
         tl.cex = 0.7, addCoef.col = "black", number.cex = 0.6,
         col = colorRampPalette(c("#C0392B","white","#2C5F8A"))(200),
         title = "Correlation Matrix", mar = c(0,0,1,0))

# ── 3. Feature & Target Setup ─────────────────────────────────────────────────
features <- c("Reason.for.absence", "Month.of.absence",
              "Transportation.expense", "Distance.from.Residence.to.Work",
              "Service.time", "Age", "Work.load.Average.day",
              "Hit.target", "Disciplinary.failure", "Education",
              "Son", "Social.drinker", "Social.smoker", "Pet",
              "Weight", "Height", "Body.mass.index")

X     <- df_clean[, features]
y_reg <- df_clean[[target]]
q75   <- quantile(y_reg, 0.75)
y_cls <- factor(ifelse(y_reg >= q75, "High", "Low"), levels = c("Low","High"))

cat("Classification threshold (Q75):", q75, "hours\n")
cat("High absenteeism:", sum(y_cls == "High"),
    sprintf("(%.1f%%)\n", mean(y_cls == "High") * 100))

# ── 4. PCA – Dimension Reduction ─────────────────────────────────────────────
X_scaled <- scale(X)
pca_res  <- prcomp(X_scaled, center = FALSE, scale. = FALSE)
cum_var  <- cumsum(pca_res$sdev^2) / sum(pca_res$sdev^2)
n_comp   <- which(cum_var >= 0.85)[1]
cat("PCA: need", n_comp, "components for 85% variance\n")

X_pca <- predict(pca_res, X_scaled)[, 1:n_comp]

# Scree plot
scree_df <- data.frame(
  PC  = 1:length(pca_res$sdev),
  Var = pca_res$sdev^2 / sum(pca_res$sdev^2) * 100,
  CumVar = cum_var * 100
)
p_scree <- ggplot(scree_df, aes(x = PC)) +
  geom_col(aes(y = Var), fill = "#2C5F8A", alpha = 0.8) +
  geom_line(aes(y = CumVar), color = "#E8863A", size = 1) +
  geom_point(aes(y = CumVar), color = "#E8863A", size = 3) +
  geom_hline(yintercept = 85, linetype = "dashed", color = "#C0392B") +
  labs(title = "PCA – Explained Variance", x = "Principal Component",
       y = "Variance Explained (%)") +
  theme_minimal()
print(p_scree)

# ── 5. Model 1: Random Forest Regression ─────────────────────────────────────
train_idx <- createDataPartition(y_reg, p = 0.8, list = FALSE)

# Full features
X_tr <- X[train_idx, ];  X_te <- X[-train_idx, ]
yr_tr <- y_reg[train_idx]; yr_te <- y_reg[-train_idx]

rf_reg <- randomForest(x = X_tr, y = yr_tr,
                       ntree = 200, mtry = floor(ncol(X_tr)/3),
                       importance = TRUE)
yr_pred <- predict(rf_reg, X_te)
rmse_full <- sqrt(mean((yr_te - yr_pred)^2))
r2_full   <- 1 - sum((yr_te - yr_pred)^2) / sum((yr_te - mean(yr_te))^2)
cat(sprintf("RF Regression (full): RMSE = %.2f, R² = %.3f\n", rmse_full, r2_full))

# PCA features
Xp_tr <- X_pca[train_idx, ]; Xp_te <- X_pca[-train_idx, ]
rf_reg_pca <- randomForest(x = Xp_tr, y = yr_tr, ntree = 200)
yr_pred_pca <- predict(rf_reg_pca, Xp_te)
rmse_pca  <- sqrt(mean((yr_te - yr_pred_pca)^2))
r2_pca    <- 1 - sum((yr_te - yr_pred_pca)^2) / sum((yr_te - mean(yr_te))^2)
cat(sprintf("RF Regression (PCA):  RMSE = %.2f, R² = %.3f\n", rmse_pca, r2_pca))

# 5-fold CV
cv_reg <- train(X, y_reg, method = "rf",
                trControl = trainControl(method = "cv", number = 5),
                tuneGrid = data.frame(mtry = floor(ncol(X)/3)),
                ntree = 100)
cat("CV R²:", round(max(cv_reg$results$Rsquared), 3), "\n")

# Actual vs Predicted plot
pred_df <- data.frame(Actual = yr_te, Predicted = yr_pred)
p_reg <- ggplot(pred_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#2C5F8A", alpha = 0.5, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "#C0392B", linetype = "dashed", size = 1) +
  labs(title = sprintf("RF Regression: Actual vs Predicted\nR²=%.3f, RMSE=%.2f", r2_full, rmse_full),
       x = "Actual Hours", y = "Predicted Hours") +
  theme_minimal()
print(p_reg)

# Feature importance
varImpPlot(rf_reg, type = 1, main = "RF Regression – Variable Importance", n.var = 10)

# ── 6. Model 2: Random Forest Classification ──────────────────────────────────
yc_tr <- y_cls[train_idx]; yc_te <- y_cls[-train_idx]

rf_cls <- randomForest(x = X_tr, y = yc_tr,
                       ntree = 200, importance = TRUE,
                       classwt = c(Low = 1, High = 2))
yc_pred <- predict(rf_cls, X_te)
acc_full <- mean(yc_pred == yc_te)
cat(sprintf("RF Classification (full): Accuracy = %.3f\n", acc_full))
print(confusionMatrix(yc_pred, yc_te))

# PCA version
rf_cls_pca <- randomForest(x = Xp_tr, y = yc_tr, ntree = 200)
yc_pred_pca <- predict(rf_cls_pca, Xp_te)
acc_pca <- mean(yc_pred_pca == yc_te)
cat(sprintf("RF Classification (PCA):  Accuracy = %.3f\n", acc_pca))

# 5-fold CV
cv_cls <- train(X, y_cls, method = "rf",
                trControl = trainControl(method = "cv", number = 5),
                tuneGrid = data.frame(mtry = floor(sqrt(ncol(X)))),
                ntree = 100)
cat("CV Accuracy:", round(max(cv_cls$results$Accuracy), 3), "\n")

# ── 7. Model 3: K-Means Clustering ───────────────────────────────────────────
clust_vars <- c("Reason.for.absence","Transportation.expense","Age",
                "Work.load.Average.day","Body.mass.index",
                "Social.drinker", "Absenteeism.time.in.hours")
X_clust_s <- scale(df_clean[, clust_vars])

# Elbow + Silhouette
sil_scores <- numeric(7)
for (k in 2:8) {
  km   <- kmeans(X_clust_s, centers = k, nstart = 10)
  sil  <- silhouette(km$cluster, dist(X_clust_s))
  sil_scores[k - 1] <- mean(sil[, 3])
}
best_k <- which.max(sil_scores) + 1
cat("Optimal K:", best_k, "| Silhouette:", round(max(sil_scores), 3), "\n")

km_final <- kmeans(X_clust_s, centers = best_k, nstart = 10)
df_clean$Cluster <- as.factor(km_final$cluster)

# Cluster profiles
cluster_profile <- df_clean %>%
  group_by(Cluster) %>%
  summarise(across(all_of(clust_vars), mean, .names = "{.col}"), .groups = "drop")
cluster_profile[, -1] <- round(cluster_profile[, -1], 2)
print(cluster_profile)

# PCA visualisation of clusters
fviz_cluster(km_final, data = X_clust_s,
             geom = "point", ellipse = TRUE,
             palette = c("#2C5F8A","#E8863A","#4CAF7D","#C0392B",
                         "#7F8C8D","#8E44AD","#27AE60","#E74C3C"),
             ggtheme = theme_minimal(),
             main = "K-Means Cluster Plot (PCA view)")

cat("\n=== Analysis Complete ===\n")
