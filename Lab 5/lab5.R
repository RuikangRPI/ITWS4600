# Lab 5
# Ruikang Lin
# Used Claude for cleaning documentation and adding pretty prints

## ── Libraries ──────────────────────────────────────────────────────────────
library(ggplot2)
library(e1071)       # svm, tune.svm
library(class)       # knn (comparison classifier)
library(caret)       # createDataPartition, confusionMatrix
library(readr)
library(tidyr)
library(dplyr)

## ── 1. Load & Prepare Data ─────────────────────────────────────────────────
setwd("~/Documents/GitHub/ITWS4600/Lab 5")

wine <- read_csv("wine.data", col_names = FALSE)
names(wine) <- c(
  "Type", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash",
  "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid Phenols",
  "Proanthocyanins", "Color Intensity", "Hue",
  "Od280/od315 of diluted wines", "Proline"
)
wine$Type <- as.factor(wine$Type)

# ── Feature selection ──────────────────────────────────────────────────────
# From Lab 4 PCA: top-4 contributors to PC1 (highest |loading|)
#   Flavanoids, Total phenols, Od280/od315 of diluted wines, Proline
FEATURES <- c("Flavanoids", "Total phenols", "Od280/od315 of diluted wines", "Proline")

X <- wine[, FEATURES]
Y <- wine$Type

cat("── Dataset:", nrow(wine), "rows,", length(FEATURES), "features\n")
cat("── Features used:", paste(FEATURES, collapse = ", "), "\n")
cat("── Class distribution:\n"); print(table(Y))


## ── 2. Train / Test Split (80 / 20, stratified) ─────────────────────────────
set.seed(42)
train_idx <- createDataPartition(Y, p = 0.80, list = FALSE)

X_train <- X[ train_idx, ]
X_test  <- X[-train_idx, ]
Y_train <- Y[ train_idx]
Y_test  <- Y[-train_idx]

# Scale (essential for SVM and kNN)
scale_params <- list(center = colMeans(X_train), scale = apply(X_train, 2, sd))
X_train_sc   <- scale(X_train, center = scale_params$center, scale = scale_params$scale)
X_test_sc    <- scale(X_test,  center = scale_params$center, scale = scale_params$scale)

cat("\n── Train size:", nrow(X_train), " | Test size:", nrow(X_test), "\n")


## ── 3. Tune SVM – Linear Kernel ─────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════\n")
cat("  TUNING: SVM Linear Kernel\n")
cat("══════════════════════════════════════════════════\n")

set.seed(42)
tune_linear <- tune.svm(
  x          = X_train_sc,
  y          = Y_train,
  kernel     = "linear",
  cost       = c(0.01, 0.1, 1, 10, 100),
  tunecontrol = tune.control(cross = 5)
)

cat("Best parameters (linear):\n"); print(tune_linear$best.parameters)
cat("Best CV error (linear):", round(tune_linear$best.performance, 4), "\n")

svm_linear <- tune_linear$best.model


## ── 4. Tune SVM – RBF (Radial) Kernel ──────────────────────────────────────
cat("\n══════════════════════════════════════════════════\n")
cat("  TUNING: SVM RBF (Radial) Kernel\n")
cat("══════════════════════════════════════════════════\n")

set.seed(42)
tune_rbf <- tune.svm(
  x           = X_train_sc,
  y           = Y_train,
  kernel      = "radial",
  cost        = c(0.1, 1, 10, 100),
  gamma       = c(0.01, 0.1, 0.5, 1),
  tunecontrol = tune.control(cross = 5)
)

cat("Best parameters (RBF):\n"); print(tune_rbf$best.parameters)
cat("Best CV error (RBF):", round(tune_rbf$best.performance, 4), "\n")

svm_rbf <- tune_rbf$best.model


## ── 5. Comparison Classifier: k-Nearest Neighbours ──────────────────────────
# Reuse the CV k-selection approach from Lab 4
cat("\n══════════════════════════════════════════════════\n")
cat("  TUNING: kNN (choosing k by 5-fold LOO-CV)\n")
cat("══════════════════════════════════════════════════\n")

set.seed(42)
k_acc <- sapply(1:20, function(k) {
  pred <- knn.cv(X_train_sc, Y_train, k = k)
  mean(pred == Y_train)
})
best_k <- which.max(k_acc)
cat("Best k:", best_k, " | CV Accuracy:", round(max(k_acc), 4), "\n")


## ── 6. Predictions on Test Set ──────────────────────────────────────────────
pred_linear <- predict(svm_linear, X_test_sc)
pred_rbf    <- predict(svm_rbf,    X_test_sc)
pred_knn    <- knn(X_train_sc, X_test_sc, Y_train, k = best_k)


## ── 7. Confusion Matrices ────────────────────────────────────────────────────
cm_linear <- confusionMatrix(pred_linear, Y_test, mode = "prec_recall")
cm_rbf    <- confusionMatrix(pred_rbf,    Y_test, mode = "prec_recall")
cm_knn    <- confusionMatrix(pred_knn,    Y_test, mode = "prec_recall")

cat("\n══════════════════════════════════════════════════\n")
cat("  SVM LINEAR – Confusion Matrix\n")
cat("══════════════════════════════════════════════════\n")
print(cm_linear$table)

cat("\n══════════════════════════════════════════════════\n")
cat("  SVM RBF – Confusion Matrix\n")
cat("══════════════════════════════════════════════════\n")
print(cm_rbf$table)

cat("\n══════════════════════════════════════════════════\n")
cat("  kNN (k =", best_k, ") – Confusion Matrix\n")
cat("══════════════════════════════════════════════════\n")
print(cm_knn$table)


## ── 8. Per-class Precision / Recall / F1 ────────────────────────────────────
extract_metrics <- function(cm, model_name) {
  df <- as.data.frame(cm$byClass[, c("Precision", "Recall", "F1")])
  df$Class <- sub("Class: ", "", rownames(df))
  df$Model  <- model_name
  rownames(df) <- NULL
  df
}

metrics_linear <- extract_metrics(cm_linear, "SVM Linear")
metrics_rbf    <- extract_metrics(cm_rbf,    "SVM RBF")
metrics_knn    <- extract_metrics(cm_knn,    sprintf("kNN (k=%d)", best_k))

metrics_all <- rbind(metrics_linear, metrics_rbf, metrics_knn)

cat("\n── Per-class Precision / Recall / F1:\n")
print(metrics_all[, c("Model", "Class", "Precision", "Recall", "F1")])

# Macro-averaged F1 & overall accuracy
macro_f1 <- function(cm) round(mean(cm$byClass[, "F1"], na.rm = TRUE), 4)
acc       <- function(cm) round(cm$overall["Accuracy"] * 100, 1)

cat(sprintf("\n── Overall Accuracy  SVM Linear : %.1f%%  |  Macro F1: %.4f\n",
            acc(cm_linear), macro_f1(cm_linear)))
cat(sprintf("── Overall Accuracy  SVM RBF    : %.1f%%  |  Macro F1: %.4f\n",
            acc(cm_rbf), macro_f1(cm_rbf)))
cat(sprintf("── Overall Accuracy  kNN (k=%d)  : %.1f%%  |  Macro F1: %.4f\n",
            best_k, acc(cm_knn), macro_f1(cm_knn)))


## ── 9. Visualisation: Precision / Recall / F1 by Model & Class ──────────────
metrics_long <- pivot_longer(
  metrics_all,
  cols      = c(Precision, Recall, F1),
  names_to  = "Metric",
  values_to = "Value"
)

knn_label  <- sprintf("kNN (k=%d)", best_k)
colour_map <- c("SVM Linear" = "#4E79A7", "SVM RBF" = "#59A14F")
colour_map[knn_label] <- "#F28E2B"

p_compare <- ggplot(metrics_long,
                    aes(x = Class, y = Value, fill = Model)) +
  geom_col(position = "dodge", colour = "white", linewidth = 0.3) +
  facet_wrap(~ Metric) +
  scale_fill_manual(values = colour_map) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(
    title = "SVM Linear vs SVM RBF vs kNN – Precision / Recall / F1 by Class",
    x     = "Wine Type",
    y     = "Score",
    fill  = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_compare)


## ── 10. Visualisation: tune.svm Heatmap (RBF only – has gamma) ───────────────
rbf_perf <- tune_rbf$performances

p_heatmap <- ggplot(rbf_perf, aes(x = factor(cost), y = factor(gamma), fill = error)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(error, 3)), size = 3) +
  scale_fill_gradient(low = "#59A14F", high = "#E15759") +
  labs(
    title = "tune.svm – RBF Kernel: 5-Fold CV Error Grid",
    x     = "Cost (C)",
    y     = "Gamma",
    fill  = "CV Error"
  ) +
  theme_minimal()

print(p_heatmap)