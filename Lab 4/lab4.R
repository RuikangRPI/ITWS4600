# Lab 4
# Ruikang Lin
# Used Claude for cleaning documentation and adding pretty prints

## ── Libraries ──────────────────────────────────────────────────────────────
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

## ── 1. Load & Prepare Data ─────────────────────────────────────────────────
setwd("~/Documents/GitHub/ITWS4600/Lab 4")
wine <- read_csv("wine.data", col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####
wine$Type <- as.factor(wine$Type)

## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)
ggpairs(wine, ggplot2::aes(colour = Type))
###
X <- wine[,-1]
Y <- wine$Type
###

cat("── Dataset dimensions:", nrow(wine), "rows ×", ncol(wine), "cols\n")
cat("── Class distribution:\n"); print(table(Y))


## ── 2. PCA ──────────────────────────────────────────────────────────────────
# Scale = TRUE is essential: variables are on very different scales
pca <- princomp(X, cor = TRUE)   # cor=TRUE  ⟺  scale the data

cat("\n── Variance explained by each PC:\n")
summary(pca)

# Scree plot
scree_df <- data.frame(
  PC       = seq_along(pca$sdev),
  Variance = (pca$sdev^2) / sum(pca$sdev^2) * 100
)

p_scree <- ggplot(scree_df, aes(x = PC, y = Variance)) +
  geom_col(fill = "#4E79A7") +
  geom_line(aes(group = 1), colour = "grey30") +
  geom_point(colour = "grey30") +
  labs(title = "Scree Plot – Wine PCA",
       x = "Principal Component", y = "% Variance Explained") +
  theme_minimal()
print(p_scree)


## ── 3. Plot Dataset on PC1 vs PC2 ───────────────────────────────────────────
scores <- as.data.frame(pca$scores)
scores$Type <- Y

p_pc12 <- ggplot(scores, aes(x = Comp.1, y = Comp.2, colour = Type, shape = Type)) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_colour_manual(values = c("1" = "#E15759", "2" = "#76B7B2", "3" = "#F28E2B")) +
  labs(title = "Wine Dataset Projected onto PC1 & PC2",
       x = "PC1", y = "PC2") +
  theme_minimal()
print(p_pc12)


## ── 4. Variables Contributing Most to PC1 ──────────────────────────────────
# Loadings = eigenvectors; absolute value ↔ contribution strength
loadings_pc1 <- pca$loadings[, 1]

loading_df <- data.frame(
  Variable = names(loadings_pc1),
  Loading  = as.numeric(loadings_pc1)
) |>
  dplyr::arrange(dplyr::desc(abs(Loading)))

cat("\n── PC1 Loadings (sorted by |loading|):\n")
print(loading_df)

p_load <- ggplot(loading_df,
                 aes(x = reorder(Variable, abs(Loading)), y = Loading, fill = Loading > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#4E79A7", "FALSE" = "#E15759")) +
  labs(title = "PC1 Loadings – Wine Variables",
       x = NULL, y = "Loading") +
  theme_minimal()
print(p_load)

# Top 4 contributors to PC1
top4 <- loading_df$Variable[1:4]
cat("\n── Top-4 contributors to PC1:", paste(top4, collapse = ", "), "\n")


## ── 5. Train / Test Split (80 / 20, stratified) ─────────────────────────────
set.seed(42)
train_idx <- createDataPartition(Y, p = 0.80, list = FALSE)

# --- Model A: kNN on the top-4 original variables ---
X_top4       <- X[, top4]
X_top4_train <- X_top4[train_idx, ]
X_top4_test  <- X_top4[-train_idx, ]

# --- Model B: kNN on PC1 + PC2 scores ---
X_pc2       <- scores[, c("Comp.1", "Comp.2")]
X_pc2_train <- X_pc2[train_idx, ]
X_pc2_test  <- X_pc2[-train_idx, ]

Y_train <- Y[train_idx]
Y_test  <- Y[-train_idx]

# Scale each feature set (kNN is distance-based → must scale)
scale_transform <- function(train, test) {
  m   <- colMeans(train)
  s   <- apply(train, 2, sd)
  list(
    train = scale(train, center = m, scale = s),
    test  = scale(test,  center = m, scale = s)
  )
}

sc_a <- scale_transform(X_top4_train, X_top4_test)
sc_b <- scale_transform(X_pc2_train,  X_pc2_test)


## ── 6. Choose k via 5-fold CV on training set ───────────────────────────────
choose_k <- function(train_x, train_y, k_range = 1:15) {
  acc <- numeric(length(k_range))
  for (i in seq_along(k_range)) {
    pred <- knn.cv(train_x, train_y, k = k_range[i])
    acc[i] <- mean(pred == train_y)
  }
  k_range[which.max(acc)]
}

best_k_a <- choose_k(sc_a$train, Y_train)
best_k_b <- choose_k(sc_b$train, Y_train)
cat("\n── Best k (Model A – top-4 vars):", best_k_a)
cat("\n── Best k (Model B – PC1+PC2):    ", best_k_b, "\n")


## ── 7. Predictions ──────────────────────────────────────────────────────────
pred_a <- knn(sc_a$train, sc_a$test, Y_train, k = best_k_a)
pred_b <- knn(sc_b$train, sc_b$test, Y_train, k = best_k_b)


## ── 8. Contingency Tables ────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════")
cat("\n  MODEL A  –  kNN on Top-4 Original Variables\n")
cat("══════════════════════════════════════════════════\n")
cm_a <- confusionMatrix(pred_a, Y_test, mode = "prec_recall")
print(cm_a$table)

cat("\n══════════════════════════════════════════════════")
cat("\n  MODEL B  –  kNN on PC1 + PC2 Scores\n")
cat("══════════════════════════════════════════════════\n")
cm_b <- confusionMatrix(pred_b, Y_test, mode = "prec_recall")
print(cm_b$table)


## ── 9. Precision / Recall / F1 per Class ────────────────────────────────────
extract_metrics <- function(cm, model_name) {
  tbl <- cm$byClass[, c("Precision", "Recall", "F1")]
  df  <- as.data.frame(tbl)
  df$Class <- rownames(df)
  df$Model  <- model_name
  df
}

metrics_a <- extract_metrics(cm_a, "Model A (top-4 vars)")
metrics_b <- extract_metrics(cm_b, "Model B (PC1+PC2)")
metrics   <- rbind(metrics_a, metrics_b)
rownames(metrics) <- NULL

cat("\n── Per-class Precision / Recall / F1:\n")
print(metrics[, c("Model", "Class", "Precision", "Recall", "F1")])

# Overall accuracy
cat(sprintf("\n── Overall Accuracy  Model A: %.1f%%\n",
            cm_a$overall["Accuracy"] * 100))
cat(sprintf("── Overall Accuracy  Model B: %.1f%%\n",
            cm_b$overall["Accuracy"] * 100))

# Macro-averaged F1
macro_f1 <- function(cm) mean(cm$byClass[, "F1"], na.rm = TRUE)
cat(sprintf("── Macro F1          Model A: %.3f\n", macro_f1(cm_a)))
cat(sprintf("── Macro F1          Model B: %.3f\n", macro_f1(cm_b)))


## ── 10. Comparison Bar Plot ──────────────────────────────────────────────────
metrics_long <- tidyr::pivot_longer(
  metrics,
  cols      = c(Precision, Recall, F1),
  names_to  = "Metric",
  values_to = "Value"
)

p_compare <- ggplot(metrics_long,
                    aes(x = Class, y = Value, fill = Model)) +
  geom_col(position = "dodge", colour = "white", linewidth = 0.3) +
  facet_wrap(~ Metric) +
  scale_fill_manual(values = c(
    "Model A (top-4 vars)" = "#4E79A7",
    "Model B (PC1+PC2)"    = "#F28E2B"
  )) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(title = "Model A vs Model B – Precision / Recall / F1 by Class",
       x = "Wine Type", y = "Score", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_compare)
