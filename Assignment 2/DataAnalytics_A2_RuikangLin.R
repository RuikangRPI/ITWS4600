library(class)
library(caret)
library(GGally)
library(psych)
library(readr)
library(EnvStats)
library(nortest)
library(dplyr)
setwd("~/Documents/GitHub/ITWS4600/Assignment 2")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")



### Variable Distribution

# 1.1 histogram of MKP.old with density lines
MKP           <- epi.data$MKP.old
MKP.complete  <- MKP[!is.na(MKP)]
summary(MKP.complete)
x <- seq(0, 100, 5)
hist(MKP.complete, x, prob = TRUE,
     main   = "Histogram of MKP.old (Full Dataset)",
     xlab   = "MKP.old",
     ylab   = "Density",
     col    = "lightblue",
     border = "white")
lines(density(MKP.complete), lwd = 2, col = "darkblue")
rug(MKP.complete)

# 1.2 boxplot of MKP.old
epi.data.mkp <- epi.data %>%
  filter(!is.na(MKP.old), !is.na(region))
boxplot(MKP.old ~ region, data = epi.data.mkp,
        main     = "MKP.old by Region",
        xlab     = "",
        ylab     = "MKP.old",
        col      = "lightblue",
        las      = 2,      # rotate x-axis labels vertical
        cex.axis = 0.65)   # smaller font so labels fit

# 2 setup for 2.1 and 2.2
region1_name <- "Eastern Europe"
region2_name <- "Latin America & Caribbean"
subset1 <- epi.data %>%
  filter(region == region1_name,
         !is.na(MKP.old), !is.na(population), !is.na(gdp))
subset2 <- epi.data %>%
  filter(region == region2_name,
         !is.na(MKP.old), !is.na(population), !is.na(gdp))
cat("Subset 1 (", region1_name, ") - rows:", nrow(subset1), "\n")
cat("Subset 2 (", region2_name, ") - rows:", nrow(subset2), "\n")

# 2.1 histograms of MKP.old by region
par(mfrow = c(1, 2))
hist(subset1$MKP.old,
     breaks = seq(0, 100, 10), prob = TRUE,
     main   = paste("MKP.old\n", region1_name),
     xlab   = "MKP.old", ylab = "Density",
     col    = "lightcoral", border = "white",
     xlim   = c(0, 100))
lines(density(subset1$MKP.old), lwd = 2, col = "darkred")
rug(subset1$MKP.old)
hist(subset2$MKP.old,
     breaks = seq(0, 100, 10), prob = TRUE,
     main   = paste("MKP.old\n", region2_name),
     xlab   = "MKP.old", ylab = "Density",
     col    = "lightgreen", border = "white",
     xlim   = c(0, 100))
lines(density(subset2$MKP.old), lwd = 2, col = "darkgreen")
rug(subset2$MKP.old)
par(mfrow = c(1, 1))

# 2.2 QQ plot comparing MKP.old between the 2 region subsets
n     <- min(nrow(subset1), nrow(subset2))
probs <- seq(0, 1, length.out = n)
q1    <- quantile(subset1$MKP.old, probs)
q2    <- quantile(subset2$MKP.old, probs)
plot(q1, q2,
     main = "QQ Plot of MKP.old:\nEastern Europe vs Latin America & Caribbean",
     xlab = paste("Quantiles -", region1_name),
     ylab = paste("Quantiles -", region2_name),
     pch  = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2, lty = 2)   # y = x reference line



### Linear Models

# 3 helper residual plot function for 3.1 and 3.2
plot_residuals <- function(model, title_prefix) {
  par(mfrow = c(1, 2))
  plot(model$fitted.values, model$residuals,
       main = paste(title_prefix, "\nResiduals vs Fitted"),
       xlab = "Fitted values", ylab = "Residuals",
       pch  = 16, col = "steelblue")
  abline(h = 0, col = "red", lwd = 1.5, lty = 2)
  
  qqnorm(model$residuals,
         main = paste(title_prefix, "\nNormal Q-Q of Residuals"),
         pch  = 16, col = "steelblue")
  qqline(model$residuals, col = "red", lwd = 1.5)
  par(mfrow = c(1, 1))
}

# 3.1  Population & GDP vs MKP.old with log transforms
# Eastern Europe
par(mfrow = c(1, 2))
plot(log(subset1$population), subset1$MKP.old,
     main = paste(region1_name, "\nlog(Population) vs MKP.old"),
     xlab = "log(Population)", ylab = "MKP.old",
     pch  = 16, col = "coral")
abline(lm(MKP.old ~ log(population), data = subset1), col = "darkred", lwd = 2)
plot(log(subset1$gdp), subset1$MKP.old,
     main = paste(region1_name, "\nlog(GDP) vs MKP.old"),
     xlab = "log(GDP)", ylab = "MKP.old",
     pch  = 16, col = "coral")
abline(lm(MKP.old ~ log(gdp), data = subset1), col = "darkred", lwd = 2)
par(mfrow = c(1, 1))

# Latin America & Caribbean
par(mfrow = c(1, 2))
plot(log(subset2$population), subset2$MKP.old,
     main = paste(region2_name, "\nlog(Population) vs MKP.old"),
     xlab = "log(Population)", ylab = "MKP.old",
     pch  = 16, col = "seagreen")
abline(lm(MKP.old ~ log(population), data = subset2), col = "darkgreen", lwd = 2)
plot(log(subset2$gdp), subset2$MKP.old,
     main = paste(region2_name, "\nlog(GDP) vs MKP.old"),
     xlab = "log(GDP)", ylab = "MKP.old",
     pch  = 16, col = "seagreen")
abline(lm(MKP.old ~ log(gdp), data = subset2), col = "darkgreen", lwd = 2)
par(mfrow = c(1, 1))

# 3.2  Fit 2 linear models per region
#      Model A: MKP.old ~ log(gdp)              (single predictor)
#      Model B: MKP.old ~ log(gdp) + log(pop)   (two predictors)

# Eastern Europe
cat("\n===== SUBSET 1:", region1_name, "=====\n")
lm1A <- lm(MKP.old ~ log(gdp), data = subset1)
cat("\n-- Model 1A: MKP.old ~ log(gdp) --\n")
print(summary(lm1A))
plot_residuals(lm1A, paste(region1_name, "- Model A"))
lm1B <- lm(MKP.old ~ log(gdp) + log(population), data = subset1)
cat("\n-- Model 1B: MKP.old ~ log(gdp) + log(pop) --\n")
print(summary(lm1B))
plot_residuals(lm1B, paste(region1_name, "- Model B"))

# Latin America & Caribbean
cat("\n===== SUBSET 2:", region2_name, "=====\n")
lm2A <- lm(MKP.old ~ log(gdp), data = subset2)
cat("\n-- Model 2A: MKP.old ~ log(gdp) --\n")
print(summary(lm2A))
plot_residuals(lm2A, paste(region2_name, "- Model A"))
lm2B <- lm(MKP.old ~ log(gdp) + log(population), data = subset2)
cat("\n-- Model 2B: MKP.old ~ log(gdp) + log(pop) --\n")
print(summary(lm2B))
plot_residuals(lm2B, paste(region2_name, "- Model B"))

# 3.3 model comparison - 
# The model with the higher Adjusted R^2 is the better fit because Adjusted R^2
# penalizes for additional predictors. For Eastern Europe, GDP alone explains
# most variance in MKP.old. For Latin America & Caribbean which has a highly variable
# country sizes, adding log(population) may improve fit. The best model overall is
# the one with the highest Adjusted R^2 and residuals that appear normally
# distributed with no pattern against fitted values.
cat("\n===== 3.3 MODEL COMPARISON (Adjusted R^2) =====\n")
cat(sprintf("  %-55s Adj-R^2 = %.4f\n",
            paste(region1_name, "Model A [log(gdp)]:"),
            summary(lm1A)$adj.r.squared))
cat(sprintf("  %-55s Adj-R^2 = %.4f\n",
            paste(region1_name, "Model B [log(gdp) + log(pop)]:"),
            summary(lm1B)$adj.r.squared))
cat(sprintf("  %-55s Adj-R^2 = %.4f\n",
            paste(region2_name, "Model A [log(gdp)]:"),
            summary(lm2A)$adj.r.squared))
cat(sprintf("  %-55s Adj-R^2 = %.4f\n",
            paste(region2_name, "Model B [log(gdp) + log(pop)]:"),
            summary(lm2B)$adj.r.squared))



### Classification - kNN

# 4 subset Eastern Europe, Latin America & Caribbean, and Global West
region3_name <- "Global West"
knn.data <- epi.data %>%
  filter(region %in% c(region1_name, region2_name, region3_name),
         !is.na(MKP.old), !is.na(population), !is.na(gdp)) %>%
  select(region, population, gdp, MKP.old)
cat("\nkNN subset rows:", nrow(knn.data), "\n")
print(table(knn.data$region))
# Log-transform population and GDP to reduce skewness
knn.data <- knn.data %>%
  mutate(log_pop = log(population),
         log_gdp = log(gdp))

# 4.1 kNN Model 1: log(pop) + log(gdp) + MKP.old
features1        <- knn.data %>% select(log_pop, log_gdp, MKP.old)
features1_scaled <- as.data.frame(scale(features1))
labels           <- as.factor(knn.data$region)
train_idx       <- createDataPartition(labels, p = 0.75, list = FALSE)
train_feat1     <- features1_scaled[train_idx, ]
test_feat1      <- features1_scaled[-train_idx, ]
train_labels    <- labels[train_idx]
test_labels     <- labels[-train_idx]
cat("\n===== kNN MODEL 1: log_pop + log_gdp + MKP.old =====\n")
best_acc1 <- 0
best_k1   <- NA

for (k in c(3, 5, 7)) {
  pred <- knn(train = train_feat1, test = test_feat1, cl = train_labels, k = k)
  acc  <- sum(pred == test_labels) / length(test_labels)
  cat(sprintf("  k = %d  Accuracy = %.4f\n", k, acc))
  if (acc > best_acc1) { best_acc1 <- acc; best_k1 <- k }
}

cat("\nBest k for Model 1:", best_k1, " | Accuracy:", round(best_acc1, 4), "\n")
best_pred1 <- knn(train = train_feat1, test = test_feat1,
                  cl = train_labels, k = best_k1)

cat("\n-- Confusion Matrix (Model 1, k =", best_k1, ") --\n")
cm1 <- confusionMatrix(best_pred1, test_labels)
print(cm1)

cat(sprintf("\nModel 1 Accuracy = correctly classified / total = %d / %d = %.4f\n",
            sum(best_pred1 == test_labels), length(test_labels), best_acc1))

# 4.2 kNN Model 2: log(pop) + log(gdp) + EPI.old  with same k
knn.data2 <- epi.data %>%
  filter(region %in% c(region1_name, region2_name, region3_name),
         !is.na(EPI.old), !is.na(population), !is.na(gdp)) %>%
  select(region, population, gdp, EPI.old) %>%
  mutate(log_pop = log(population),
         log_gdp = log(gdp))

features2        <- knn.data2 %>% select(log_pop, log_gdp, EPI.old)
features2_scaled <- as.data.frame(scale(features2))
labels2          <- as.factor(knn.data2$region)
train_idx2   <- createDataPartition(labels2, p = 0.75, list = FALSE)
train_feat2  <- features2_scaled[train_idx2, ]
test_feat2   <- features2_scaled[-train_idx2, ]
train_labels2 <- labels2[train_idx2]
test_labels2  <- labels2[-train_idx2]
best_pred2 <- knn(train = train_feat2, test = test_feat2,
                  cl = train_labels2, k = best_k1)
acc2 <- sum(best_pred2 == test_labels2) / length(test_labels2)

cat("\n===== kNN MODEL 2: log_pop + log_gdp + EPI.old (k =", best_k1, ") =====\n")
cm2 <- confusionMatrix(best_pred2, test_labels2)
print(cm2)

cat(sprintf("\nModel 2 Accuracy = %d / %d = %.4f\n",
            sum(best_pred2 == test_labels2), length(test_labels2), acc2))

# 4.3 final comparison summary
# Model 2 using EPI.old outperformed Model 1 using MKP.old
# because EPI.old is the overall Environmental Performance Index, which captures
# broad, systemic differences across regions, whereas MKP.old, or Marine
# Protected Areas, is a narrower, noisier signal that is less informative
# for distinguishing inland-dominated regions such as Eastern Europe.
cat("\n===== 4.3 kNN MODEL COMPARISON =====\n")
cat(sprintf("  Model 1 [log_pop, log_gdp, MKP.old] - best accuracy (k=%d): %.4f\n",
            best_k1, best_acc1))
cat(sprintf("  Model 2 [log_pop, log_gdp, EPI.old]  - accuracy     (k=%d): %.4f\n",
            best_k1, acc2))
