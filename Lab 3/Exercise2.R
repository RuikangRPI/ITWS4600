library(class)
library(caret)
library(GGally)
library(psych)
library(cluster)
library(factoextra)
setwd("~/Documents/GitHub/ITWS4600/Lab 3")

# prep
abalone <- read.csv("./abalone/abalone.data", header = FALSE)
colnames(abalone) <- c("sex", "length", "diameter", "height",
                       "whole_weight", "shucked_wieght",
                       "viscera_wieght", "shell_weight", "rings")
abalone$age.group <- cut(abalone$rings,
                         br = c(0, 8, 11, 35),
                         labels = c("young", "adult", "old"))
abalone.sub <- abalone[, c(2:8, 10)]
abalone.sub$age.group <- as.factor(as.character(abalone.sub$age.group))
set.seed(6)
train.indexes <- sample(4177, 0.7 * 4177)
train <- abalone.sub[train.indexes, ]
test  <- abalone.sub[-train.indexes, ]

# feature subset: weight-based
X <- abalone.sub[, 4:7]

# scale features for clustering
X.scaled <- scale(X)



# kmeans
k.values <- 2:10
sil.kmeans <- numeric(length(k.values))

for (i in seq_along(k.values)) {
  km <- kmeans(X.scaled, centers = k.values[i], nstart = 25)
  sil <- silhouette(km$cluster, dist(X.scaled))
  sil.kmeans[i] <- mean(sil[, 3])
}

kmeans.results <- data.frame(k = k.values,
                             avg_silhouette = sil.kmeans)
kmeans.results

best.k.kmeans <- kmeans.results$k[which.max(kmeans.results$avg_silhouette)]
best.k.kmeans

kmeans.final <- kmeans(X.scaled, centers = best.k.kmeans, nstart = 25)
sil.kmeans.final <- silhouette(kmeans.final$cluster, dist(X.scaled))

fviz_silhouette(sil.kmeans.final) +
  ggtitle(paste("K-means Silhouette Plot (k =", best.k.kmeans, ")"))



# PAM
sil.pam <- numeric(length(k.values))

for (i in seq_along(k.values)) {
  pam.fit <- pam(X.scaled, k = k.values[i])
  sil.pam[i] <- pam.fit$silinfo$avg.width
}

pam.results <- data.frame(k = k.values,
                          avg_silhouette = sil.pam)
pam.results

best.k.pam <- pam.results$k[which.max(pam.results$avg_silhouette)]
best.k.pam

pam.final <- pam(X.scaled, k = best.k.pam)
sil.pam.final <- silhouette(pam.final)

fviz_silhouette(sil.pam.final) +
  ggtitle(paste("PAM Silhouette Plot (k =", best.k.pam, ")"))



# compare silhouette
plot(kmeans.results$k, kmeans.results$avg_silhouette,
     type = "b", col = "blue", pch = 19,
     xlab = "k", ylab = "Average Silhouette Width",
     main = "Silhouette Comparison")

lines(pam.results$k, pam.results$avg_silhouette,
      type = "b", col = "red", pch = 19)

legend("topright",
       legend = c("k-means", "PAM"),
       col = c("blue", "red"),
       lty = 1, pch = 19)