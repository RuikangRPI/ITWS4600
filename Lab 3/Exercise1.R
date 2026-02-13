library(class)
library(caret)
library(GGally)
library(psych)
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
X.train1 <- scale(train[, 1:7])
X.test1  <- scale(test[, 1:7],
                  center = attr(X.train1, "scaled:center"),
                  scale  = attr(X.train1, "scaled:scale"))

# model 1: numeric features
Y.train <- train$age.group
Y.test  <- test$age.group
k1 <- 5
pred1 <- knn(train = X.train1, test = X.test1, cl = Y.train, k = k1)
# contingency table
tab1 <- table(Predicted = pred1, Actual = Y.test)
tab1
# accuracy
acc1 <- sum(diag(tab1)) / sum(tab1)
acc1

# model 2: using only weight related features
X.train2 <- scale(train[, 4:7])
X.test2  <- scale(test[, 4:7],
                  center = attr(X.train2, "scaled:center"),
                  scale  = attr(X.train2, "scaled:scale"))

k2 <- 5
pred2 <- knn(train = X.train2, test = X.test2, cl = Y.train, k = k2)
# contingency table
tab2 <- table(Predicted = pred2, Actual = Y.test)
tab2
# accuracy
acc2 <- sum(diag(tab2)) / sum(tab2)
acc2

# compare models
acc1
acc2
# model 1 is better

# tune k for model 1
k.values <- seq(1, 50, by = 1)
accuracy <- numeric(length(k.values))
for (i in seq_along(k.values)) {
  pred <- knn(train = X.train1,
              test  = X.test1,
              cl    = Y.train,
              k     = k.values[i])
  accuracy[i] <- sum(pred == Y.test) / length(Y.test)
}
# results table
k.results <- data.frame(k = k.values, accuracy = accuracy)
k.results

model1.k <- k.results$k[which.max(k.results$accuracy)]
model1.acc <- max(k.results$accuracy)

model1.k
model1.acc

plot(k.results$k, k.results$accuracy, type = "b",
     xlab = "k", ylab = "Accuracy",
     main = "kNN Accuracy vs k")