################################################
#### Evaluating Classification & Clustering ####
################################################

install.packages("caret")
install.packages("GGally")
install.packages("psych")


library("caret")
library("GGally")
library("psych")
setwd("~/Documents/GitHub/ITWS4600/Lab 3")

###########
# Example #
###########

## read data
abalone <- read.csv("./abalone/abalone.data", header=FALSE)
## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 
## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]
## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)
## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)
## split train/test
train.indexes <- sample(4177,0.7*4177)
train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]
## separate x (features) & y (class labels)
X <- train[,1:7] 
Y <- train[,8]
## features subset
# train <- train[,5:8]
# test <- test[,5:8]
## feature boxplots
boxplot(X, main="abalone features")
## class label distributions
plot(Y)
## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")
featurePlot(x=X, y=Y, plot="box")
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)
## psych scatterplot matrix
pairs.panels(X,gap = 0,bg = c("pink", "green", "blue")[Y],pch=21)
## GGally 
ggpairs(train, ggplot2::aes(colour = Y))



## EOF ##