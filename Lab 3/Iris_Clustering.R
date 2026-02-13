###################################
##### Clustering with Iris v3 #####
###################################


install.packages("dendextend")
install.packages("colorspace")
install.packages("factoextra")

## imports
library(GGally)
library(ggplot2)
library(psych)
library(cluster)
library(dendextend)
library(colorspace)
library(factoextra)

## data
iris.data <- iris

## scale variables
# iris.data[-5] <- scale(iris.data[-5])


## ggplo2 scatterplots
ggplot(iris.data, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

ggplot(iris.data, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point()

ggplot(iris.data, aes(x = Petal.Length, y = Sepal.Width, colour = Species)) +
  geom_point()

ggplot(iris.data, aes(x = Sepal.Length, y = Petal.Width, colour = Species)) +
  geom_point()


## psych scatterplot matrix
pairs.panels(iris.data[,-5],gap = 0,bg = c("pink", "green", "blue")[iris.data$Species],pch=21)


## GGally 
ggpairs(iris.data, ggplot2::aes(colour = Species))


### K-Means ###

## set seed value for random number generator
set.seed(6)

## number of clusters to look for
k = 3

## learn clusters
iris.km <- kmeans(iris.data[,-5], centers = k)


## Total (for all clusters) within clusters sum of squares
wcss <- iris.km$tot.withinss
wcss

### plot clustering output 
## put cluster assignments into a variable
assigned.clusters <- as.factor(iris.km$cluster)

## scatterplot of the data colored by cluster membership
ggplot(iris.data, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
geom_point()

## compute pairwise distances between all points: 150 x 150 matrix
iris.dist <- as.matrix(dist(iris.data[-5]))

## Silhouette Plot
sil <- silhouette(iris.km$cluster, dist(iris.data[-5]))
fviz_silhouette(sil)

## run tests with multiple k values and plot WCSS
k.list <- c(1,2,3,4,5,6,7)

## create empty lists
wcss.list <- c()
si.list <- c()

## for each value of k, run the algorithm, compute wcss and average silhouette width and append them to list
for (k in k.list) {
  
  iris.km <- kmeans(iris.data[,-5], centers = k)
  
  wcss <- iris.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  if (k > 1){
    
    si <- silhouette(iris.km$cluster, dist(iris.data[-5]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
}

## plot wcss vs. k
plot(k.list,wcss.list,type = "b")

## plot avg silhouette width vs. k
plot(k.list[-1],si.list,type = "b")


wcss.list

diff(wcss.list)

diff(diff(wcss.list))


### Partitioning Around Medoids ###

k = 3

iris.pam <- pam(iris.data[-5], k)

iris.pam$objective

## get and plot clustering output 
assigned.clusters <- as.factor(iris.pam$cluster)

ggplot(iris.data, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()

## Silhouette Plot
sil <- silhouette(iris.pam$cluster, dist(iris.data[-5]))
fviz_silhouette(sil)

## run tests with multiple k values and plot sum of dissimilarities (sum of distances)
k.list <- c(1,2,3,4,5,6,7)

sumdiss.list <- c()
si.list <- c()

for (k in k.list) {
  
  iris.pam <- pam(iris.data[,-5], k)
  
  sumdiss <- iris.pam$objective[2]
  
  sumdiss.list <- c(sumdiss.list,sumdiss)
  
  if (k>1){
    si <- silhouette(iris.pam$cluster, dist(iris.data[-5]))
    
    avg.si <- mean(si[, 3])  
    
    si.list <- c(si.list,avg.si)
  }
  
  
}

plot(k.list,sumdiss.list,type = "b")

plot(k.list[-1],si.list,type = "b")


### Hierarchical Clustering ###

iris.dist <- dist(iris.data[-5])

iris.hclust <- hclust(iris.dist)

plot(iris.hclust)

#### Custom Dendrogram #########
## https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html
#######

iris_species <- sort(levels(iris[,5]), decreasing = TRUE)

dend <- as.dendrogram(iris.hclust)

# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=3)  #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(iris[,5])[order.dendrogram(dend)]
  )]

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(iris[,5])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))

plot(dend, 
     main = "Clustered Iris data set
     (the labels give the true flower species)", 
     horiz =  TRUE,  nodePar = list(cex = .007))

legend("topleft", legend = iris_species, fill = rainbow_hcl(3))



## EOF ##