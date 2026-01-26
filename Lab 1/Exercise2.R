library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("~/Desktop/Data Analytics/Lab 1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)


### Explore Variable ###
IFL <- epi.data$IFL.new
summary(epi.data$IFL.new) # summary 1
NAs <- is.na(IFL)
rownums <- which(NAs)
IFL[rownums]
IFL.complete <- IFL[!NAs]

FSS <- epi.data$FSS.new
NAs <- is.na(FSS)
rownums <- which(NAs)
FSS[rownums]
FSS.complete <- FSS[!NAs]
FSS.below50 <- FSS.complete[FSS.complete<50]
FSS.below50
summary(FSS.below50) # summary 2

# box plots
boxplot(IFL.complete, FSS.below50, names = c("IFL","FSS<50"))

# histograms
x <- seq(0., 100., 5)
hist(IFL.complete, x, prob=TRUE)
lines(density(IFL.complete, bw="SJ"))
rug(IFL.complete)

x <- seq(0., 50., 5)
hist(FSS.below50, x, prob=TRUE)
lines(density(FSS.below50, bw="SJ"))
rug(FSS.below50)

# ECDF plots
plot(ecdf(IFL.complete), do.points=FALSE, verticals=TRUE) 
plot(ecdf(FSS.below50), do.points=FALSE, verticals=TRUE) 

# QQ plot against normal
qqnorm(IFL.complete); qqline(IFL.complete)
qqnorm(FSS.below50); qqline(FSS.below50)

# QQ plot against each other
qqplot(IFL.complete, FSS.below50, xlab = "Q-Q plot for IFL & FSS")

# statistical test
x <- IFL.complete
y <- FSS.below50
hist(x)
hist(y)
shapiro.test(x)
shapiro.test(y)
ad.test(x)
ad.test(y)
ks.test(x,y)
wilcox.test(x,y)
var.test(x,y)
t.test(x,y)
hist(x, col='lightsteelblue', main='histogram of IFL.complete')
hist(y, col='lightgreen', add=TRUE, main='histogram of FSS.below50')
