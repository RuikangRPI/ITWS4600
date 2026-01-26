install.packages("readr", type = "source")
install.packages("EnvStats", type = "source")
install.packages("nortest", type = "source")

library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("~/Desktop/Data Analytics/Lab 1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)

# print vlues in variable
epi.data$EPI.new

# print summary of variable
summary(epi.data$EPI.new)


### Explore Variable ###

## take a copy of a variable from a dataframe into a separate variable
EPI <- epi.data$EPI.new

# find NAs in variable: function outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(EPI)

# print
NAs

# function "which" returns row numbers of rows with NAs
rownums <- which(NAs)

# print rows with NAs
EPI[rownums]

## create copy of new variable

MHP <- epi.data$MHP.new

# print values in variable
MHP

# find NAs inv variable - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(MHP)

rownums <- which(NAs)

# print NAs
MHP[rownums]

# take subset of NOT NAs from variable
MHP.complete <- MHP[!NAs]

MHP.complete

# filter for only values above 30 and assign result to new variable
MHP.above30 <- MHP.complete[MHP.complete>30]

MHP.above30
  
# stats
summary(MHP.above30)

# boxplot of variable(s)
boxplot(EPI, MHP.above30, names = c("EPI","MHP>30"))


### Histograms ###

# histogram (frequency distribution)
hist(EPI)

# define sequence of values over which to plot histogram
x <- seq(20., 80., 5)
  
# histogram (frequency distribution) over specified range
hist(EPI, x, prob=TRUE)

# print estimated density curve for variable
lines(density(EPI,bw="SJ")) # or try bw=“SJ”

# print rug under histogram
rug(EPI)


## plot  histogram again

# histogram (frequency distribution) over range
hist(EPI, x, prob=TRUE) 

# range of values
x1<-seq(20,80,1)

# generate probability density values for a normal distribution with given mean and sd
d1 <- dnorm(x1,mean=46, sd=11,log=FALSE)

# print density values
lines(x1,d1)


### Empirical Cumulative Distribution Function ###

# plot ecdfs
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 

plot(ecdf(MHP), do.points=FALSE, verticals=TRUE) 


### Quantile-quantile Plots ###

# print quantile-quantile plot for variable with theoretical normal distribuion
qqnorm(EPI); qqline(EPI)


# print quantile-quantile plot for random numbers from a normal distribution with theoretical normal distribution

# rnorm generates random values from a normal distribution with given mean and sd
x <- rnorm(180, mean=46, sd=10)

qqnorm(x); qqline(x)


# print quantile-quantile plot of two variables

qqplot(EPI, MHP, xlab = "Q-Q plot for EPI & MHP") 

# print quantile-quantile plot for 2 variables
qqplot(epi.data$EPI.new, epi.data$ECO.new, xlab = "Q-Q plot for EPI.new & EPI.old") 


## Statistical Tests

x <- rnorm(500)
y <- rnorm(500)

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

hist(x, col='lightsteelblue')
hist(y, col='lightgreen', add=TRUE)


### THE END ###