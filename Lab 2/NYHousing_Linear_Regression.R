library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("./NY-House-Dataset.csv")
dataset <- NY_House_Dataset

# quick view
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

### Example Linear Model - Price Vs. Property Sqft

## filter data
dataset <- dataset[dataset$PRICE<195000000,]
## column names
names(dataset)
## fit linear model
lmod0 <- lm(PRICE~PROPERTYSQFT, data = dataset)
## print model output
summary(lmod0)
## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod0)
## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")
lmod1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
## print model output
summary(lmod1)
## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod1)
## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")
ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

## filter data
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]
lmod2 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
## print model output
summary(lmod2)
## scatter plot of 2 variables with best fit line
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod2)
## better scatter plot of 2 variables with best fit line
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")



### Linear Model 1 - Price Vs. Bedrooms
dataset <- NY_House_Dataset

ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point()
ggplot(dataset, aes(x = BEDS, y = log10(PRICE))) +
  geom_point()

dataset <- dataset[dataset$PRICE<195000000,]
lmod3 <- lm(log10(PRICE)~BEDS, data = dataset)
summary(lmod3)

plot(log10(PRICE)~BEDS, data = dataset)
abline(lmod3)

ggplot(dataset, aes(x = BEDS, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")



### Linear Model 2 - Price Vs. Baths
dataset <- NY_House_Dataset

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point()
ggplot(dataset, aes(x = BATH, y = log10(PRICE))) +
  geom_point()

dataset <- dataset[dataset$PRICE<195000000,]
lmod4 <- lm(log10(PRICE)~BEDS, data = dataset)
summary(lmod4)

plot(log10(PRICE)~BATH, data = dataset)
abline(lmod4)

ggplot(dataset, aes(x = BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")



### Linear Model 3 - Price Vs. Combination Score
#   
#   Based on searches online:
#     - a new bedroom can add about 10-20% value
#     - same for bathrooms
#     - home location matters a lot -> we are not using for this lab
#     - each 1000 sqft can add 30% value
# 
#   Finally, I'm setting property sqft as a basedline score, using the beds and bath as multipliers
#   SCORE = 1.30*(PROPERTYSQFT/1000) * (1+0.15*BEDS) * (1+0.15*BATH)

dataset <- NY_House_Dataset
dataset$SCORE <- 1.30*(dataset$PROPERTYSQFT) * (1+0.15*dataset$BEDS) * (1+0.15*dataset$BATH)
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]
ggplot(dataset, aes(x = SCORE, y = PRICE)) +
  geom_point()
ggplot(dataset, aes(x = log10(SCORE), y = log10(PRICE))) +
  geom_point()

lmod5 <- lm(log10(PRICE)~log10(SCORE), data = dataset)
summary(lmod5)

plot(log10(PRICE)~log10(SCORE), data = dataset)
abline(lmod5)
ggplot(dataset, aes(x = log10(SCORE), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

# Conclusion:
# Clearly, with only BEDS or BATH, there isn't nearly enough information to correlate to price.
# And property sqft has the most effect on the price.
# Thus, I believe that my third linear model is the most useful in this case.
# Though, I think that the location of the house plays another big factor in the pricing which I did not get to use in this lab.


### THE END ###

