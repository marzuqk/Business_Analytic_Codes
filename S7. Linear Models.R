#################################################################
# From R for Marketing Research and Analytics, Chapter 7
#################################################################

# Load the .csv data from the web
sat.df = read.csv("http://goo.gl/HKnl74")

# Explore the data structure
str(sat.df)
head(sat.df)
summary(sat.df)

#################################################################
# Data Inspection for Linear Models
#################################################################

# Check the distribution and the joint relationship
install.packages("gpairs")
library(gpairs)
gpairs(sat.df)

# Transforming distance
sat.df$distance = log(sat.df$distance)
gpairs(sat.df)   

# Correlation plot
library(corrplot)
corrplot.mixed(cor(sat.df[ , c(2, 4:8)]), upper="ellipse")

#################################################################
# Fitting a model with a single predictor
#################################################################

model=lm(overall~rides, data=sat.df)
summary(model)

plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(model, col='blue')

#################################################################
# Non-linear example
#################################################################

# data generation
x = rnorm(500)
y = x^2 + rnorm(500)

# linear model
toy.model = lm(y~x)
summary(toy.model)

# plot
plot(y~x)
abline(toy.model, col='blue')

# altenate model
toy.model2 = lm(y~x+I(x^2))
summary(toy.model2)

# model comparison
library(lmtest)
anova(toy.model, toy.model2) # baseed on R square
lrtest(toy.model, toy.model2) # baseed on log-likelihood

#################################################################
# Fitting a model with multiple predictors
#################################################################

model2 = lm(overall ~ rides + games + wait + clean, data=sat.df)
summary(model2)

# Is clean more important than wait? (uReply)

# We need the same unit to compare
# example (USD vs.HKD)
spending = c(100, 110, 120, 130, 240, 250, 260, 270, 380, 390)
income_USD = c(1, 2, 3, 4, 5, 6, 7, 8, 8, 8)
family_size = c(4, 4, 4, 4, 7, 7, 7, 7, 7, 12)
summary(lm(spending~income_USD+family_size))

income_HKD=7.8*income_USD
summary(lm(spending~income_HKD+family_size))

# Standardize the variables to compare coefficients
summary(lm(spending~scale(income_HKD)+scale(family_size)))
summary(lm(spending~scale(income_USD)+scale(family_size)))

# Coming back to the park data
model3 = lm(overall ~ scale(rides) + scale(games) + scale(wait) + scale(clean), data=sat.df)
summary(model3)

# visualize the coefficients
install.packages("coefplot")
library(coefplot)
coefplot(model3, intercept=FALSE, outerCI=1.96, lwdOuter=1.5, 
         ylab="Rating of Feature", xlab="Association with Overall Satisfaction")
# What insight can you get from this graph?

#################################################################
# Using factors as predictors
#################################################################

# standardize data before the anlaysis
sat.std=sat.df
sat.std[ , 3:8] = scale(sat.df[ , 3:8])
head(sat.std)

# to check whether the satisfaction is different for customers who come on the weekend, travel more, or have more children
model4 = lm(overall ~ rides + games + wait + clean + weekend + distance + num.child, data = sat.std)
summary(model4)

# converting num.child to a factor
model5 = lm(overall ~ rides + games + wait + clean + weekend + distance + factor(num.child), data = sat.std)
summary(model5)

# making a new dummy
sat.std$has.child = factor(sat.std$num.child>0)
model6 = lm(overall ~ rides + games + wait + clean + weekend + distance + has.child, data = sat.std)
summary(model6)

# which one is better? (uReply)
lrtest(model5, model6)

#################################################################
# Moderating effects
#################################################################

# Is the relationship between satisfaction and waiting times different for parties with and without children?
# run the model with an interaction term
model7 = lm(overall ~ rides + games + wait + clean + weekend + distance + has.child + wait*has.child, data = sat.std)
summary(model7)

















