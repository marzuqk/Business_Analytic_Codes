#################################################################
# From R for Marketing Research and Analytics, Chapter 9
#################################################################

# Load the .csv data from the web
cust.df = read.csv("http://goo.gl/PmPkaG")
cust.df$cust.id = factor(cust.df$cust.id)

# Explore the data structure
str(cust.df)
head(cust.df)
View(cust.df)
#################################################################
# Choosing IVs for your linear model
#################################################################

# use all available variables except the cust.id
# omit customers with zero online spend
spend.m1 = lm(online.spend ~ ., data=subset(cust.df[ , -1], online.spend > 0))
summary(spend.m1)

# Running the linear model without "online.trans"
spend.m2 = lm(online.spend ~ . -online.trans , data=subset(cust.df[ , -1], online.spend > 0))
summary(spend.m2)

# Checking the multicollinearity
# VIF
library(car)
vif(spend.m1)
# to check which ones ars correlated with each other
pairs(cust.df) 

# Which one will you choose for your IV, store.trans or store.spend (uReply)? why?

# A revised linear model
spend.m3 = lm(online.spend ~ . -online.trans -store.trans, data=subset(cust.df[ , -1], online.spend > 0))
summary(spend.m3)
vif(spend.m3)

#################################################################
# Logistic model
#################################################################

# to load the data from website
pass.df = read.csv("http://goo.gl/J8MH6A")
pass.df$Promo = factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))
str(pass.df)
head(pass.df)
summary(pass.df)

# "Does the promotion bundle have an effect on season pass sales?"

# initial logistic regression model
pass.m1 = glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m1)

# How to interpret the result?
plogis(0.3888)                          # outcome %
plogis(0.3888) / (1-plogis(0.3888))     # ratio of outcome % to alternative %
exp(0.3888)                             # identical

# odds ratio for sales
exp(coef(pass.m1))
# confidence intervals
exp(confint(pass.m1))

# Exploring the channel effect
install.packages("vcd")
library(vcd)    
doubledecker(table(pass.df))

# revised logistic regression model
# main effects
pass.m2 = glm(Pass ~ Promo + Channel, data=pass.df, family=binomial)
summary(pass.m2)
exp(coef(pass.m2))
exp(confint(pass.m2))
# interaction effects
pass.m3 = glm(Pass ~ Promo + Channel + Promo:Channel, 
               data=pass.df, family=binomial)
summary(pass.m3)
plogis(2.1071) / (1-plogis(2.1071)) 
plogis(2.1071-2.9808) / (1-plogis(2.1071-2.9808)) 
plogis(2.1071-2.8115) / (1-plogis(2.1071-2.8115)) 

#################################################################
# Fitting models based on the likelihood
#################################################################

# Fitting a normal distribution

# generating 100 data points from a normal distribution, N(1, 2)
set.seed(1234)
x = rnorm(100, mean = 1, sd = 2)

# if we have a reason to guess that the data came from a normal
LL = function(mu, sigma) {
  R = dnorm(x, mu, sigma)
  -sum(log(R))
}

#install.packages("bbmle")
library(bbmle)
mle2(LL, start = list(mu = 1, sigma=1))

#If we try a larger sample
x2 = rnorm(10000, mean = 1, sd = 2)
LL = function(mu, sigma) {
  R = dnorm(x2, mu, sigma)
  -sum(log(R))
}
mle2(LL, start = list(mu = 1, sigma=1))

# Fitting a linear model

# data generating process
x = runif(10000)
y = 3 * x + 1 + rnorm(10000)

# using the least square
LS_model = lm(y ~ x)
summary(LS_model)

# using the MLE
LL = function(beta0, beta1, mu, sigma) {
  R = y - x * beta1 - beta0
  R = dnorm(R, mu, sigma, log = TRUE)
  -sum(R)
}
MLE_model = mle2(LL, start = list(beta0 = 1, beta1 = 1, mu = 0, sigma = 1))
summary(MLE_model)



