##############
# Regression #
##############

# Prepared by Jay Simon
# Last updated on 11/8/2017

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a demonstration of multiple linear regression in R
# It also includes extensions of the model to capture interactions and weights
# The script uses the attitude data set. This data set contains survey responses from employees of a large financial organization

# Each row in the data set is one of 30 of the organization's branches. The independent variables are the proportion of favorable responses
# to a number of individual questions. The dependent variable is the average overall rating given to the branch. For further explanation of the
# data set, see https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/attitude.html

# Packages required: stats (typically comes pre-installed with R)

# It's a good idea to make sure we know what the variables in the data set are before creating a regression model.
# Either of the following two commands can be used:
head(attitude)
summary(attitude)

# The function for linear regression in the stats package is "lm"
# The following command creates the regression model using all variables, and stores it in attitude.MLRmodel
attitude.MLRmodel <- lm(rating ~ complaints + privileges + learning + raises + critical + advance, data=attitude)

# The following command shows the regression output; the coefficients are the "Estimate" column
summary(attitude.MLRmodel)

# The output shows that most variables are insignificant. Let's try a model that uses only complaints.
attitude.SLRmodel <- lm(rating ~ complaints, data=attitude)

# Again, the following command shows the output
summary(attitude.SLRmodel)

# It's easy to include interaction terms if desired
# The following model includes an interaction between critical and advance:
attitude.MLRInteractionmodel <- lm(rating ~ complaints + privileges + learning + raises + critical + advance + critical*advance, data=attitude)
summary(attitude.MLRInteractionmodel)


# We can use regression models to make predictions for new data points as well using the "predict" function
# Most predictive methods in R have a predict function that works very much like this one
# Two commands are required. The first creates a "data frame" containing the new data points, and the second makes the predictions.
# (Alternatively, instead of the first command, we could import a file that contains the same information.)
# Here, we're using the simple linear regression model to predict ratings for three new branches with values of 10, 50, and 90 for complaints
new.branches <- data.frame(complaints=c(10, 50, 90))
predict(attitude.SLRmodel, newdata=new.branches)

# The following two commands make predictions for three new branches using the full model with all variables
new.branches2 <- data.frame(complaints=c(10, 50, 90), privileges=c(10, 50, 90), learning=c(10, 50, 90), raises=c(10, 50, 90), critical=c(10, 50, 90), advance=c(10, 50, 90))
predict(attitude.MLRmodel, newdata=new.branches2)

# We can also obtain 95% prediction intervals along with the predictions, i.e. the model is 95% sure the rating will be within this range
predict(attitude.SLRmodel, newdata=new.branches, interval='prediction')


# To create a weighted linear regression model, use the "weights" parameter
# Here, we're fitting a model to predict the magnitude of the residuals based on complaints,
# then assigning 1 over that predicted residual squared as the weight for the data point
wts <- 1/fitted(lm(abs(residuals(attitude.SLRmodel)) ~ attitude$complaints))^2
attitude.Weightedmodel <- lm(rating ~ complaints, weights=wts, data=attitude)