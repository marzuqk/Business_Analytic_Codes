#######################
# k-Nearest Neighbors #
#######################

# Prepared by Jay Simon
# Last updated on 11/24/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a demonstration of how to build a k-nearest neighbors model for prediction or classification
# The script first uses the Lending_Club data set to predict the interest rate a borrower will get through Lending Club
# The script then uses the DC Exempt Organizations data set to classify an organization as having tax-deductible donations or not

# Packages required: FNN, class


#------------------------------------------------------------------
# k-NN for PREDICTION



# First, we need to import the Lending Club data set into R
# Use the Files tab in the bottom right panel of RStudio
# In this script, it was stored as "lc"

# The following command forces the "class" of the data set to be a data frame.
# This can eliminate potential errors with how some functions will interpret objects of other classes
lc <- as.data.frame(lc)

# The set.seed function sets the starting point for random number generation
# When using the same seed, random processes will always produce the same output
# It's useful for being able to reproduce your results
# Always place this command immediately before the command that uses randomness
set.seed(12345)

# The following command will randomly select 60% of the row numbers in the data set to represent the training data
training <- sample(1:nrow(lc), 0.6*nrow(lc))

# The following two commands separate the training data into two objects; one has interest rate removed, the other contains only interest rate
lc.training <- lc[training,-8]
lc.training.results <- lc[training,8]

# The following two commands do the same for the remaining 40% of the data
lc.test <- lc[-training,-8]
lc.test.results <- lc[-training,8]

# If you have not installed the "FNN" package, do so before continuing.

# The following command runs the k-nearest neighbors model with k=5
lc.knn <- knn.reg(lc.training, lc.test, lc.training.results, k=5)

# The following command computes and displays the root mean squared error of the model on the test set
(mean((lc.knn$pred - lc.test.results)^2))^0.5

# To make predictions for new data points, we first need to import the new data
# Again, use the Files tab in the bottom right panel of RStudio, find LendingClubNew.csv
# In this script, it was stored as "lcnew"

# We then apply the same k-NN function, but using the new data as the test set, and then display the prediction(s)
lc.knn.new <- knn.reg(lc.training, lcnew, lc.training.results, k=5)
lc.knn.new$pred

# We might want to try a range of values for k to find which gives the most accurate predictions
# A for loop is useful for this, where in each iteration, it checks to see if the current RMSE beats the best RMSE observed so far
# (Note that we've called the index variable i, not k.  That's because the knn.reg function already has a parameter called k.)
# This loop checks values of k from 1 to 50.
best.k <- -1
RMSE <- -1
best.RMSE <- 99999999
for (i in 1:50) {
  lc.knn <- knn.reg(lc.training, lc.test, lc.training.results, k=i)
  RMSE <- (mean((lc.knn$pred - lc.test.results)^2))^0.5
  if (RMSE < best.RMSE) {
    best.k <- i
    best.RMSE <- RMSE
  }
}
print(paste("The optimal value of k is",best.k,"with a RMSE of",best.RMSE))



#------------------------------------------------------------------
# k-NN for CLASSIFICATION

# Now, let's import the DC Exempt Organizations data set into R
# In this script, it was stored as "DC"
DC <- read.csv("DCExemptOrganizations.csv", header=T)
# The following command forces the "class" of the data set to be a data frame.
# This can eliminate potential errors with how some functions will interpret objects of other classes
DC <- as.data.frame(DC)

# It will be convenient to remove the Name column from the data set before doing any analysis (it's column 1)
# Note that with other data sets, you should NOT include this command unless you need to remove the first column!
DC <- DC[,-1]

# The set.seed function sets the starting point for random number generation
# When using the same seed, random processes will always produce the same output
# It's useful for being able to reproduce your results
# Always place this command immediately before the command that uses randomness
set.seed(12345)

# The following command will randomly select 60% of the row numbers in the data set to represent the training data
training <- sample(1:nrow(DC), 0.6*nrow(DC))

# The following two commands separate the training data into two variables, one with DEDUCTIBLE removed, and one containing only DEDUCTIBLE
# Note that the second one is different from what we did for kNN for prediction; we want to store the results as binary variables, not integers
DC.training <- DC[training,-6]
DC.training.results <- DC[training,6] > 0.5

# The following two commands do the same for the remaining 40% of the data
# Here, again, the second command stores the results as binary variables
DC.test <- DC[-training,-6]
DC.test.results <- DC[-training,6] > 0.5

# If you have not installed the "class" package, do so before continuing.

# The following command runs the k-nearest neighbors model with k=5
DC.knn <- knn(DC.training, DC.test, DC.training.results, k=5)

# The following command computes the proportion of classifications on the test set that were correct
sum(DC.knn == DC.test.results) / length(DC.test.results)

# The following command shows the classification confusion matrix for the test set.
table(DC.knn, DC.test.results)

# To classify new data points, we first need to import the new data
# Again, use the Files tab in the bottom right panel of RStudio, find DCExemptOrganizationsNew.csv
# In this script, it was stored as "DCnew"
DCnew <- read.csv("DCExemptOrganizationsNew.csv", header=T)
# We then apply the same k-NN function, but using the new data as the test set, and then display the prediction(s)
DC.knn.new <- knn(DC.training, DCnew, DC.training.results, k=5)

# Note that the classify version of knn gives us a model that does not have a "pred" property; its first elements are the predictions
DC.knn.new[1:nrow(DCnew)]

# We might want to try a range of values for k to find which gives the most accurate classifications
# A for loop is useful for this, where in each iteration, it checks to see if the current error rate beats the best error rate observed so far
# (Here we are minimizing error rate, which is 1 minus classification accuracy.  A similar loop could be created to maximize classification accuracy.)
# Note that we've called the index variable i rather than k, since the knn function has a parameter called k.
best.k <- -1
error.rate <- -1
best.error.rate <- 99999999
for (i in 1:20) {
  DC.knn <- knn(DC.training, DC.test, DC.training.results, k=i)
  error.rate <- 1-(sum(DC.knn == DC.test.results) / length(DC.test.results))
  if (error.rate < best.error.rate) {
    best.k <- i
    best.error.rate <- error.rate
  }
}
print(paste("The optimal value of k is",best.k,"with an overall error rate of",best.error.rate))

