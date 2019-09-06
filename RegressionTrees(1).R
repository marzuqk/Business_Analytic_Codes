#######################
# Regression Trees #
#######################

# Prepared by Jay Simon
# Last updated on 11/18/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a demonstration of how to build a regression tree in R
# The script uses the Lending_Club data set to predict the interest rate a borrower will get through Lending Club

# Packages required: tree


# First, we need to import the Lending Club data set into R
# Use the Files tab in the bottom right panel of RStudio
# In this script, it was stored as "lc"

# The following command forces the "class" of the data set to be a data frame.
# This can eliminate potential errors with how some functions will interpret objects of other classes
lc <- as.data.frame(lc)

# The set.seed function sets the starting point for random number generation
# When using the same seed, random processes will always produce the same output
# It's useful for being able to reproduce your results
# Always run this command immediately before the command that uses randomness
set.seed(12345)

# The following command will randomly select 60% of the row numbers in the data set to represent the training data
training <- sample(1:nrow(lc), 0.6*nrow(lc))

# The following two commands separate the training data into two objects; one has interest rate removed, the other contains only interest rate
lc.training <- lc[training,-8]
lc.training.results <- lc[training,8]

# The following two commands do the same for the remaining 40% of the data
lc.test <- lc[-training,-8]
lc.test.results <- lc[-training,8]

# If you have not installed & activated the "tree" package, do so before continuing.

# The following command creats a regression tree using the training set and saves it as an object named "lc.tree"
lc.tree <- tree(int_rate ~ annual_inc + delinq_2yrs + home_ownership + inq_last_6mths + open_acc + pub_rec + loan_amnt, data=lc[training,])

# The next two lines plot the tree
plot(lc.tree)
text(lc.tree)


# The tree command has many parameters that we omitted; R sets them all to their default values
# If we want to change the rules by which it builds the tree, we can do so.
# A commonly changed parameter is "mindev" (other related ones are "mincut" and "minsize")
# It specifies the minimum reduction in variance needed to split a node, and thus controls the size of the tree
# Its default value is 0.01
# We can change it to 0.005 and see what happens:

lc.tree <- tree(int_rate ~ annual_inc + delinq_2yrs + home_ownership + inq_last_6mths + open_acc + pub_rec + loan_amnt, data=lc[training,], mindev=0.005)
plot(lc.tree)
text(lc.tree, cex=0.6)
# The cex parameter is used to control font size

# The summary command gives us some more information:
summary(lc.tree)
# Note: "Residual mean deviance" in the summary output is mean squared error (on the data used to build the tree).

# The following command generates predictions for the test set
lc.tree.predictions <- predict(lc.tree,lc)[-training]

# The following command computes RMSE of the model on the test set:
(mean((lc.test.results-lc.tree.predictions)^2))^0.5

# To make predictions for new data points, we first need to import the new data
# Use the Files tab in the bottom right panel of RStudio and find "LendingClubNew.csv"
# In this script, it was stored as "lcnew"

# Then use the predict function on the new data
predict(lc.tree,lcnew)

# We might want to try many different trees to find which one gives the most accurate predictions
# The easiest way to do this is to use many different values of mindev
# A for loop is useful for this, where in each iteration, it checks to see if the current RMSE beats the best RMSE observed so far
best.mindev <- -1
RMSE <- -1
best.RMSE <- 99999999
for (i in 1:100) {
  lc.tree <- tree(int_rate ~ annual_inc + delinq_2yrs + home_ownership + inq_last_6mths + open_acc + pub_rec + loan_amnt, data=lc[training,], mindev=0.0005*i)
  lc.tree.predictions <- predict(lc.tree,lc)[-training]
  RMSE <- (mean((lc.test.results-lc.tree.predictions)^2))^0.5
  if (RMSE < best.RMSE) {
    best.mindev <- 0.0005*i
    best.RMSE <- RMSE
  }
}
print(paste("The optimal value of mindev is",best.mindev,"with a RMSE of",best.RMSE))
# The following commands re-create and plot the optimal tree
lc.best.tree <- tree(int_rate ~ annual_inc + delinq_2yrs + home_ownership + inq_last_6mths + open_acc + pub_rec + loan_amnt, data=lc[training,], mindev=best.mindev)
plot(lc.best.tree)
text(lc.best.tree, cex=0.5)