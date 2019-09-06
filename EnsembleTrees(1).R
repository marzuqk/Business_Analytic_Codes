##################
# Ensemble Trees #
##################

# Prepared by Jay Simon
# Last updated on 11/24/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a demonstration of how to expand regression trees in R to implement bagging and random trees
# The script uses the Lending Club data set to predict the interest rate a borrower will get through Lending Club


# First, we need to import the Lending Club data set into R
# In this script, it was stored as "lc"
lc <- read.csv("lc.csv", header=T)
# The following command forces the "class" of the data set to be a data frame.
# This can eliminate potential errors with how some functions will interpret objects of other classes
lc <- as.data.frame(lc)

# The set.seed function sets the starting point for random number generation
# When using the same seed, random processes will always produce the same output
# It's useful for being able to reproduce your results
set.seed(12345)

# The following command will randomly select 60% of the row numbers in the data set to represent the training data
training <- sample(1:nrow(lc), 0.6*nrow(lc))

# The following line avoids having to change the code manually for data sets with different numbers of columns
nvars <- ncol(lc)

# The following two commands separate the training data into two objects; one has interest rate removed, the other contains only interest rate
lc.training <- lc[training,-nvars]
lc.training.results <- lc[training,nvars]

# The following two commands do the same for the remaining 40% of the data
lc.test <- lc[-training,-nvars]
lc.test.results <- lc[-training,nvars]


#------------------------------------------------
# BAGGING

# Bagging parameters
bag.proportion <- 0.3 #proportion of training set used for each tree
bag.numtrees <- 25 #number of trees
bag.mindev <- 0.005 #controls the size of the trees (higher mindev -> smaller trees)

# Empty lists of trees & predictions that will be populated during the bagging process
bag.trees <- vector(mode="list",length=bag.numtrees) #creates the empty list of trees
bag.predictions <- vector(mode="list",length=bag.numtrees) #creates the empty list of prediction vectors
bagged.predictions <- 0

# The following for loop creates the trees using the Lending Club variables
for (i in 1:bag.numtrees){
  set.seed(12345+i) #if we used 12345 every time, we wouldn't get different samples from the training set
  lc.subset <- lc[sample(training,bag.proportion*length(training)),] #selects a random subset of the training set
  bag.trees[[i]] <- tree(int_rate ~ annual_inc + delinq_2yrs + home_ownership + inq_last_6mths + open_acc + pub_rec + loan_amnt, data=lc.subset, mindev=bag.mindev)
  bag.predictions[[i]] <- predict(bag.trees[[i]],lc)[-training]
  bagged.predictions <- bagged.predictions + bag.predictions[[i]] #Keeps a running total of the predictions of the test set
}
bagged.predictions = bagged.predictions / bag.numtrees #divides the totals by the # of trees to get the average predictions for the test set
(mean((lc.test.results-bagged.predictions)^2))^0.5 #computes RMSE

#------------------------------------------------
# RANDOM TREES

# Random tree parameters
rt.vars <- 3 #number of independent variables used in each tree
rt.numtrees <- 25 #number of trees
rt.mindev <- 0.005 #controls the size of the trees (higher mindev -> smaller trees)

# Empty lists of trees & predictions that will be populated during the random trees process
rt.trees <- vector(mode="list",length=rt.numtrees) #creates the empty list of trees
rt.predictions <- vector(mode="list",length=rt.numtrees) #creates the empty list of prediction vectors
randomtree.predictions <- 0

# The following for loop creates the trees using the Lending Club variables
for (i in 1:rt.numtrees){
  set.seed(12345+i) #if we used 12345 every time, we wouldn't get different subsets of variables
  lc.subset <- lc[training,sample(1:(nvars-1),rt.vars)] #selects a random subset of the variables
  lc.subset[,rt.vars+1] <- lc.training.results
  names(lc.subset)[rt.vars+1] = "int_rate" #this is necessary for the predict function to be able to match variables correctly
  rt.trees[[i]] <- tree(int_rate ~ ., data=lc.subset, mindev=rt.mindev) #include as many independent variables as are being used
  rt.predictions[[i]] <- predict(rt.trees[[i]],lc)[-training]
  randomtree.predictions <- randomtree.predictions + rt.predictions[[i]] #Keeps a running total of the predictions of the test set
}
randomtree.predictions = randomtree.predictions / rt.numtrees #divides the totals by the # of trees to get the average predictions for the test set
(mean((lc.test.results-randomtree.predictions)^2))^0.5 #computes RMSE
