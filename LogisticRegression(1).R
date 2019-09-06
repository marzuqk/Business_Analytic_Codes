#######################
# Logistic Regression #
#######################

# Prepared by Jay Simon
# Last updated on 11/24/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a demonstration of how to build a logistic regression model in R
# The script then uses the DC Exempt Organizations data set to classify an organization as having tax-deductible donations or not

# First, let's import the DC Exempt Organizations data set into R
# In this script, it was stored as "DC"
DC <- read.csv("DCExemptOrganizations.csv", header=T)
DCnew<-read.csv("DCExemptOrganizationsNew.csv", header=T)
# The following command forces the "class" of the data set to be a data frame.
# This can eliminate potential errors with how some functions will interpret objects of other classes
DC <- as.data.frame(DC)

# It will be convenient to remove the Name column from the data set before doing any analysis (it's column 1)
DC <- DC[,-1]

# The set.seed function sets the starting point for random number generation
# When using the same seed, random processes will always produce the same output
# It's useful for being able to reproduce your results
# Always run this command immediately before the command that uses randomness
set.seed(12345)

# The following command will randomly select 60% of the row numbers in the data set to represent the training data
training <- sample(1:nrow(DC), 0.6*nrow(DC))

# The following two commands separate the training data into two variables, one with DEDUCTIBLE removed, and one containing only DEDUCTIBLE
DC.training <- DC[training,-6]
DC.training.results <- DC[training,6]

# The following two commands do the same for the remaining 40% of the data
DC.test = DC[-training,-6]
DC.test.results = DC[-training,6]

# The following command builds a logistic regression model on the training set
DC.lr <- glm(DEDUCTIBLE ~ ., family=binomial(link='logit'),data=DC[training,])

# The following command shows the logistic regression output
summary(DC.lr)

# The following command creates a list of the model's probabilities of each organization in the test set having tax-deductible donations
DC.test.probabilities <- predict(DC.lr,DC.test,type = "response")

# The following command converts the probabilities to binary 0/1 classifications
DC.lr.classifications <- round(DC.test.probabilities,0)

# The following command computes the proportion of classifications on the test set that were correct
sum(DC.lr.classifications == DC.test.results) / length(DC.test.results)

# The following command shows the classification confusion matrix for the test set
table(DC.lr.classifications,DC.test.results)

# To classify new data points, we first need to import the new data
# Again, use the Files tab in the bottom right panel of RStudio, find DCExemptOrganizationsNew.csv
# In this script, it was stored as "DCnew"

# Then use the predict function to obtain the probabilities
predict(DC.lr, DCnew, type="response")

# Alternatively, as we did with the test set, we can convert them to binary classifications
round(predict(DC.lr, DCnew, type="response"),0)
