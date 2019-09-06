########################
# Classification Trees #
########################

# Prepared by Jay Simon
# Last updated on 8/8/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a demonstration of how to build a classification tree in R
# The script uses the DC Exempt Organizations data set to predict whether or not an organization will have tax-deductible donations

# Packages required: tree


# The first step is to import the data set as a CSV
DC <- read.csv("DCExemptOrganizations.csv", header=T)

# It will be convenient to remove the Name column from the data set before doing any analysis (it's column 1)
DC <- DC[,-1]

# Set the seed used for random number generation, for reproduceability
# Always put this command immediately before the command that uses randomness
set.seed(12345)

# The following command randomly chooses 60% of the rows in the data set
training <- sample(1:nrow(DC), 0.6*nrow(DC))

# The following two commands separate the training data into two variables, one with DEDUCTIBLE removed, and one containing only DEDUCTIBLE
DC.training <- DC[training,-6]
DC.training.results <- DC[training,6]

# The following two commands do the same for the remaining 40% of the data
DC.test = DC[-training,-6]
DC.test.results = DC[-training,6]


# If you have not installed & activated the "tree" package, do so before continuing.

# Now, we can build a tree using the training set
# The period after the tilde is shorthand telling R to use all other variables

DC.tree <- tree(DEDUCTIBLE ~ ., data=DC[training,])
plot(DC.tree)
text(DC.tree)


# The tree command has many parameters that we omitted; R sets them all to their default values
# If we want to change the rules by which it builds the tree, we can do so.
# The most commonly changed parameter is "mindev" 
# It specifies the minimum reduction in variance needed to split a node, and thus controls the size of the tree
# Its default value is 0.01
# We can change it to 0.001 and see what happens:

DC.tree <- tree(DEDUCTIBLE ~ ., data=DC[training,], mindev=0.001)
plot(DC.tree)
text(DC.tree, cex=0.6)
# The cex parameter is used to control font size


# The following command obtains the proportions of 1's from the training set 
# in the tree endpoint for each data point in the test set
# ("-training" tells R to use the rows not in the training set)
DC.tree.proportions <- predict(DC.tree,DC[-training,-6])

# The following command rounds each proportion to 0 or 1, obtaining the binary classifications
DC.tree.classifications <- round(DC.tree.proportions,0)

# The following commands compute the proportion of classifications on the test set that were correct
sum(DC.tree.classifications == DC.test.results) / nrow(DC[-training,])

# The following command shows the classification confusion matrix for the test set
table(DC.tree.classifications, DC.test.results)

# To apply the model to new data points, we first need to import the new data
DC.new <- read.csv("DCExemptOrganizationsNew.csv", header=T)

# Then use the predict function to obtain the proportions
predict(DC.tree,DC.new)

# Alternatively, as we did with the test set, we can convert them to binary classifications
round(predict(DC.tree,DC.new),0)