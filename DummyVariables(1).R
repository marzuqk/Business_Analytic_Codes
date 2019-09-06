###################
# Dummy Variables #
###################

# Prepared by Jay Simon
# Last updated on 8/2/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script demonstrates how to create dummy variables from a categorical variable in R
# The script uses the chickwts data set. This data set contains the "weight" (numeric) and type of "feed" (text) for 71 chickens.
# For further explanation of the data set, see https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/chickwts.html

#To view the data set:
chickwts

# The key function is model.matrix.  It will create a dummy variable for each value of a category variable and store them in a new data set
feeds <- model.matrix(~feed-1, data=chickwts)

#We can then add these new dummy variables to the chickwts data set (shown with two different syntaxes; using either the column number or name will work)
chickwts$casein <- feeds[,1]
chickwts$horsebean <- feeds[,"feedhorsebean"]
chickwts$linseed <- feeds[,3]
chickwts$meatmeal <- feeds[,"feedmeatmeal"]
chickwts$soybean <- feeds[,5]
chickwts$sunflower <- feeds[,"feedsunflower"]

#Note that we've created dummy variables for all six feeds. Usually a model will only require us to use five of them.

#To view the updated data set:
chickwts

#Now, we can run models such as linear regression using the dummy variables (sunflower is omitted):
feedeffects <- lm(weight ~ casein + horsebean + linseed + meatmeal + soybean, data=chickwts)
summary(feedeffects)