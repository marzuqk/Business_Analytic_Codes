#####################
# Association Rules #
#####################

# Prepared by Jay Simon
# Last updated on 10/26/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a demonstration of association rules in R
# The script uses the Voter Registration data set, which will have to be imported into R

# Packages required: arules


# First, we need to import the Voter Registration data set into R
# The easiest way is to save the data set in your working directory, click on the filename
# in the "Files" tab of the bottom right panel, and then choose "Import Dataset"
# In this script, the data set is called "voter"


# Association rules require binary variables
# By default, numbers in CSV/Excel files are interpreted by R as numbers, not binary values, even if they only contain 0s and 1s
# Types can be changed when importing, but the following command is easier; 
# it converts the whole data set to binary and stores it in voter.binary
voter.binary <- voter > 0.5


# Make sure you have installed and activated the arules package before continuing

# The function that generates the set of association rules is "apriori"
rules <- apriori(voter.binary)

# The "inspect" function displays the rules
inspect(rules)

# The apriori function has default filters for rules
# We can change them to whatever we want; the following is an example with specified minimum support & confidence
rules <- apriori(voter.binary, parameter = list(supp=0.05, conf=0.4))

# We can also sort the rules in the inspect function by whichever metric we want; the following commands sort by lift ratio
# The sort function sorts in descending order by default. To sort in ascending order, include decreasing="FALSE" in the function
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
