#################################################################
# From R for Marketing Research and Analytics, Chapter 5 and 6
#################################################################

# Load the .csv data from the web
seg.df = read.csv("http://goo.gl/qw303p")
summary(seg.df)

# Explore the data structure
str(seg.df)
head(seg.df)
summary(seg.df)

#################################################################
# Descriptive statistics by groups
#################################################################

# Take the mean of each segment: a tedious work
mean(seg.df$income[seg.df$Segment == "Moving up"])
mean(seg.df$income[seg.df$Segment == "Moving up" & seg.df$subscribe=="subNo"])

# aggregate( ): summarizing continuous variables by groups 
aggregate(seg.df$income, list(seg.df$Segment), mean)

# Use the formula syntax (~)
aggregate(income ~ Segment, data=seg.df, mean)
aggregate(income ~ Segment + subscribe, data=seg.df, mean)
aggregate(income ~ Segment + subscribe + ownHome, data=seg.df, mean)

# table
table(seg.df$Segment)
table(seg.df$Segment, seg.df$ownHome)
table(seg.df$Segment, seg.df$ownHome, seg.df$subscribe)

# How many kids does the Suburb mix segment have? What about the Travelers? (uReply)

#################################################################
# Visualization by groups: Frequencies and Proportions
#################################################################

# loading the lattice package
install.packages(lattice)
library(lattice)

# proportions by subscrive across segment 
histogram(~subscribe | Segment, data=seg.df)

# counts instead of proportions, and some visual options
histogram(~subscribe | Segment, data=seg.df, type="count", layout=c(4,1), col=c("burlywood", "darkolivegreen"))

# histogram by 2 factors
histogram(~subscribe | Segment + ownHome, data=seg.df)

# use prop.table to get the proportion
prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2) # Change the margin to 1. What happens? 
prop.table(table(seg.df$subscribe, seg.df$Segment, seg.df$ownHome), margin=2)

# barchart( ): returns a barchart
# barchart of Subscriber proportion by Segment
barchart(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2, ], 
         xlab="Subscriber proportion by Segment", col="darkolivegreen")

# bar chart for continuous variable
# aggregate the data then plot it
seg.mean = aggregate(income ~ Segment, data=seg.df, mean)
barchart(income~Segment, data=seg.mean, col="grey")
# two factors
seg.income.agg = aggregate(income ~ Segment + ownHome, data=seg.df, mean)
barchart(income ~ Segment, data=seg.income.agg, 
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=c("gray95", "gray50")))

#bwplot( ): an enhanced version of boxplot( )
bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, xlab = "Income")
# add a conditioning variable, ownHome
bwplot(Segment ~ income | ownHome, data=seg.df, horizontal=TRUE, xlab="Income")

#################################################################
# Comparing groups: Statistical tests
#################################################################

# One way chi-square test
tmp.tab = table(rep(c(1:4), times=c(25,25,25,20)))
tmp.tab
chisq.test(tmp.tab)

tmp.tab = table(rep(c(1:4), times=c(25,25,30,10)))
tmp.tab
chisq.test(tmp.tab)

# Are the segment sizes significantly different from one another?
chisq.test(table(seg.df$Segment))

# Two way chi-square test
Input =("
Gender  Democrat  Republican
Female     30         10
Male       50         50
")
tmp.tab = as.matrix(read.table(textConnection(Input), header=TRUE, row.names=1))
tmp.tab  

chisq.test(tmp.tab,correct=FALSE)  

# Is Subscription independent from ownHome?
table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

# What about Segment and ownHome? (uReply)

# Testing group means: t.test( )

# Is the income different by home ownership?
# do some plotting to check income by ownHome
histogram(seg.df$income)
with(seg.df, histogram(income[ownHome=="ownYes"]))
with(seg.df, histogram(income[ownHome=="ownNo"]))

# t-tests
t.test(income ~ ownHome, data=seg.df)
t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers"))

# Testing multiple group means: aov( )
# Is income different by home ownership, segment, or both?

seg.aov.own = aov(income ~ ownHome, data=seg.df)
anova(seg.aov.own)

seg.aov.seg = aov(income ~ Segment, data=seg.df)
anova(seg.aov.seg)

anova(aov(income ~ Segment + ownHome, data=seg.df))





