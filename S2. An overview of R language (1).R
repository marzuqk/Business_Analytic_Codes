x = c(1, 2, 3) # Welcome to DSME2040 Business Analytics
x

# Hope you enjoy the course
y = c(4, 5, 6)
y

# Vector ??? ¡°c ( )¡±
xNum = c(1, 1.1, 1.12, 1.123)
xLog = c(TRUE, FALSE, TRUE, FALSE)
xChar = c("DSME", "2040", "Business", "Analytics")
xMix = c("It is", TRUE, "that DSME", 2040, "is an interesting course")
xNum
xLog
xChar
xMix

# Indexing ??? ¡°[ ]¡±
xChar[3] # Third element of xChar 
xChar[2:4] # Second to fourth element of xChar
xChar[1,3] # Indexing the element at first row and third column : error
xChar[c(1,3)] # First and third element of xChar

xNum[2]+1
y = xNum[2]+1
y
xMix[4]+1 # Character string + numeric value : error
# use a predefined "as.numeric" function to change the type to numeric value
# before you add one
as.numeric(xMix[4])+1 

# uReply Question 1
# How can we change the value of second element of xMix to FALSE?

# Factor ??? ¡°factor( )¡±
xNum[2]-xNum[1]
yNum=factor(xNum)
yNum[2]-yNum[1]
summary(xNum)
summary(yNum)

# List ??? ¡°list( )¡±
yMix = list("It is", TRUE, "that DSME", 2040, "is an interesting course")
yMix
# Indexed with double brackets 
yMix[[4]]+1

# Data frame ??? ¡°data.frame( )¡±
data.frame(xNum, xLog, xChar)
x.df=data.frame(xNum, xLog, xChar)
x.df[3,2]

# Data table ??? ¡°data.table( )¡±
install.packages("data.table")
library(data.table)
x.dt=data.table(x.df)
# observations of xNum < 1.11
x.dt[xNum<1.11]
# How many observations have had xNum < 1.11?
x.dt[, sum(xNum<1.11)]
# How many observations have had xNum < 1.11 by xLog?
x.dt[, sum(xNum<1.11), xLog]

# Object structure ??? ¡°str()¡±
str(xNum)
str(yMix)
str(x.df)

# ¡°head( )¡± or ¡°tail( )¡±
# returns the first two rows
head(x.df, n=2)
# returns the last two rows
tail(x.df, n=2)

# Handling missing values
# ¡°na.rm=TRUE¡±
x = c(1, 2, 3, 4)
mean(x)
y = c(1, 2, 3, NA, 4)
mean(y)
mean(y, na.rm=TRUE)
# ¡°complete.cases()¡±
mean(y[complete.cases(y)])

####################################################################################
# Data creation : From R for Marketing Research and Analytics, Chapter 2
####################################################################################
rm(list=ls())    # deletes all objects

store.num = factor(c(3, 14, 21, 32, 54))   # store id
store.rev = c(543, 654, 345, 678, 234)     # store revenue, $1000
store.visits = c(45, 78, 32, 56, 34)       # visits, 1000s
store.manager = c("Annie", "Bert", "Carla", "Dave", "Ella")
store.df = data.frame(store.num, store.rev, store.visits, store.manager, stringsAsFactors=F)  # F = FALSE
str(store.df)
head(store.df)
####################################################################################

# Loading and saving data
# note the / instead of \ 
write.csv(store.df, "d:/store.csv")
write.csv(store.df, "d:/store2.csv", row.names=FALSE)
x = read.csv("d:/store.csv")
head(x)
y = read.csv("d:/store2.csv")
head(y)

# Renaming variables
# Rename interactively
fix(y) # results are saved on close
# install packages for the first time use
install.packages("reshape")
library(reshape)
y = rename(y, c(store.num="store.number"))
head(y)

# Other useful symbols
# "$": select a variable by name from a two dimensional object
y$rev.per.visit=y$store.rev/y$store.visits
y
# ¡°~¡± : explains with
lm(store.rev~store.visits, y)
# ¡°~.¡± : explains with all available variables
lm(y$store.rev~., y)
# ¡°~.-¡±: explains with all available variables except 
lm(y$store.rev~. -store.manager, y)

# Data size
# one dimensional data
length(store.df$store.rev)
# two dimensional data
dim(store.df)
nrow(store.df)
ncol(store.df)
#str(store.df)

# Writing your own function
# function for calculating the mean
my.mean = function (x) {
  output = sum(x) / length(x)
  return(output)
  }
my.mean(store.df$store.visits)
# compare the result by the built-in function, mean()
mean(store.df$store.visits)



















