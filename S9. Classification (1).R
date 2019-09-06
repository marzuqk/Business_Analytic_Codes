# Load the library and the data
install.packages("C50")
library(C50)
data(churn)

# Explore the data structure
str(churnTrain)
head(churnTrain)

# Remove state, area_code, account_length which are not relevant for classification
churnTrain = churnTrain[, !names(churnTrain) %in% c("state", "area_code", "account_length")]

# Split 70 percent of the data into the training dataset and 30 percent into the testing dataset
set.seed(1234)
index = sample(2, nrow(churnTrain), replace = TRUE, prob = c(0.7, 0.3))
trainset = churnTrain[index==1,]
testset = churnTrain[index==2,]

# Check the data size
dim(trainset)
dim(testset)

#################################################################
# Tree model
#################################################################

# recursive partitioning tree
library(rpart)
churn.rp = rpart(churn ~ ., data=trainset)
churn.rp

# visualize the tree
plot(churn.rp)
text(churn.rp)
# use parameters to adjust the layout
plot(churn.rp, uniform=TRUE, branch=0.1, margin=0.1)
text(churn.rp, all=TRUE, use.n=TRUE)

# Performance
predictions = predict(churn.rp, testset, type="class")
table(predictions, testset$churn)
# Performance: Confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))

# conditional inference tree
library(party)
churn.ci = ctree(churn ~ ., data=trainset)
churn.ci

# visualize the tree
plot(churn.ci) # too big 
# export the tree graph
png(file="d:/churn.ci.png",width=2000,height=1000)
plot(churn.ci, main="CI Tree")
dev.off()

# Performance
predictions = predict(churn.ci, testset)
table(predictions, testset$churn)
# Performance: Confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))

# random forest
install.packages("randomForest")
library(randomForest)
churn.rf = randomForest(churn ~ ., data=trainset, importance=T)
churn.rf
importance(churn.rf)


# Performance
predictions = predict(churn.rf, testset)
table(predictions, testset$churn)
# Performance: Confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))


