# load the library and the data
install.packages("C50")
library(C50)
data(churn)

# explore the data structure
str(churnTrain)
head(churnTrain)

# remove state, area_code, account_length which are not relevant for classification
churnTrain = churnTrain[, !names(churnTrain) %in% c("state", "area_code", "account_length")]

# split 70 percent of the data into the training dataset and 30 percent into the testing dataset
set.seed(1234)
index = sample(2, nrow(churnTrain), replace = TRUE, prob = c(0.7, 0.3))
trainset = churnTrain[index==1,]
testset = churnTrain[index==2,]

# check the data size
dim(trainset)
dim(testset)

#################################################################
# Tree Model and Random Forest Model
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

# performance
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

# performance
predictions = predict(churn.ci, testset)
table(predictions, testset$churn)
# performance: Confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))

# random forest
install.packages("randomForest")
library(randomForest)
churn.rf = randomForest(churn ~ ., data=trainset, importance=T)
churn.rf
importance(churn.rf)

# performance
predictions = predict(churn.rf, testset)
table(predictions, testset$churn)
# Performance: Confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))

#################################################################
# K-nearest neighbor classifier
#################################################################

install.packages("class")
library(class)

# replace "yes" and "no" to "1" and "0" (to calculate the distance)
levels(trainset$international_plan) = list("0"="no", "1"="yes")
levels(trainset$voice_mail_plan) = list("0"="no", "1"="yes")
levels(testset$international_plan) = list("0"="no", "1"="yes")
levels(testset$voice_mail_plan) = list("0"="no", "1"="yes")

# knn (k=3)
churn.knn = knn(trainset[,!names(trainset) %in% c("churn")], 
                testset[,!names(testset) %in% c("churn")], trainset$churn, k=4)
summary(churn.knn)

# performance
predictions = churn.knn
table(predictions, testset$churn)
# Performance: Confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))

# What is the sensitivity of the model if k=4? (uReply)

#################################################################
# Logistic regresssion
#################################################################

# logistic regression
fit = glm(churn ~., data=trainset, family=binomial)
summary(fit)

# keep significant variables only
fit2 = glm(churn ~ international_plan + voice_mail_plan + number_vmail_messages
          + total_intl_calls + number_customer_service_calls, data=trainset, family=binomial)
summary(fit2)

# model comparison
library(lmtest)
lrtest(fit, fit2)

# prediction
pred = predict(fit, testset, type="response")
predictions = (pred <.7) # try different cut-offs

# performance
table(predictions, testset$churn)
testset$churn_l=ifelse(testset$churn=="yes", TRUE, FALSE)
table(predictions, testset$churn_int)
# Performance: Confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn_l))

# finding the best cutoff
# install.packages("pROC")
library(pROC)
churn.roc=roc(testset$churn, pred)
plot(churn.roc)
coords(churn.roc, "best")


#################################################################
# Naive Bayes
#################################################################

# Naive Bayes
library(e1071)
churn.NB = naiveBayes(trainset[,!names(trainset) %in% c("churn")], trainset$churn)
churn.NB

# performance
predictions = predict(churn.NB, testset[,!names(testset) %in% c("churn")])
table(predictions, testset$churn)
# performance: confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))

#################################################################
# Support Vector Machine
#################################################################

# SVM
library(e1071)
churn.SVM = svm(churn~., data=trainset, kernel="radial", cost=1, gamma=1/ncol(trainset))
summary(churn.SVM)

# performance
predictions = predict(churn.SVM, testset[,!names(testset) %in% c("churn")])
table(predictions, testset$churn)
# performance: confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))

#################################################################
# Neural network
#################################################################

# Neural network
install.packages("neuralnet")
library(neuralnet) # clash with ROCR package
# detach("package:ROCR", unload=TRUE)

trainset$yes = trainset$churn == "yes"
trainset$no = trainset$churn == "no"
testset$yes = testset$churn == "yes"
testset$no = testset$churn == "no"
trainset$international_plan=as.numeric(trainset$international_plan)
trainset$voice_mail_plan=as.numeric(trainset$voice_mail_plan)
testset$international_plan=as.numeric(testset$international_plan)
testset$voice_mail_plan=as.numeric(testset$voice_mail_plan)


churn.NN = neuralnet(yes+no ~ scale(international_plan)+scale(voice_mail_plan)+scale(number_vmail_messages)
                     +scale(total_intl_calls)+scale(number_customer_service_calls), trainset, hidden=3)
churn.NN
churn.NN$result.matrix

# visualize
plot(churn.NN)

# prediction
pred = compute(churn.NN, scale(testset[,c(1:3, 14, 16)]))$net.result
predictions = c("yes", "no")[apply(pred, 1, which.max)] # class(predictions)==character
table(predictions, testset$churn)
testset$churn=ifelse(testset$churn=="yes","yes","no") # change the type to the character for the confustion matrix
# performance: confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))

#################################################################
# GBM
#################################################################

# GBM
install.packages("gbm")
library(gbm)

head(trainset)
trainset=trainset[,!names(trainset) %in% c("yes", "no")]
testset=testset[,!names(testset) %in% c("yes", "no")]

trainset$churn = ifelse(trainset$churn=="yes", 1, 0)
testset$churn = ifelse(testset$churn=="yes", 1, 0)


set.seed(1234)
churn.GBM=gbm(churn~., distribution="bernoulli", data=trainset, n.trees=1000, 
              interaction.depth=7, shrinkage=0.01, cv.folds = 3)
summary(churn.GBM)

# prediction
pred = predict(churn.GBM, testset, n.trees=1000)

# finding the best cut-off 
# install.packages("pROC")
library(pROC)
churn.roc=roc(testset$churn, pred)
plot(churn.roc)
coords(churn.roc, "best")

# obtaining the classification table
predictions = ifelse(pred>coords(churn.roc, "best") ["threshold"], 1, 0)
table(predictions, testset$churn)
# performance: confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))



