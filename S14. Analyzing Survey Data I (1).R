#################################################################
# load the class survey data
#################################################################

raw=read.csv("C:/tmp/raw.csv")

# data summary 
str(raw)
summary(raw)
head(raw)

#################################################################
# EFA
#################################################################

install.packages(c("nFactors", "GPArotation"))
library(nFactors)
library(GPArotation)
nScree(raw[,1:9])
factanal(raw[,1:9], factors=3, rotation="oblimin")

#################################################################
# CFA
#################################################################

install.packages(c("lavaan", "semTools", "semPlot"))   
library(lavaan)
library(semTools)
library(semPlot)

Model1 =  ' PV =~ x1_1 + x1_2 + x1_3
            SE =~ x2_1 + x2_2 + x2_3
            LE =~ x3_1 + x3_2 + x3_3'

Model1.fit = cfa(Model1, data=raw)
summary(Model1.fit, fit.measures=TRUE)

Model2 =  ' CM =~ x1_1 + x1_2 + x1_3 + x2_1 + x2_2 + x2_3 + x3_1 + x3_2 + x3_3' 

Model2.fit = cfa(Model2, data=raw)
summary(Model2.fit, fit.measures=TRUE)

# compare models
library(semTools)
comp = compareFit(Model1.fit, Model2.fit, nested=FALSE)
summary(comp)
anova(Model1.fit, Model2.fit)

#################################################################
# SEM
#################################################################

SEM1 = '    
    # measurement model
    PV =~ x1_1 + x1_2 + x1_3
    SE =~ x2_1 + x2_2 + x2_3
    LE =~ x3_1 + x3_2 + x3_3
    # regressions
    LE ~ PV + SE
    '
SEM1.fit = sem(SEM1, data=raw)
summary(SEM1.fit, fit.measures=TRUE)

# plot it
library(semPlot)
semPaths(SEM1.fit, what="est", fade=FALSE, residuals=FALSE,
         edge.label.cex=0.75)



