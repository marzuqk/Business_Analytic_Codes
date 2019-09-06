#################################################################
# EFA for finding dimensions
#################################################################

# load the data
brand.ratings = read.csv("http://goo.gl/IQl8nc")
# normalize the brand data
brand.sc = brand.ratings
brand.sc[, 1:9] = scale(brand.ratings[, 1:9])
summary(brand.sc)
head(brand.sc)

# EFA
install.packages(c("nFactors", "GPArotation"))
library(nFactors)
library(GPArotation)
library(semPlot)
nScree(brand.sc[,1:8])
factanal(brand.sc[,1:8], factors=3, rotation="oblimin")
semPaths(factanal(brand.sc[,1:8], factors=3, rotation="oblimin"), what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)

# factor scores and rebuy 
brand.fa.ob = factanal(brand.sc[, 1:8], factors=3, rotation="oblimin", scores="Bartlett")
brand.scores = data.frame(brand.fa.ob$scores)
brand.scores$rebuy = brand.sc$rebuy
names(brand.scores) = c("Leader", "Value", "Latest", "Rebuy")
head(brand.scores)
summary(lm(Rebuy~., data=brand.scores))

# factor scores and brand 
brand.fa.ob = factanal(brand.sc[, 1:8], factors=3, rotation="oblimin", scores="Bartlett")
brand.scores = data.frame(brand.fa.ob$scores)
brand.scores$brand = brand.sc$brand
brand.fa.mean = aggregate(. ~ brand, data=brand.scores, mean)
rownames(brand.fa.mean) = brand.fa.mean[, 1]
brand.fa.mean = brand.fa.mean[, -1]
names(brand.fa.mean) = c("Leader", "Value", "Latest")
brand.fa.mean


#################################################################
# A second-order CFA example: Product involvement measurement 
#################################################################

### load data 
piesSimData = read.csv("http://goo.gl/yT0XwJ")
summary(piesSimData)

install.packages(c("lavaan", "semTools", "semPlot"))   
library(lavaan)
library(semTools)
library(semPlot)

# define the measurement model
piesModel = " General =~ i1 + i2 + i3
               Feature =~ i4 + i5 + i6  + i7
               Image   =~ i8 + i9 + i10 + i11
               PIES =~ General + Feature + Image "

# CFA
piesModel.fit = cfa(piesModel, data=piesSimData)
summary(piesModel.fit, fit.measures=TRUE)
semPaths(piesModel.fit, what="est", fade=FALSE, residuals=FALSE,
         edge.label.cex=0.75) 

# define a competing model (one factor model)
piesModel2 = " PIES =~ i1 + i2 + i3 + i4 + i5 + i6  + i7 + i8 + i9 + i10 + i11
             "

piesModel2.fit = cfa(piesModel2, data=piesSimData)
summary(piesModel2.fit, fit.measures=TRUE)

# compare models
library(semTools)
comp = compareFit(piesModel.fit, piesModel2.fit)
summary(comp)
anova(piesModel.fit, piesModel2.fit)


#################################################################
# SEM
#################################################################

satData = read.csv("http://goo.gl/MhghRq")
summary(satData)

satModel = "
              Quality =~ q1 + q2 + q3
              Cost    =~ c1 + c2 + c3
              Value   =~ v1 + v2 + v3
              CSat    =~ cs1 + cs2 + cs3
              Repeat  =~ r1 + r2 + r3 
              Value ~ Cost + Quality
              CSat ~ Value + Quality
              Repeat ~ Cost + CSat
          "

SEM.fit = sem(satModel, data=satData)
summary(SEM.fit, fit.measures=TRUE)

# plot it
library(semPlot)
semPaths(SEM.fit, what="est", fade=FALSE, residuals=FALSE,
         edge.label.cex=0.75)

# model comparison
competingModel = "
              Quality =~ q1 + q2 + q3
              Cost    =~ c1 + c2 + c3
              Value   =~ v1 + v2 + v3
              CSat    =~ cs1 + cs2 + cs3
              Repeat  =~ r1 + r2 + r3 
              Value ~ Cost 
              CSat ~ Value + Quality
              Repeat ~ CSat
              "
SEM2.fit = sem(competingModel, data=satData)
summary(SEM2.fit, fit.measures=TRUE)

# plot it
semPaths(SEM2.fit, what="est", fade=FALSE, residuals=FALSE,
         edge.label.cex=0.75)

# compare models
library(semTools)
comp = compareFit(SEM.fit, SEM2.fit)
summary(comp)
anova(SEM.fit, SEM2.fit)

