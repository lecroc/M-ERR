# Miracle-Ear RR Predictor

# libraries

library(forcats)
library(MASS)
library(caret)
library(xgboost)
library(data.table)
library(Matrix)
library(methods)
library(vcd)
library(e1071)
library(DMwR)
library(mboost)
library(randomForest)
library(plyr)
library(earth)
library(plotmo)
library(plotrix)
library(TeachingDemos)
library(kernlab)
library(ggplot2)
library(neuralnet)
library(Cubist)
library(elasticnet)
library(pls)



# set up functions for RMSE and MAE

rmse<-function(error)
{
  sqrt(mean(error^2))
}

mae<-function(error)
{
  mean(abs(error))
}

# get data
mldat<-read.csv("./mlrrd.csv")

# get rid of X column
mldat<-mldat[,2:12]

# Separate numeric and factor variables
nums<-sapply(mldat, is.numeric)
numerics<-mldat[, nums]
factors<-mldat[, !nums]

# dummify factors
dmy <- dummyVars(" ~ .", data = factors)
dummies <- data.frame(predict(dmy, newdata = factors))

# Combine dummified factors with numerics
mdata<-as.data.frame(cbind(numerics, dummies))

# re-order to put RR first
mdata<-mdata[c(4,1,2,3,5:37)]

# Create train and test sets 
set.seed(1234)
inTrain<- createDataPartition(y=mdata$RR, p=0.6, list=F, times=1)
trn<-mdata[inTrain,]
tst<-mdata[-inTrain,]

# Pre-process trn
preObj<-preProcess(trn[, 2:5], method=c("center", "scale"))
proc<-predict(preObj, trn[, 2:5])

trn1<-as.data.frame(cbind(trn$RR, proc, trn[, 6:37]))
names(trn1)<-names(trn)
trn<-trn1

# Pre-process tst with preprocess object from trn
proc2<-predict(preObj, tst[, 2:5])

tst1<-as.data.frame(cbind(tst$RR, proc2, tst[, 6:37]))
names(tst1)<-names(tst)
tst<-tst1

# Set up fit control
fitControl<-trainControl(method="repeatedcv", number=10, repeats = 10)

# SVM Model

set.seed(345)
svmfit<-train(RR~., data = trn, method="svmLinear", trControl=fitControl)

svmfit

# evaluate svm
psvm<-predict(svmfit, trn)
e<-trn$RR-psvm
rmse(e)
mae(e)

# Bagged MARS

set.seed(123)
marsfit<-train(RR~., data = trn, method="bagEarth", trControl=fitControl)
marsfit

# evaluate mars

pmf<-predict(marsfit, trn)
e1<-trn$RR-pmf
rmse(e1)
mae(e1)

# Boosted glm

set.seed(456)
glmfit<-train(RR~., data = trn, method="glmboost", trControl=fitControl)
glmfit

# evaluate glm
pglm<-predict(glmfit, trn)
e2<-trn$RR-pglm
rmse(e2)
mae(e2)

# Random Forest

set.seed(124)
rffit<-train(RR~., data=trn, method="rf", trControl=fitControl)
rffit

# evaluate Random Forest
prf<-predict(rffit, trn)
e3<-trn$RR-prf
rmse(e3)
mae(e3)

# Neural Network

set.seed(789)
nnfit<-train(RR~., data=trn, method="neuralnet", trControl=fitControl)
nnfit

# evaluate Neural Network
pnn<-predict(nnfit, trn)
e4<-trn$RR-pnn
rmse(e4)
mae(e4)

# Cubist

set.seed(1645)
cfit<-train(RR~., data=trn, method="cubist", trControl=fitControl)
cfit

# evaluate Cubist
pc<-predict(cfit, trn)
e5<-trn$RR-pc
rmse(e5)
mae(e5)

# Ridge Regression

set.seed(456)

rrfit<-train(RR~., data=trn, method="ridge", trControl=fitControl)
rrfit

# evaluate Ridge
prr<-predict(rrfit, trn)
e6<-trn$RR-prr
rmse(e6)
mae(e6)


# Lasso

set.seed(987)

lsfit<-train(RR~., data=trn, method="lasso", trControl=fitControl)
lsfit

# evaluate Robust Linear Regression
prls<-predict(lsfit, trn)
e7<-trn$RR-prls
rmse(e7)
mae(e7)

# PCA

set.seed(12345)
pcafit<-train(RR~., data=trn, method="pcr", trControl=fitControl)
pcafit

# evalutae principal components
ppca<-predict(pcafit, trn)
e8<-trn$RR-ppca
rmse(e8)
mae(e8)

