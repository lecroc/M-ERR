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
library(doSNOW)
library(foreach)


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
inTrain<- createDataPartition(y=mdata$RR, p=0.8, list=F, times=1)
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

# initialize parallel processing

getDoParWorkers()
registerDoSNOW(makeCluster(16, type="SOCK"))
getDoParWorkers()
getDoParName()

# Turn off scientific notation so we can see decimals

options(scipen = 999)


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

# Cubist

set.seed(1645)
cfit<-train(RR~., data=trn, method="cubist", trControl=fitControl)
cfit

# evaluate Cubist
pc<-predict(cfit, trn)
e4<-trn$RR-pc
rmse(e4)
mae(e4)

# Ridge Regression

set.seed(456)

rrfit<-train(RR~., data=trn, method="ridge", trControl=fitControl)
rrfit

# evaluate Ridge
prr<-predict(rrfit, trn)
e5<-trn$RR-prr
rmse(e5)
mae(e5)


# Lasso

set.seed(987)

lsfit<-train(RR~., data=trn, method="lasso", trControl=fitControl)
lsfit

# evaluate Lasso
prls<-predict(lsfit, trn)
e6<-trn$RR-prls
rmse(e6)
mae(e6)


# PCA

set.seed(12345)
pcafit<-train(RR~., data=trn, method="pcr", trControl=fitControl)
pcafit

# evalutae principal components
ppca<-predict(pcafit, trn)
e7<-trn$RR-ppca
rmse(e7)
mae(e7)

# Extreme Gradient Boost

trndt<-data.table(trn, keep.rownames = F)
tstdt<-data.table(tst, keep.rownames = F)

xgbfit<-train(RR~., data=trndt, method="xgbLinear", trCrontrol=fitControl)
xgbfit

#evaluate Xgb
pxgb<-predict(xgbfit, trndt)
e8<-trndt$RR-pxgb
rmse(e8)
mae(e8)

# Just use the average RR
pavg<-mean(trn$RR)
e9<-trn$RR-pavg
rmse(e9)
mae(e9)

# Compile results from all models, compare to taking an average

RMSEs<-c(rmse(e), rmse(e1), rmse(e2), rmse(e3), rmse(e4), rmse(e5), rmse(e6), rmse(e7), rmse(e8), rmse(e9))
MAEs<-c(mae(e), mae(e1), mae(e2), mae(e3), mae(e4), mae(e5), mae(e6), mae(e7), mae(e8), mae(e9))
Models<-c("SVM", "MARS", "glm", "RF", "Cubist", "Ridge", "Lasso", "PCA", "XGB", "AVG")

Results<-as.data.frame(cbind(Models, RMSEs, MAEs))
View(Results)

StackTrain<-as.data.frame(cbind(trn$RR, psvm, pmf, pglm, prf, pc, prr, prls, ppca, pxgb))
names(StackTrain)<-c("RR", "psvm", "pmf", "pglm", "prf", "pc", "prr", "prls", "ppca", "pxgb")

View(StackTrain)

# stack model

set.seed(234)

stack<-train(RR~., data=StackTrain, method="bagEarth", trainControl=fitControl)
pstack<-predict(stack, StackTrain)
e10<-StackTrain$RR-pstack
rmse(e10)
mae(e10)

# predict test data

tpsvm<-predict(svmfit, tst)
tpmf<-predict(marsfit, tst)
tpglm<-predict(glmfit, tst)
tprf<-predict(rffit, tst)
tpc<-predict(cfit, tst)
tprr<-predict(rrfit, tst)
tpls<-predict(lsfit, tst)
tppca<-predict(pcafit, tst)
tpxgb<-predict(xgbfit, tstdt)

StackTest<-as.data.frame(cbind(tst$RR, tpsvm, tpmf, tpglm, tprf, tpc, tprr, tpls, tppca, tpxgb))
names(StackTest)<-c("RR", "psvm", "pmf", "pglm", "prf", "pc", "prr", "prls", "ppca", "pxgb")

pstacktst<-predict(stack, StackTest)
e11<-StackTest$RR-pstacktst
rmse(e11)
mae(e11)


