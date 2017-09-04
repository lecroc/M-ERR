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

# Scale numeric predictors
scl<-scale(numerics[,c(1:3, 5:6)], center = T)

#Vector of target Variable
RR<-mldat$RR

# Combine target, scaled predictor numerics, dummy predictors
mdata<-cbind(RR, scl, dummies)

# Create train and test sets 
set.seed(1234)
inTrain<- createDataPartition(y=mdata$RR, p=0.7, list=F, times=1)
trn<-mdata[inTrain,]
tst<-mdata[-inTrain,]

## Model xgBoost

set.seed(4567)

trdf<- data.table(trn, keep.rownames = F)
tedf<-data.table(tst, keep.rownames = F)

mxgB<-train(SalePrice~., data=trdf, method="xgbLinear")

pxgB<-predict(mxgB, tedf)
rmsexgB<-sqrt(mean((trn$RR-pxgB)^2))
rmsexgB