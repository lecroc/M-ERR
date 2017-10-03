# Miracle-Ear Response Rate

library(ggplot2)
library(caret)
library(dplyr)
library(MASS)
library(countreg)

# set up functions for RMSE and MAE

rmse<-function(error)
{
  sqrt(mean(error^2))
}

mae<-function(error)
{
  mean(abs(error))
}

## Get Data

# setwd("H:/direct_r/Miracle-Ear/Analytics/Funnel Analysis August 2017/M-ERR")

rrd<-read.csv("./ResponseRate.csv")


# create a vector of records with no NAs

completes<-complete.cases(rrd)

# drop NAs

rrd<-rrd[completes,]

# drop spots with $0 cost

rrd<-subset(rrd, rrd$SpotCost>0)

# convert some integers to factors

rrd$DayPart<-factor(rrd$DayPart)
rrd$Length<-factor(rrd$Length)

# Change all syndication to DayPart=2
rrd<-mutate(rrd, DayPart = ifelse(test=TVType=="SY", yes="2", no=DayPart))

# Consolidate Dayparts
rrd<-mutate(rrd, DayPart = ifelse(test=DayPart=="1", yes="2", no=DayPart))
rrd<-mutate(rrd, DayPart = ifelse(test=DayPart=="3", yes="2", no=DayPart))
rrd<-mutate(rrd, DayPart = ifelse(test=DayPart=="6", yes="5", no=DayPart))
rrd<-mutate(rrd, DayPart = ifelse(test=DayPart=="2", yes="Day", no=DayPart))
rrd<-mutate(rrd, DayPart = ifelse(test=DayPart=="4", yes="Prime", no=DayPart))
rrd<-mutate(rrd, DayPart = ifelse(test=DayPart=="5", yes="OV", no=DayPart))
rrd<-mutate(rrd, DayPart = ifelse(test=DayPart=="7", yes="WE", no=DayPart))

# Create a CPM field and comp field (response rate, CPM and composition)

rrd$CPM<-rrd$SpotCost/rrd$Oldest
rrd$comp<-rrd$Oldest/rrd$Base

# Drop columns we don't need
rrd$Program<-NULL
rrd$Length<-NULL
rrd$Source<-NULL
rrd$SpotCost<-NULL
rrd$Calls<-NULL

# Step-wise linear model to see what we get......

fit<-lm(Leads~., data=rrd)
step<-stepAIC(fit, direction = "both")
step$anova

lm1<-lm(Leads ~ TVType + ProgramType + Month + DayPart + SpotName + Oldest + Base + CPM, data=rrd)

rrd$pred<-predict(lm1, rrd)