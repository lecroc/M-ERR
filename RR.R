# Miracle-Ear Response Rate

library(ggplot2)
library(caret)
library(dplyr)
library(MASS)

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

# convert spots with zero leads to small non-zero number

#zero<-sum(rrd$Leads==0)
#newlead<-1/zero
#rrd<-mutate(rrd, Leads = ifelse(test=Leads==0, yes=newlead, no=Leads))
#rrd$Leads<-as.numeric(format(round(rrd$Leads, 2), nsmall = 2))

# Drop spots with 0 leads

rrd<-subset(rrd, rrd$Leads>0)

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

# Create RR field, a CPM field and comp field (response rate, CPM and composition)

rrd$RR<-rrd$Leads/(rrd$Oldest*1000)
rrd$CPM<-rrd$SpotCost/rrd$Oldest
rrd$comp<-rrd$Oldest/rrd$Base

# Drop columns we don't need
rrd$Program<-NULL
rrd$Length<-NULL
rrd$Source<-NULL
rrd$SpotCost<-NULL
rrd$Calls<-NULL
rrd$Leads<-NULL

# write.csv(rrd, "./mlrrd.csv")

# Plots

qplot(Oldest, data=rrd, fill=TVType, bins=25)
qplot(Old, data=rrd, fill=TVType, bins=25)
qplot(Base, data=rrd, fill=TVType, bins=25)
qplot(RR, data=rrd, fill=TVType, bins=25)
qplot(CPM, data=rrd, fill=TVType, bins=25)
qplot(comp, data=rrd, fill=TVType, bins=25)

# No numeric variables have normal distribution - convert to log values

# rrd$Oldest<-log(rrd$Oldest)
# $rrd$Old<-log(rrd$Old)
# rrd$Base<-log(rrd$Base)
# rrd$RR<-log(rrd$RR)
# rrd$CPM<-log(rrd$CPM)
# rrd$comp<-log(rrd$comp)

# Tried the log transform, got better results with values as-is

# Re-plot to see impact of log transformations

# qplot(Oldest, data=rrd, fill=TVType, bins=25)
# qplot(Old, data=rrd, fill=TVType, bins=25)
# qplot(Base, data=rrd, fill=TVType, bins=25)
# qplot(RR, data=rrd, fill=TVType, bins=25)
# qplot(CPM, data=rrd, fill=TVType, bins=25)
# qplot(comp, data=rrd, fill=TVType, bins=25)


# Boxplots to see relationships

p1<-ggplot(rrd, aes(x=DayPart, y=RR, fill=DayPart))+geom_boxplot()+ggtitle("RR by TVType, Daypart")+facet_grid(.~TVType)+theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))

p1

p2<-ggplot(rrd, aes(x=SpotName, y=RR, fill=SpotName))+geom_boxplot()+ggtitle("RR by Spot, TVType")+facet_grid(TVType~DayPart)+theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))

p2

p3<-ggplot(rrd, aes(x=ProgramType, y=RR, fill=ProgramType))+geom_boxplot()+ggtitle("RR by ProgramType, TVType")+facet_grid(.~TVType)+theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))

p3

# Split data into training and testing sets

set.seed(1234)

inTrain<-createDataPartition(y=rrd$RR, p=.8, list=F, times=1)
rrtrn<-rrd[inTrain,]
rrtst<-rrd[-inTrain,]

sum(rrtrn$TVType=="SY")
sum(rrtrn$TVType=="NW")
sum(rrtst$TVType=="SY")
sum(rrtst$TVType=="NW")


# Step-wise linear model to see what we get......

 fit<-lm(RR~., data=rrtrn)
 step<-stepAIC(fit, direction = "both")
 step$anova

# turn off scientific notation
options(scipen = 999)

m1<-lm(RR ~ ProgramType+DayPart+SpotName+Oldest+Old+Base+CPM+comp, data=rrtrn)
summary(m1)

e<-rrtrn$RR-m1$fitted.values

rmse(e)
mae(e)

# plot our model estimates vs. actuals

actual<-rrtrn$RR

fitted<-m1$fitted.values

plot(actual~fitted, col=rrtrn$TVType)
abline(lm(actual~fitted))

# Separate numeric and factor variables from rrtrn
nums<-sapply(rrtrn, is.numeric)
numerics<-rrtrn[, nums]
factors<-rrtrn[, !nums]

results<-as.data.frame(cbind(actual, fitted, factors))

results$diff<-results$actual-results$fitted

View(results)

p4<-ggplot(results, aes(x=SpotName, y=diff, fill=TVType))+geom_boxplot()+ggtitle("Error by Spot, TVType")+facet_grid(.~TVType)+theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))

p4

p5<-ggplot(results, aes(x=ProgramType, y=diff, fill=ProgramType))+geom_boxplot()+ggtitle("Error by Program, TVType")+facet_grid(.~TVType)+theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))

p5



# Predict test with regression model

pred1<-predict(m1, rrtst)

pe<-rrtst$RR-pred1

rmse(pe)
mae(pe)

# Compare model to just using an average

avgpred<-mean(rrtst$RR)
avpe<-rrtst$RR-avgpred

rmse(avpe)
mae(avpe)

# plot our model predictions vs. actuals

actualp<-rrtst$RR

fittedp<-pred1

plot(actualp~fittedp, col=rrtst$TVType)
abline(lm(actualp~fittedp))

# Separate numeric and factor variables from rrtrn
nums<-sapply(rrtst, is.numeric)
numerics<-rrtst[, nums]
factors<-rrtst[, !nums]

results1<-as.data.frame(cbind(actualp, fittedp, factors))

results1$diff<-results1$actual-results1$fitted

View(results1)

p6<-ggplot(results1, aes(x=SpotName, y=diff, fill=TVType))+geom_boxplot()+ggtitle("Error by Spot, TVType")+facet_grid(.~TVType)+theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))

p6

p7<-ggplot(results1, aes(x=ProgramType, y=diff, fill=ProgramType))+geom_boxplot()+ggtitle("Error by Program, TVType")+facet_grid(.~TVType)+theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))

p7