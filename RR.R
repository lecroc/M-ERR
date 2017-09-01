# Miracle-Ear Response Rate

library(ggplot2)
library(caret)
library(dplyr)
library(MASS)

## Get Data

setwd("H:/direct_r/Miracle-Ear/Analytics/Funnel Analysis August 2017/M-ERR")

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

# Create RR field and comp field (response rate and composition)

rrd$RR<-rrd$Leads/(rrd$Oldest*1000)
rrd$CPM<-rrd$SpotCost/rrd$Oldest

# Drop columns we don't need
rrd$Program<-NULL
rrd$Length<-NULL
rrd$Source<-NULL
rrd$SpotCost<-NULL
rrd$Calls<-NULL
rrd$Leads<-NULL

# Plots

qplot(Oldest, data=rrd, fill=TVType, bins=25)
qplot(Old, data=rrd, fill=TVType, bins=25)
qplot(Base, data=rrd, fill=TVType, bins=25)
qplot(RR, data=rrd, fill=TVType, bins=25)
qplot(CPM, data=rrd, fill=TVType, bins=25)

# No numeric variables have normal distribution - convert to log values

rrd$Oldest<-log(rrd$Oldest)
rrd$Old<-log(rrd$Old)
rrd$Base<-log(rrd$Base)
rrd$RR<-log(rrd$RR)
rrd$CPM<-log(rrd$CPM)

# Re-plot to see impact of log transformations

qplot(Oldest, data=rrd, fill=TVType, bins=25)
qplot(Old, data=rrd, fill=TVType, bins=25)
qplot(Base, data=rrd, fill=TVType, bins=25)
qplot(RR, data=rrd, fill=TVType, bins=25)
qplot(CPM, data=rrd, fill=TVType, bins=25)


# Boxplots to see relationships

p1<-ggplot(rrd, aes(x=DayPart, y=RR, fill=DayPart))+geom_boxplot()+ggtitle("RR by TVType, Daypart")+facet_grid(.~TVType)

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

m1<-lm(RR ~ Month + DayPart + SpotName + Oldest + Base + CPM, data=rrtrn)

pred1<-predict(m1, rrtst)

rrtst$predRR<-pred1

