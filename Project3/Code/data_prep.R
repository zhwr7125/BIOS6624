#=====================================# 
# TITLE: PROJECT 3 (VA Data) Cleaning
# AUTHOR: Gordon Kordas
# DATE: 11/07/2018
#=====================================#

setwd("C:/repository/bios6624-zhwr7125/Project3")
 
library(ggplot2)
library(dplyr)
library(mice)
library(VIM)
library(gridExtra)
library(viridis)
library(gplots)


library(Table1)
library(Hmisc)
label(va$proced)<-"procedure"
label(va$asa)<-"patient's condition"
label(va$weight)<-"weight,lb"
label(va$height)<-"height, inches"
label(va$bmi)<-"BMI, kg/cm2"
label(va$albumin)<-"albumin, g/dL"
label(va$death30)<-"30 day mortality"
va$hospcode<-factor(va$hospcode)
va$proced<-factor(va$proced)
va$asa<-factor(va$asa)
va$death30<-factor(va$death30)
va$indicator<-factor(rep(1,26491))


levels(va$sixmonth)<-c("1980,1-6 month","1980, 7-12 month","1981, 1-6 month","1981, 7-12 month","1982, 1-6 month","1982, 7-12 month")


tab_miss<-missing_table(va,c('proced','asa','weight','height','bmi','albumin','death30'),va$indicator)
dim(tab_miss)
tab1<-final_table(va,c('proced','asa','weight','height','bmi','albumin','death30'),va$indicator,1,T,2)

#------------------#
# Read in data
#------------------#

dat <- read.csv("Data/vadata2.csv") 
factor_vars <- c(2:4)
dat[,factor_vars] <- lapply(dat[,factor_vars] , factor)

dat <- dat[dat$proced != "2",]

table(dat$asa, dat$death30)
# there were very few people with asa of 1 and all survived beyond 30 days. Let's combine asa 1 and 2
dat[dat$asa == "1" & !is.na(dat$asa),]$asa <- "2"

# some BMIs are unrealistic. Let's remove those above 54
# there is a bmi of 2.5, which is crazy low, but realistically not impossible. We'll keep them for now
dat[dat$bmi > 54 & !is.na(dat$bmi),]$bmi <- NA


# logistic regression
training <- dat[dat$sixmonth != "39",]
m1 <- glm(death30 ~ albumin + bmi + asa + proced, data = training)

dat_sub <- dat[,c(1,2,3,4,7,8,9)]
dat_sub <- dat_sub[complete.cases(dat_sub),]
t_sub <- training[,c(1,2,3,4,7,8,9)]
t_sub <- t_sub[complete.cases(t_sub),]

dat_sub$t_pred <- predict(m1,newdata = dat_sub, type = "response")

#get only the periods prior to 39 with complete cases
dat_sub <- dat_sub[dat_sub$sixmonth != 39,]

#the predicted death rate for each hospital based on historic data
expected <- aggregate(dat_sub$t_pred,list(dat_sub$hospcode),mean)*100
colnames(expected) <- c("hospital", "exp_rate")
expected$hospital <- expected$hospital/100