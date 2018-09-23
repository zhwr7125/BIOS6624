#tried table 1
library(Hmisc)
library(knitr)
library(devtools)
library(Table1)
source('Macro.R')

data<-read.csv("/Users/zhouwenru/Documents/UCD Courses/BIOS6624/PRO1/Data processed/hiv_6624_final.csv",header=T,na.strings = "NA")

#factorize;
data$ART<-as.factor(data$ART)
data$income<-as.factor(data$income)
data$SMOKE<-as.factor(data$SMOKE)
data$RACE<-as.factor(data$RACE)
data$EDUCBAS<-as.factor(data$EDUCBAS)
data$hard_drugs<-as.factor(data$hard_drugs)

#set lables;
label(data$newid)="Subject ID Number"
label(data$AGG_MENT)='AGG MENT'
label(data$AGG_PHYS)='AGG PHYS'
label(data$HASHV)="Hash/marijuana since last visit"
label(data$HASHF)="Frequency used harsh/marijuana since last visit"
label(data$HBP)="High blood pressure"
label(data$DIAB)="diabetes"
label(data$LIV34)="Liver disease stage 3/4"
label(data$KID)="Kidney disease"
label(data$FRP)="Fraility Related Phenotype"
label(data$FP)="Frailty Phenotype"
label(data$years)="years since iniating ART"
label(data$hard_drugs)="Hard drug use since last visit"
label(data$hivpos)="HIV Serostatus"
label(data$TCHOL)="Total Cholesterol"
label(data$TRIG)="Triglycerides"
label(data$LDL)="Low density lipoprotein"
label(data$DYSLIP)="dyslipidemia at visit"
label(data$CESD)="Depression scale"
label(data$DKGRP)="Alcohol use since last visit"
label(data$HEROPIATE)="Took heroin or other opiates since last visit?"
label(data$IDU)="Took/used drugs with a needle since last visit?"
label(data$LEU3N)="# of CD4 positive cells(helpers)"
label(data$ADH)="Adherence to meds taken since last visit"
label(data$ART)="Take ART at visit"
label(data$everART)="Ever Took ART"
label(data$SMOKE)="Smoke"
label(data$income)="income"
label(data$BMI)="BMI"
label(data$VLOAD)="virus load"
label(data$age)="Age, years"
label(data$RACE)="Race, 8 levels"
label(data$income)="Income, 8 levels"

#Combine race
data$race



#levels;
levels(data$ART)=c("No ART",'ART')



#create a new subset
data_easy<-subset(data[,c("newid","AGG_MENT","AGG_PHYS","LEU3N","VLOAD","ART","years","hard_drugs","age","BMI","ADH","RACE","income","SMOKE")],data$years<4&data$years>0)

#Create baseline data, Group those who had hard drugs at baseline and whose who did not
missing_hard_drugs_count<-dim(subset(data_easy,data_easy$years==1&is.na(data_easy$hard_drugs)==TRUE))[1]
missing_hard_drugs_rate<-dim(subset(data_easy,data_easy$years==1&is.na(data_easy$hard_drugs)==TRUE))[1]/dim(subset(data_easy,data_easy$years==1))[1]

data_base<-subset(data_easy,data_easy$years==1&is.na(data_easy$hard_drugs)==FALSE)
data_base$group[data_base$hard_drugs==1]=1
data_base$group[data_base$hard_drugs==0]=0
data_base$group<-as.factor(data_base$group)
label(data_base$group)="hard drug at baseline"
data_base$years<-as.factor(data_base$years)

#Create year three data
data_three<-merge(data_base[,c("newid","group")],subset(data_easy,data_easy$years==3),by="newid")
data_three$years<-as.factor(data_three$years)
label(data_three$hard_drugs)="Hard drug use since last visit"


#Baseline and Two years after
data_long<-rbind(data_base,data_three)
data_long$group<-as.factor(data_long$group)
label(data_long$group)="hard drug at baseline"
data_long$log_VLOAD<-log(data_long$VLOAD)
data_long$log_BMI<-log(data_long$BMI)
label(data_long$log_VLOAD)="log # virus load"
label(data_long$RACE)="Race, 8 levels"
label(data_long$income)="Income, 8 levels"
label(data_long$SMOKE)="Smoke"
var<-c("newid","AGG_MENT","AGG_PHYS","LEU3N","VLOAD","log_VLOAD","group","age","BMI","ADH","RACE","income","SMOKE")
con_missing<-missing_table(data_long,var,data_long$years)
con_final<-final_table(data_long,var,data_long$years,margin=2,single=F,ron=2)

data_wide<-merge(data_base[,c("newid","AGG_MENT","AGG_PHYS","LEU3N","VLOAD","years","age","BMI","ADH","RACE","income","SMOKE","group")],data_three[,c("newid","AGG_MENT","AGG_PHYS","LEU3N","VLOAD","years","age","BMI","ADH","RACE","income","SMOKE")],by="newid")

