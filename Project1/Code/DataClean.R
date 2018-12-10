library(Hmisc)
library(knitr)
library(devtools)
library(Table1)
#To get Table1, please visit 
#https://rdrr.io/github/palmercl/Table1/

#install_github("palmercl/Table1")

#But I made some revise to Table 1, therefore use Macro.R to mask it.
setwd("C:/repository/bios6624-zhwr7125/Project1/Code")
source('Macro.R')

#Reading data
data<-read.csv("C:/repository/bios6624-zhwr7125/Project1/Data/hiv_6624_final.csv",header=T,na.strings = "NA")
#table(data$EDUCBAS[data$years==0])
#Make variables, some has low number

data$RACE_ori<-data$RACE
data$RACE[data$RACE_ori==1|data$RACE_ori==2]=1#white
data$RACE[data$RACE_ori==3|data$RACE_ori==4|data$RACE_ori==7|data$RACE_ori==8]=0#nON-WHITE


data$SMOKE_ori<-data$SMOKE
data$SMOKE[data$SMOKE_ori==1|data$SMOKE_ori==2]=0#No current smoke
data$SMOKE[data$SMOKE_ori==3]=1#current smoke

data$ADH_ori<-data$ADH
data$ADH[data$ADH_ori==1|data$ADH_ori==2]=1#Adh
data$ADH[data$ADH_ori==3|data$ADH_ori==4]=0#Not adh

data$EDUCBAS_ori<-data$EDUCBAS
data$EDUCBAS[data$EDUCBAS_ori==1|data$EDUCBAS_ori==2|data$EDUCBAS_ori==3|data$EDUCBAS==4]=1#LESS COLLEGE
data$EDUCBAS[data$EDUCBAS_ori==5]=2#COLLEGE
data$EDUCBAS[data$EDUCBAS_ori==6|data$EDUCBAS_ori==7]=3#GREATER THAN COLLEGE


#factorize;
data$ART<-as.factor(data$ART)
data$SMOKE<-as.factor(data$SMOKE)
data$RACE<-as.factor(data$RACE)
data$hard_drugs<-as.factor(data$hard_drugs)
data$ADH<-as.factor(data$ADH)
data$EDUCBAS<-as.factor(data$EDUCBAS)


#set lables;
label(data$newid)="Subject ID Number"
label(data$AGG_MENT)='SF36 MCS score'
label(data$AGG_PHYS)='SF36 PCS score'
label(data$years)="years since initiating ART"
label(data$hard_drugs)="Hard drug use since last visit"
label(data$LEU3N)="# of CD4 positive cells (helpers)"
label(data$ADH)="If Adherence to meds since last visit"
label(data$ART)="Take ART at visit"
label(data$SMOKE)="Smoke"
label(data$BMI)="BMI(kg/m2)"
label(data$VLOAD)="Standardized viral load (copies/ml)"
label(data$age)="Age, years"
label(data$RACE)="Race"
label(data$EDUCBAS)="Education"

#levels;
levels(data$ART)=c("No ART",'ART')
levels(data$SMOKE)=c('Non-Current smoker','Current smoker')
table(data$SMOKE_ori,data$SMOKE)
table(is.na(data$SMOKE_ori))
table(is.na(data$SMOKE))

levels(data$RACE)=c('Non-White','White')
table(data$RACE_ori,data$RACE)
table(is.na(data$RACE_ori))
table(is.na(data$RACE))


levels(data$ADH)=c("No",'Yes')
table(data$ADH_ori,data$ADH)
table(is.na(data$ADH_ori))
table(is.na(data$ADH))

levels(data$EDUCBAS)=c("less than college",'college','greater than college')
table(data$EDUCBAS_ori,data$EDUCBAS)
table(is.na(data$EDUCBAS_ori))
table(is.na(data$EDUCBAS))

#create a new subset with only year 0 and 2
data_easy<-subset(data[,c("newid","AGG_MENT","AGG_PHYS","LEU3N","VLOAD","ART","years","hard_drugs","age","BMI","ADH","RACE","SMOKE","EDUCBAS")],data$years==0|data$years==2)


#Check mising hard drug status at baseline
missing_hard_drugs_count<-dim(subset(data_easy,data_easy$years==0&is.na(data_easy$hard_drugs)==TRUE))[1]
missing_hard_drugs_rate<-dim(subset(data_easy,data_easy$years==0&is.na(data_easy$hard_drugs)==TRUE))[1]/dim(subset(data_easy,data_easy$years==0))[1]



#Create baseline data with hard drug status
data_base<-subset(data_easy,data_easy$years==0&is.na(data_easy$hard_drugs)==FALSE)
colnames(data_base)<-c("newid",paste(colnames(data_base)[2:length(colnames(data_base))],"_0",sep=""))
data_base$group[data_base$hard_drugs_0==1]=1
data_base$group[data_base$hard_drugs_0==0]=0
data_base$group<-as.factor(data_base$group)
levels(data_base$group)<-c("No hard drug use at baseline","Hard drug use at baseline")
table(data_base$group)

#factor years to compare as two groups
data_base$years_0<-as.factor(data_base$years_0)
label(data_base$group)="hard drug at baseline"

#Create year two data
data_three<-merge(data_base[,c("newid","group")],subset(data_easy,data_easy$years==2),by="newid")
colnames(data_three)<-c("newid","group",paste(colnames(data_three)[3:length(colnames(data_three))],"_2",sep=""))
table(data_three$group)

#factor years to compare as two groups
data_three$years_2<-as.factor(data_three$years_2)
label(data_three$hard_drugs)="Hard drug use since last visit"


#Make wide table for Baseline and Two years after
data_wide<-merge(data_base[,c("newid","AGG_MENT_0","AGG_PHYS_0","LEU3N_0","VLOAD_0","years_0","age_0","BMI_0","RACE_0","SMOKE_0","EDUCBAS_0","group")],data_three[,c("newid","AGG_MENT_2","AGG_PHYS_2","LEU3N_2","VLOAD_2","years_2","ADH_2")],by="newid")
label(data_wide$group)="hard drug at baseline"
