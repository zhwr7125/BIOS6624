library(Hmisc)
library(knitr)
library(devtools)

setwd("C:/repository/bios6624-zhwr7125/Project1/Code")

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
table(data$EDUCBAS_ori,data$EDUCBAS)

#create a new subset with only year 0 and 2
data_easy<-subset(data[,c("newid","AGG_MENT","AGG_PHYS","LEU3N","VLOAD","ART","years","hard_drugs","age","BMI","ADH","RACE","SMOKE","EDUCBAS")],data$years==0|data$years==2)


#Create baseline data with hard drug status
data_base<-subset(data_easy,data_easy$years==0&is.na(data_easy$hard_drugs)==FALSE)
colnames(data_base)<-c("newid",paste(colnames(data_base)[2:length(colnames(data_base))],"_0",sep=""))
data_base$group[data_base$hard_drugs_0==1]=1
data_base$group[data_base$hard_drugs_0==0]=0
data_base$group<-as.factor(data_base$group)


#Create year two data
data_three<-merge(data_base[,c("newid","group")],subset(data_easy,data_easy$years==2),by="newid")
colnames(data_three)<-c("newid","group",paste(colnames(data_three)[3:length(colnames(data_three))],"_2",sep=""))
table(data_three$group)


#Make wide table for Baseline and Two years after
data_wide<-merge(data_base[,c("newid","AGG_MENT_0","AGG_PHYS_0","LEU3N_0","VLOAD_0","years_0","age_0","BMI_0","RACE_0","SMOKE_0","EDUCBAS_0","group")],data_three[,c("newid","AGG_MENT_2","AGG_PHYS_2","LEU3N_2","VLOAD_2","years_2","ADH_2")],by="newid")
label(data_wide$group)="hard drug at baseline"



#Combine unknowns as missing
#BMI missing and strange value
#strange, Improbable value, missing
#see values
data_wide$BMI_0[data_wide$BMI_0>500]
#see summary
data_wide$BMI_0[data_wide$BMI_0>500|data_wide$BMI_0==-1]<-NA
#min(data_wide$BMI_0,na.rm=TRUE)
#max(data_wide$BMI_0,na.rm=TRUE)



#Those who have BMI missing, insufficient and improbable should not be included in this model
data_final<-subset(data_wide,is.na(data_wide$BMI)==FALSE&is.na(data_wide$EDUCBAS_0)==FALSE)


write_csv(data_final, "C:/repository/bios6624-zhwr7125/Project1/Data/hivclean.csv")

