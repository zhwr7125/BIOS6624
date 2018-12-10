#############################################
##MICE_example.R
##
#############################################

install.packages("haven")
install.packages("mice")
#install.packages("dplyr")
install.packages("table1")
library(table1)
library(haven) #to read in SAS data
library(mice) #imputation package
library(dplyr)

setwd("/Users/nichole/Dropbox/Teaching/Bios6624/Fall2018/Module3MissingData/MissingDataFiles/PROCMI_Files/whi_mam")

whidata<-read_sas("whimam.sas7bdat")

whidata <- mutate(whidata, mamnum = ifelse(MAM5Y==0,0,MAMNUM5Y))

whidata <- filter(whidata, ETHNIC != 8)

whidata <- select(whidata, c("mamadhere","ETHNIC","BMI","MENARCHE","BRCA_1HX", "BRSTFDMO", "CANC_F30","CVD","DIAB","EDUC_C","HRDEX18","HYPT","HYSTBOOPH","income_c","INSUR","LMSEPI","mamnum","octime_c","PARITY_C","SMOKING","TOTHCAT","alcday_c","MENO"))

##Create factors on the categorical variables
whidata$ETHNIC <- as.factor(whidata$ETHNIC)
whidata$BRCA_1HX <- as.factor(whidata$BRCA_1HX )
whidata$BRSTFDMO <- as.factor(whidata$BRSTFDMO)
whidata$CANC_F30 <- as.factor(whidata$CANC_F30)
whidata$CVD <- as.factor(whidata$CVD)
whidata$DIAB <- as.factor(whidata$DIAB)
whidata$EDUC_C <- as.factor(whidata$EDUC_C)
whidata$HRDEX18 <- as.factor(whidata$HRDEX18)
whidata$HYPT <- as.factor(whidata$HYPT)
whidata$HYSTBOOPH <- as.factor(whidata$HYSTBOOPH)
whidata$income_c <- as.factor(whidata$income_c)
whidata$INSUR <- as.factor(whidata$INSUR)
whidata$LMSEPI <- as.factor(whidata$LMSEPI)
whidata$octime_c <- as.factor(whidata$octime_c)
whidata$PARITY_C <- as.factor(whidata$PARITY_C)
whidata$SMOKING <- as.factor(whidata$SMOKING)
whidata$TOTHCAT <- as.factor(whidata$TOTHCAT)
whidata$alcday_c <- as.factor(whidata$alcday_c)

##COMPLETE CASE ANALYSIS OF ADHERENCE TO MAMMOGRAPHY
mammodel <- glm(mamadhere ~ ETHNIC+BMI+MENARCHE+BRCA_1HX+BRSTFDMO+CANC_F30+CVD+DIAB+EDUC_C+HRDEX18+HYPT+HYSTBOOPH+income_c+INSUR+LMSEPI+mamnum+octime_c+PARITY_C+SMOKING+TOTHCAT+alcday_c+MENO, binomial(link = "logit"),data=whidata)
summary(mammodel)

##Look at amount of missing data in a Table 1
table1(~EDUC_C+BMI+MENARCHE+BRCA_1HX+BRSTFDMO+CANC_F30+CVD+DIAB+EDUC_C+HRDEX18+HYPT+HYSTBOOPH+income_c+INSUR+LMSEPI+mamnum+octime_c+PARITY_C+SMOKING+TOTHCAT+alcday_c+MENO | mamadhere, data=whidata)

###To focus our example: only work on imputing BRCA_1HX, income_c, and meno
##Clean data by deleting missing in the other variables 
colnames(whidata)
missingind<- complete.cases(whidata[,c(1:4,6:13,15:22)])

##Complete cases, except for our three variables of interest
whi.complete<- filter(whidata,missingind=="TRUE")
table1(~EDUC_C+BMI+MENARCHE+BRCA_1HX+BRSTFDMO+CANC_F30+CVD+DIAB+EDUC_C+HRDEX18+HYPT+HYSTBOOPH+income_c+INSUR+LMSEPI+mamnum+octime_c+PARITY_C+SMOKING+TOTHCAT+alcday_c+MENO | mamadhere, data=whi.complete)

##impute the three variables of interest
imp<-mice(whi.complete, m = 1, maxit = 1)

complete(imp)  ## not very interesting on a dataset of this size.

impanalysis<-with(imp, glm(mamadhere ~ ETHNIC+BMI+MENARCHE+BRCA_1HX+BRSTFDMO+CANC_F30+CVD+DIAB+EDUC_C+HRDEX18+HYPT+HYSTBOOPH+income_c+INSUR+LMSEPI+mamnum+octime_c+PARITY_C+SMOKING+TOTHCAT+alcday_c+MENO, binomial(link = "logit")))

###now do with more imputations and iterations per imputation.
imp5<-mice(whi.complete)

plot(imp5)

imp5$method

##rerun the analysis on the 5 imputed datasets

imp5analysis<-with(imp5, glm(mamadhere ~ ETHNIC+BMI+MENARCHE+BRCA_1HX+BRSTFDMO+CANC_F30+CVD+DIAB+EDUC_C+HRDEX18+HYPT+HYSTBOOPH+income_c+INSUR+LMSEPI+mamnum+octime_c+PARITY_C+SMOKING+TOTHCAT+alcday_c+MENO, binomial(link = "logit")))

imp5analysis  ## print the 5 results

pool.analysis<-pool(imp5analysis)

summary(pool.analysis)
