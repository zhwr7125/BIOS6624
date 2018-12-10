##########################simulation results##############################
#                                                                        #
# Author: Yaxu, Lingdi, Wenru, 10/28/2018                                #                                                                        #
# pleases read the step0~step5 in section1 and 2                         #
#                                                                        #
##########################################################################

##section 0----Do not change#################
#set replicates
rep<-500

#Set storing vectors
storcoskew1 <- rep(5,rep)
storcoskew2 <- rep(5,rep)
storcoskew3 <- rep(5,rep)
storcoskew4 <- rep(5,rep)

storcomix1 <- rep(5,rep)
storcomix2 <- rep(5,rep)
storcomix3 <- rep(5,rep)
storcomix4 <- rep(5,rep)

storpvskew1 <- rep(5,rep)
storpvskew2 <- rep(5,rep)
storpvskew3 <- rep(5,rep)
storpvskew4 <- rep(5,rep)

storpvmix1 <- rep(5,rep)
storpvmix2 <- rep(5,rep)
storpvmix3 <- rep(5,rep)
storpvmix4 <- rep(5,rep)





##section1----Set param######################

#Step0: download the newest data_function.R from github, and change the address below
source("C:/repository/bios6624-zhwr7125/Project2/Code/data_function.r")

#Step1. sample size, Yaxu1000, Wenru200, Lingdi20
n.set=200

#Step2. time replicate, 5 or 10
p.set=5
#p.set=10

#Step3. random int or slp, you must also replace by hand in lme later in step 5
#random int:
var_b1.set=0
var_b1.1.set = 0
var_b1.2.set = 0

#random slp:
#var_b1.set=0.693147181
#var_b1.1.set = 2
#var_b1.2.set = 4

#Step4. Null or alt
#Null
#beta.set<-c(0, 0, 0, 0)
#Alt
beta.set<-c(2, 1, 0.5, 0.25)


  
#section2----Start simulation, step 5 is here#############################
#set seed
seed<-seq(from=1,to=rep, by=1)
for (i in 1:rep) {
ctrl <- lmeControl(opt='optim')
#Step5: change random=~1|id for random intercept
#       change random=list(id = pdDiag(~ time)) for random intercept and slope
  rand.fitskew<- tryCatch(lme(y_S ~ grp*time,random=~1|id,control=ctrl,data_gene(n1=n.set/2,n2=n.set/2,p=p.set,beta=beta.set,var_b0 = 0.834115194,var_b1 = var_b1.set,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = var_b1.1.set, var_b1.2 = var_b1.2.set)), error=function(e) NULL )
  rand.fitmix <- tryCatch(lme(y_M ~ grp*time,random=~1|id,control=ctrl,data_gene(n1=n.set/2,n2=n.set/2,p=p.set,beta=beta.set,var_b0 = 0.834115194,var_b1 = var_b1.set,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = var_b1.1.set, var_b1.2 = var_b1.2.set)), error=function(e) NULL )
  
  storcoskew1[i] <- tryCatch(fixef(rand.fitskew)[1], error=function(e) NA)
  storcoskew2[i] <- tryCatch(fixef(rand.fitskew)[2], error=function(e) NA)
  storcoskew3[i] <- tryCatch(fixef(rand.fitskew)[3], error=function(e) NA)
  storcoskew4[i] <- tryCatch(fixef(rand.fitskew)[4], error=function(e) NA)
  
  storcomix1[i] <- tryCatch(fixef(rand.fitmix)[1], error=function(e) NA)
  storcomix2[i] <- tryCatch(fixef(rand.fitmix)[2], error=function(e) NA)
  storcomix3[i] <- tryCatch(fixef(rand.fitmix)[3], error=function(e) NA)
  storcomix4[i] <- tryCatch(fixef(rand.fitmix)[4], error=function(e) NA)
  
  storpvskew1[i] <- tryCatch(Anova(rand.fitskew,type=3)$'Pr(>Chisq)'[1], error=function(e) NA)
  storpvskew2[i] <- tryCatch(Anova(rand.fitskew,type=3)$'Pr(>Chisq)'[2], error=function(e) NA)
  storpvskew3[i] <- tryCatch(Anova(rand.fitskew,type=3)$'Pr(>Chisq)'[3], error=function(e) NA)
  storpvskew4[i] <- tryCatch(Anova(rand.fitskew,type=3)$'Pr(>Chisq)'[4], error=function(e) NA)
  
  storpvmix1[i] <- tryCatch(Anova(rand.fitmix,type=3)$'Pr(>Chisq)'[1], error=function(e) NA)
  storpvmix2[i] <- tryCatch(Anova(rand.fitmix,type=3)$'Pr(>Chisq)'[2], error=function(e) NA)
  storpvmix3[i] <- tryCatch(Anova(rand.fitmix,type=3)$'Pr(>Chisq)'[3], error=function(e) NA)
  storpvmix4[i] <- tryCatch(Anova(rand.fitmix,type=3)$'Pr(>Chisq)'[4], error=function(e) NA)
}

storcoskewcom <- cbind(storcoskew1,storcoskew2,storcoskew3,storcoskew4)
storcomixcom <- cbind(storcomix1,storcomix2,storcomix3,storcomix4)

storpvskewcom <- cbind(storpvskew1,storpvskew2,storpvskew3,storpvskew4)
storpvmixcom <- cbind(storpvmix1,storpvmix2,storpvmix3,storpvmix4)

colnames(storcoskewcom) <- c("Beta0/c_S","Beta1/c_S","Beta2/c_S","Beta3/c_S")
colnames(storcomixcom) <- c("Beta0/c_M","Beta1/c_M","Beta2/c_M","Beta3/c_M")

colnames(storpvskewcom) <- c("Beta0/p_S","Beta1/p_S","Beta2/p_S","Beta3/p_S")
colnames(storpvmixcom) <- c("Beta0/p_M","Beta1/p_M","Beta2/p_M","Beta3/p_M")

results_c <- cbind(storcoskewcom,storcomixcom)
results_p <- cbind(storpvskewcom,storpvmixcom)

##section3----calculate mean, bias, sd, se, type I, II error################
#coef part
results_c<-na.omit(results_c)
results_c_mean<-apply(results_c,2,mean,na.rm=T)
results_c_sd<-apply(results_c,2,sd,na.rm=T)
results_c_se<-results_c_sd/sqrt(nrow(results_c))
results_c_bias<-results_c_mean-c(beta.set,beta.set)


#pval part
#for null and alt
results_p<-na.omit(results_p)
results_p_cal<-apply(ifelse(results_p<=0.05,1,0),2,mean,na.rm=T)

final_result<-data.frame(results_c_mean,results_c_sd,results_c_se,results_c_bias,results_p_cal)
names <- rownames(final_result)
final_result<-cbind(names,final_result,nrow(results_p))
##section4----save the output####################

library(readr)
write_csv(final_result, "C:/repository/bios6624-zhwr7125/Project2/Data/n200-p5-alt-int-final.csv")
