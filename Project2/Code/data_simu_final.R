rep<-10
#should be 500

seed<-seq(from=1,to=rep, by=1)

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

##storpvskewcom <- as.data.frame(matrix(rep(0), nrow = 20, ncol = 4))

#N=200,P=5,RAND INT, NULL
for (i in 1:rep) {
  check<-data_gene(n1=200/2,n2=200/2,p=5,beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0)
  
  rand.fitskew<- lme(y_S ~ grp*time,random=~1|id,data_gene(n1=200/2,n2=200/2,p=5,beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0))#nul, int
  rand.fitmix <- lme(y_M ~ grp*time,random=~1|id,data_gene(n1=200/2,n2=200/2,p=5,beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0))#nul, int
  
  storcoskew1[i] <- fixef(rand.fitskew)[1]
  storcoskew2[i] <- fixef(rand.fitskew)[2]
  storcoskew3[i] <- fixef(rand.fitskew)[3]
  storcoskew4[i] <- fixef(rand.fitskew)[4]
  
  storcomix1[i] <- fixef(rand.fitmix)[1]
  storcomix2[i] <- fixef(rand.fitmix)[2]
  storcomix3[i] <- fixef(rand.fitmix)[3]
  storcomix4[i] <- fixef(rand.fitmix)[4]
  
  storpvskew1[i] <- Anova(rand.fitskew,type=3)$'Pr(>Chisq)'[1]
  storpvskew2[i] <- Anova(rand.fitskew,type=3)$'Pr(>Chisq)'[2]
  storpvskew3[i] <- Anova(rand.fitskew,type=3)$'Pr(>Chisq)'[3]
  storpvskew4[i] <- Anova(rand.fitskew,type=3)$'Pr(>Chisq)'[4]
  
  storpvmix1[i] <- Anova(rand.fitmix,type=3)$'Pr(>Chisq)'[1]
  storpvmix2[i] <- Anova(rand.fitmix,type=3)$'Pr(>Chisq)'[2]
  storpvmix3[i] <- Anova(rand.fitmix,type=3)$'Pr(>Chisq)'[3]
  storpvmix4[i] <- Anova(rand.fitmix,type=3)$'Pr(>Chisq)'[4]
  
  
  
  
  ##storpvskewcom[i] <- cbind(storpvskew1[i],storpvskew2[i],storpvskew3[i],storpvskew4[i])
  
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

write_csv(sample, "C:/repository/bios6624-zhwr7125/Project2/Data/sample.csv")
