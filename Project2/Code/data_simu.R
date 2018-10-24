######################################################################
## Purpose: Generate longitudinal data determined                   ##
##          by generalized linear mixed effects models              ##
## By: Wenru Zhou  10/21/2018                                       ##
##                  updated: 10/23/2018: finished mixture and loop
######################################################################


#define the names of datasets

source("C:/repository/bios6624-zhwr7125/Project2/Code/data_function.r")

size<-c(20,200,1000)
time<-c(5,10)
coef<-c("null","alter")
type<-c("int","slp")
seed<-seq(from=1,to=2, by=1)#change to 500
haha<-array("haha",dim=c(length(size),length(time),length(coef),length(type),length(seed)))
for (i in 1:length(size)){
  for (j in 1:length(time)){
    for (k in 1:length(coef)){
      for (l in 1:length(type)){
        for (q in 1:length(seed)){
           haha[i,j,k,l,q]<-paste0("MS",size[i],"_t",time[j],"_",coef[k],"_",type[l],"_",seed[q])
        }
      }
    }
  }
}


  for (i in 1:length(size)){
    for (j in 1:length(time)){  #n1,n2,p,beta,var_b0,var_b1,var_b0.1,var_b0.2,var_b1.1,var_b1.2
        for (q in 1:length(seed)){
          set.seed(seed[q])
          assign(paste(haha[i,j,1,1,q], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0 = 3,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0))#nul, int

          assign(paste(haha[i,j,2,1,q], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0 = 3,var_b1=0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0))#alt, int

          assign(paste(haha[i,j,1,2,q], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0 = 3,var_b1=0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 2, var_b1.2 = 4))#nul, slp

          assign(paste(haha[i,j,2,2,q], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0=3,var_b1=0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 2, var_b1.2 = 4))#alt, slp
          }
      }
  }

rand.fit <- lmer(y_S ~ grp*time + (1 + time|id),data=MS20_t5_alter_slp_1) # using lme4 package
#summary(rand.fit)
