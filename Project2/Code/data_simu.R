######################################################################
## Purpose: Generate longitudinal data determined                   ##
##          by generalized linear mixed effects models              ##
## By: Wenru Zhou  10/21/2018                                       ##
##                  updated: 10/23/2018: finished mixture and loop
##                  updated: 10/24/2018: log normal code from Yaxu  ##
######################################################################


#define the names of datasets

source("C:/repository/bios6624-zhwr7125/Project2/Code/data_function.r")

size<-c(20,200,1000)
time<-c(5,10)
coef<-c("null","alter")
type<-c("int","slp")
seed<-seq(from=1,to=2, by=1)#change to 500
haha<-array("haha",dim=c(length(size),length(time),length(coef),length(type),length(seed)))
coco<-array(c("coco"),dim=c(length(size),length(time),length(coef),length(type),length(seed)))
pvpv<-array(c("pvpv"),dim=c(length(size),length(time),length(coef),length(type),length(seed)))


for (i in 1:length(size)){
  for (j in 1:length(time)){
    for (k in 1:length(coef)){
      for (l in 1:length(type)){
        for (q in 1:length(seed)){
           haha[i,j,k,l,q]<-paste0("MS",size[i],"_t",time[j],"_",coef[k],"_",type[l],"_",seed[q])
           coco[i,j,k,l,q]<-paste0("CO",size[i],"_t",time[j],"_",coef[k],"_",type[l],"_",seed[q])
           pvpv[i,j,k,l,q]<-paste0("Pv",size[i],"_t",time[j],"_",coef[k],"_",type[l],"_",seed[q])
           
        }
      }
    }
  }
}


  for (i in 1:length(size)){
    for (j in 1:length(time)){  #n1,n2,p,beta,var_b0,var_b1,var_b0.1,var_b0.2,var_b1.1,var_b1.2
        for (q in 1:length(seed)){
          set.seed(seed[q])
          assign(paste(haha[i,j,1,1,q], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0))#nul, int

          assign(paste(haha[i,j,2,1,q], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0))#alt, int

          assign(paste(haha[i,j,1,2,q], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0.693147181,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 2, var_b1.2 = 4))#nul, slp

          assign(paste(haha[i,j,2,2,q], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0 = 0.834115194,var_b1 = 0.693147181,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 2, var_b1.2 = 4))#alt, slp
          
          
          #random intercept model, null
          rand.int <- lme(y_S ~ grp*time,random=~1|id,data=data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0)) # using lme4 package
          coco[i,j,1,1,q]<-fixef(rand.int)
          pvpv[i,j,1,1,q]<-Anova(rand.int,type=3)$'Pr(>Chisq)'
          
          }
      }
  }


fix.int<-c("intercept","group","time","group*time")
pva.int<-c("intercept","group","time","group*time")

fix.int<-data.frame(rbind(fix.int,fixed_effect.int))
pva.int<-data.frame(rbind(pva.int,p_value.int))

summary<-list(fix.int,pva.int)

return(summary)