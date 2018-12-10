######################################################################
## Purpose: Generate longitudinal data determined                   ##
##          by generalized linear mixed effects models              ##
## By: Wenru Zhou  10/21/2018                                       ##
##                  updated: 10/23/2018: finished mixture and loop
##                  updated: 10/24/2018: log normal code from Yaxu  ##
######################################################################


#define the names of datasets

source("C:/repository/bios6624-zhwr7125/Project2/Code/data_function.r")

#size<-c(20,200,1000)
size<-c(20)
time<-c(5,10)
coef<-c("null","alter")
type<-c("int","slp")
seed<-seq(from=1,to=2, by=1)#change to 500
haha<-array("haha",dim=c(length(size),length(time),length(coef),length(type),length(seed)))
coco<-array(c("coco"),dim=c(length(size),length(time),length(coef),length(type),length(seed)))
pvpv<-array(c("pvpv"),dim=c(length(size),length(time),length(coef),length(type),length(seed)))
fix<-array(c("fix"),dim=c(length(size),length(time),length(coef),length(type)))
pva<-array(c("pva"),dim=c(length(size),length(time),length(coef),length(type)))

for (i in 1:length(size)){
  for (j in 1:length(time)){
    for (k in 1:length(coef)){
      for (l in 1:length(type)){
        fix[i,j,k,l]<-paste0("fix",size[i],"_t",time[j],"_",coef[k],"_",type[l])
        pva[i,j,k,l]<-paste0("pva",size[i],"_t",time[j],"_",coef[k],"_",type[l])
        
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
          
          
          #random intercept model, nul
          rand.int.S <- lme(y_S ~ grp*time,random=~1|id,data=data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0)) # using lme4 package
          rand.int.M <- lme(y_M ~ grp*time,random=~1|id,data=data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0)) # using lme4 package
          d1<-data.frame(fixef(rand.int.S),fixef(rand.int.M))
          d2<-data.frame(Anova(rand.int.S,type=3)$'Pr(>Chisq)',Anova(rand.int.M,type=3)$'Pr(>Chisq)')

          #random intercept model, alt
          rand.int.S <- lme(y_S ~ grp*time,random=~1|id,data=data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0)) # using lme4 package
          rand.int.M <- lme(y_M ~ grp*time,random=~1|id,data=data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0 = 0.834115194,var_b1 = 0,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 0, var_b1.2 = 0)) # using lme4 package
          d3<-data.frame(fixef(rand.int.S),fixef(rand.int.M))
          d4<-data.frame(Anova(rand.int.S,type=3)$'Pr(>Chisq)',Anova(rand.int.M,type=3)$'Pr(>Chisq)')

          #random intercept and slope model, nul
          rand.slp.S <- lme(y_S ~ grp*time,random=list(id = pdDiag(~ time)),data=data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0.693147181,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 2, var_b1.2 = 4)) # using lme4 package
          rand.slp.M <- lme(y_M ~ grp*time,random=list(id = pdDiag(~ time)),data=data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0 = 0.834115194,var_b1 = 0.693147181,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 2, var_b1.2 = 4)) # using lme4 package
          d5<-data.frame(fixef(rand.slp.S),fixef(rand.slp.M))
          d6<-data.frame(Anova(rand.slp.S,type=3)$'Pr(>Chisq)',Anova(rand.slp.M,type=3)$'Pr(>Chisq)')

          #random intercept and slope model, alt
          rand.slp.S <- lme(y_S ~ grp*time,random=list(id = pdDiag(~ time)),data=data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0 = 0.834115194,var_b1 = 0.693147181,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 2, var_b1.2 = 4)) # using lme4 package
          rand.slp.M <- lme(y_M ~ grp*time,random=list(id = pdDiag(~ time)),data=data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0 = 0.834115194,var_b1 = 0.693147181,var_b0.1 = 7, var_b0.2 = 5, var_b1.1 = 2, var_b1.2 = 4)) # using lme4 package
          d7<-data.frame(fixef(rand.slp.S),fixef(rand.slp.M))
          d8<-data.frame(Anova(rand.slp.S,type=3)$'Pr(>Chisq)',Anova(rand.slp.M,type=3)$'Pr(>Chisq)')
          haha<-list(d1,d2,d3,d4,d5,d6,d7,d8)
          }
      }
  }


for (i in 1:length(size)){
  for (j in 1:length(time)){  #n1,n2,p,beta,var_b0,var_b1,var_b0.1,var_b0.2,var_b1.1,var_b1.2
    for (k in 1:length(coef)){
      for (l in 1:length(type)){  
        for (q in 1:length(seed)){
        if (q==1)
        {
          assign(fool,c("intercept","group","time","group*time"))
          assign(fool,c("intercept","group","time","group*time"))
          assign(fool,rbind(fool,cat("",paste(coco[i,j,k,l,q],sep=""))))
          assign(paste(pva[i,j,k,l],sep=""),rbind(cat("",paste(pva[i,j,k,l],sep="")),cat("",paste(coco[i,j,k,l,q],sep=""))))
          
        }
        else {
          assign(paste(fix[i,j,k,l],sep=""),rbind(cat("",paste(fix[i,j,k,l],sep="")),cat("",paste(coco[i,j,k,l,q],sep=""))))
          assign(paste(pva[i,j,k,l],sep=""),rbind(cat("",paste(pva[i,j,k,l],sep="")),cat("",paste(coco[i,j,k,l,q],sep=""))))
          #summary<-list(fix.int[i,j,1,1],pva.int[i,j,1,1])
          }
        }
      }
    }
  }
}