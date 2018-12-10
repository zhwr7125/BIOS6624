###########################################################
## Purpose: Generate longitudinal data determined ##
##          by generalized linear mixed effects models   ##
## By: Wenru Zhou                                        ##
###########################################################

library(lme4)
library(MASS)

# we will need gmat() to generate covariance matrix for random effects
# we may also want to use un() from the MultiMod.R script
gmat <- function(sig2, rho, p = length(sig2)){
  
  if(length(sig2) == 1)
    sig2 <- rep(sig2, p)
  else if (length(sig2) != p)
    stop("length(sig2) != p")
  
  if(length(rho) != 1)
    stop("length(rho) != 1")
  
  
  R <- matrix(rho, p, p)
  diag(R) <- 1
  D <- diag(sqrt(sig2), p, p)
  V <- D %*% R %*% D
  return(V)
  
}

data_gene<-function(n1,n2,p,beta,var_b0,var_b1){
n <- n1 + n2 # total sample size
  
# design matrix for both skewed and mixture distribution
grp <- factor( rep(1:0, times = c(n1*p,n2*p)) ) # identify treatment assignment
time <- rep(0:(p-1), times = n) # time variable
X <- model.matrix(~ grp*time) # reference cell coding
id <- rep(1:n, each = p) # id indexing clusters

# generate correlated random intercept
# Skewed: 
#b0i+1~logN(0,3) b1+1~logN(0,0)
#b0i+1~logN(0,3) b1+1~logN(0,2)
Sig0 <- gmat(sig2 = c(sqrt(var_b0),sqrt(var_b1)), rho = 0, p = 2)
d <- mvrnorm(n, mu = c(0,0), Sigma = Sig0)
b0 <- exp(d[,1])-exp(var_b0/2)
b1 <- exp(d[,2])-exp(var_b1/2)
d0 <-rep(b0,each=p)
d1 <-rep(b1,each=p) 

#error
e<-rnorm(n*p,0,sqrt(3))

#create outcome
y <- c( X %*% beta + d0 + d1*time + e ) # linear predictor
y_fix <- c( X %*% beta )
#y <- c( X %*% beta) # linear predictor for check


sample<-data.frame(id,y,y_fix,grp,time,d0,d1,e)

}


#define variance of random effect;

haha<-array("haha",dim=c(3,2,2,2))
size<-c(20,200,1000)
time<-c(5,10)
coef<-c("null","alter")
type<-c("int","slp")
for (i in 1:length(size)){
  for (j in 1:length(time)){
    for (k in 1:length(coef)){
      for (l in 1:length(type)){
    haha[i,j,k,l]<-paste0("Skew",size[i],"_t",time[j],"_",coef[k],"_",type[l])
      }
    }
  }
}


  for (i in 1:length(size)){
    for (j in 1:length(time)){
          assign(paste(haha[i,j,1,1], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0=3,var_b1=0))
          assign(paste(haha[i,j,2,1], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0=3,var_b1=0))
          assign(paste(haha[i,j,1,2], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0=3,var_b1=2))
          assign(paste(haha[i,j,2,2], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0=3,var_b1=2))
    }
  }

rand.fit <- lmer(y ~ grp*time + (1 + time|id),data=Skew20_t5_alter_slp) # using lme4 package
summary(rand.fit)



##################Mixture part#############

guagua<-Skew20_t5_alter_slp[c(1,3,4,5,8)]
uni_id<-rep(runif(20,0,1),each=5)
guagua2<-data.frame(guagua,uni_id)
guagua2$indicat[guagua2$uni_id<=0.3] <- 0
guagua2$indicat[guagua2$uni_id>0.3] <- 1

#random intercept
Sig1 <- gmat(sig2 = c(3,0), rho = 0, p = 2)
Sig2 <- gmat(sig2 = c(6,0), rho = 0, p = 2)

d_1.3 <- mvrnorm(20, mu = c(0,0), Sigma = Sig1)
d_2.3 <- mvrnorm(20, mu = c(0,0), Sigma = Sig2)

b0 <- (1/3)*d_1.3[,1]+(2/3)*d_2.3[,1]
b1 <- (1/3)*d_1.3[,2]+(2/3)*d_2.3[,2]
d0 <-rep(b0,each=5)
d1 <-rep(b1,each=5)

