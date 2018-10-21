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
  
# design matrix
grp <- factor( rep(1:0, times = c(n1*p,n2*p)) ) # identify treatment assignment
time <- rep(0:(p-1), times = n) # time variable
X <- model.matrix(~ grp*time) # reference cell coding
id <- rep(1:n, each = p) # id indexing clusters

# generate correlated random intercept
# Skewed: b0i+1~logN(0,3) b1+1~logN(0,2) or b1+1~logN(0,0)
Sig0 <- gmat(sig2 = c(sqrt(var_b0),sqrt(var_b1)), rho = 0, p = 2)
d <- mvrnorm(n, mu = c(0,0), Sigma = Sig0)
b0 <- exp(d[,1])-exp(sqrt(var_b0)/2)
b1 <- exp(d[,2])-exp(sqrt(var_b1)/2)
d0 <-rep(b0,each=p)
d1 <-rep(b1,each=p)

#error
e<-rnorm(n*p,0,sqrt(3))

#create outcome
y <- c( X %*% beta + d0 + d1*time + e ) # linear predictor
#y <- c( X %*% beta) # linear predictor for check


sample<-data.frame(id,y,grp,time)

}

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

type_n<-c(0,2)
  for (i in 1:length(size)){
    for (j in 1:length(time)){
        for (l in 1:length(type_n)){
          assign(paste(haha[i,j,1,l], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(0, 0, 0, 0),var_b0=3,var_b1=type_n[l]))
          assign(paste(haha[i,j,2,l], sep=""), data_gene(n1=size[i]/2,n2=size[i]/2,p=time[j],beta=c(2, 1, 0.5, 0.25),var_b0=3,var_b1=type_n[l]))
          
        }
    }
  }

rand.fit <- lmer(y ~ grp*time + (1 + time|id),data=Skew20_t5_alter_slp) # using lme4 package
summary(rand.fit)