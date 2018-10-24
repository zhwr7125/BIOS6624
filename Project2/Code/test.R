n1=10
n2=10
p = 5
beta = c(0, 0, 0, 0)
var_b0 = 3
var_b1 = 0
var_b0.1 = 7
var_b0.2 = 5
var_b1.1 = 0
var_b1.2 = 0

n <- n1 + n2 # total sample size

# design matrix for both skewed and mixture distribution
grp <- factor( rep(1:0, times = c(n1*p,n2*p)) ) # identify treatment assignment
time <- rep(0:(p-1), times = n) # time variable
X <- model.matrix(~ grp*time) # reference cell coding
id <- rep(1:n, each = p) # id indexing clusters

# generate correlated random intercept
# Skewed: 
#b0i+mean~logN(0,3)        b1+1~logN(0,0)
#b0i+mean~logN(0,3)        b1+1~logN(0,2)
Sig0.S <- gmat(sig2 = c(sqrt(var_b0),sqrt(var_b1)), rho = 0, p = 2)
d.S <- mvrnorm(n, mu = c(0,0), Sigma = Sig0.S)

b0.S <- exp(d.S[,1])-exp(var_b0/2)
b1.S <- exp(d.S[,2])-exp(var_b1/2)

d0.S <-rep(b0.S,each=p)
d1.S <-rep(b1.S,each=p) 

# Mixture:
#b0i=1/3N(0,7)+2/3N(0,5)    b1=1/3N(0,0)+2/3N(0,0)
#b0i=1/3N(0,7)+2/3N(0,5)    b1=1/3N(0,2)+2/3N(0,4)
Sig0.M1 <- gmat(sig2 = c(sqrt(var_b0.1),sqrt(var_b1.1)), rho = 0, p = 2)#The first part of distribution
Sig0.M2 <- gmat(sig2 = c(sqrt(var_b0.2),sqrt(var_b1.2)), rho = 0, p = 2)#The second part of distribution
d.M1 <- mvrnorm(n, mu = c(0,0), Sigma = Sig0.M1)#The first part of distribution
d.M2 <- mvrnorm(n, mu = c(0,0), Sigma = Sig0.M2)#The second part of distribution

b0.M1 <- d.M1[,1]#intercept, PART1
b0.M2 <- d.M2[,1]#intercept, PART2

b1.M1 <- d.M1[,2]#slope, PART1
b1.M2 <- d.M2[,2]#slope, PART2

d0.M1 <-rep(b0.M1,each=p)#intercept, PART1
d0.M2 <-rep(b0.M2,each=p)#intercept, PART2
d1.M1 <-rep(b1.M1,each=p)#slope, PART1
d1.M2 <-rep(b1.M2,each=p)#slope, PART2

uni_id<-rep(runif(20,0,1),each=p )


#error
e<-rnorm(n*p,0,sqrt(3))

#create outcome
y_S <- c( X %*% beta + d0.S + d1.S*time + e ) # linear predictor FOR SKEW
y_M1 <- c( X %*% beta + d0.M1 + d1.M1*time + e )
y_M2 <- c( X %*% beta + d0.M2 + d1.M2*time + e )

y_fix <- c( X %*% beta )


sample<-data.frame(id,uni_id,y_S,y_M1, y_M2, y_fix,grp,time,d0.S,d1.S,d0.M1,d0.M2,d1.M1,d1.M2,e)
sample$indicat[sample$uni_id<=0.3] <- 0
sample$indicat[sample$uni_id>0.3] <- 1

sample$y_M<-rep(-999,n*p)
for (i in 1:(n*p)){
if (sample$indicat[i]==0){sample$y_M[i]<-sample$y_M1[i]}
else {sample$y_M[i]<-sample$y_M2[i]}
}
return(sample)


toto=matrix(0,5,5)
toto
tata=c(1,2,3,4,5)
toto[,1]=tata