##########################Distribution plots##############################
#                                                                        #
# Author: Wenru, 10/28/2018                                              #                                                                        #
#                                                                        #
##########################################################################
par(mfrow=c(1,2))

seed=1
x0<-seq(from=-5,to=40,by=0.001)
p_l1<-dlnorm(x0, meanlog = 0, sdlog = sqrt(0.834115194))
x<-x0-exp(0.834115194/2)
plot(p_l1~x, xlab = "Random intercept, skewed",ylab = "Density of lognormal",pch='.')


seed=1
p_l2<-dlnorm(x0, meanlog = 0, sdlog = sqrt(0.693147181))
x<-x0-exp(0.693147181/2)
plot(p_l2~x, xlab = "Random slope, skewd",ylab = "Density of lognormal",pch='.')


seed=1
x2<-seq(from=-25, to=25, by=0.001)
components <- sample(1:2,prob=c(0.33,0.67),size=50000,replace=TRUE)
mus <- c(0,0)
sds <- sqrt(c(7,5))
samples <- dnorm(x2,mean=mus[components],sd=sds[components])
plot(samples~x2, xlab = "Random intercept, mixture",ylab = "Density of Mixture",pch='.')


seed=1
components <- sample(1:2,prob=c(0.33,0.67),size=50000,replace=TRUE)
mus <- c(0,0)
sds2 <- sqrt(c(2,4))
samples2 <- dnorm(x2,mean=mus[components],sd=sds2[components])
plot(samples2~x2, xlab = "Random slope, mixture",ylab = "Density of Mixture",pch='.')

