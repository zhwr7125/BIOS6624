#coef part
results_c_mean<-apply(results_c,2,mean)
results_c_sd<-apply(results_c,2,sd)
results_c_se<-results_c_sd/sqrt(rep)
results_c_bias<-results_c_mean-c(c(0, 0, 0, 0),c(0, 0, 0, 0))


#pval part
#for null and alt
results_p_cal<-apply(ifelse(results_p<=0.05,1,0),2,mean)

final_result<-data.frame(results_c_mean,results_c_sd,results_c_se,results_c_bias,results_p_cal)