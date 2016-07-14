library(survival)

####generate survival data!!!#####
expit <- function(x){
  return(exp(x)/(1+exp(x)))
}
beta0 = -0.25
beta1 = 0.3
beta2 = 0.2
theta0 = -3.2
theta1 = 0.1
theta2 = 0.1
theta3 = 0.1
theta4 = 0.1
n=1000

set.seed(100)
#generate binary exposure and confounder
A = rbinom(n,1,0.4)
C = rnorm(n,mean=1,sd=sqrt(1))
#generate binary mediator from logistic regression
linpred = (beta0+beta1*A+beta2*C)
probm = expit(linpred)
M_bin = rbinom(n,1,probm)
M_cont = rnorm(n,linpred,1)

#linear combo for outcome regression
linpred = exp(-(theta0+theta1*A+theta2*M_bin+theta4*C))
Y_surv_noint = rexp(n,linpred)
cen<-quantile(Y_surv_noint,probs = 0.75)
#censor observations after censoring time
Ycen_noint<-pmin(Y_surv_noint,cen)
#generate event  indicator
cens<-as.numeric(Ycen_noint<cen)
delta<-as.numeric(Y_surv_noint>cen)
linpred = theta0+theta1*A+theta2*M_bin+theta4*C
Y_cont_noint = rnorm(n,linpred,1)
Y_bin_noint = rbinom(n,1,expit(linpred))
Y_count_noint = rpois(n,abs(linpred))
Mbin_noint_data = cbind(A,M_bin,Ycen_noint,Y_cont_noint,Y_bin_noint,Y_count_noint,cen,C,delta)
write.table(Mbin_noint_data,file="Mbin_noint_data.txt")


#linear combo for outcome regression
linpred = exp(-(theta0+theta1*A+theta2*M_cont+theta4*C))
Y_surv_noint = rexp(n,linpred)
#generate censoring time so that 30% of data is censored
cen<-quantile(Y_surv_noint,probs = 0.75)
#censor observations after censoring time
Ycen_noint<-pmin(Y_surv_noint,cen)
#generate event  indicator
cens<-as.numeric(Ycen_noint<cen)
delta<-as.numeric(Y_surv_noint>cen)
linpred = theta0+theta1*A+theta2*M_cont+theta4*C
Y_cont_noint = rnorm(n,linpred,1)
Y_bin_noint = rbinom(n,1,expit(linpred))
Y_count_noint = rpois(n,abs(linpred))
Mcont_noint_data = cbind(A,M_cont,Ycen_noint,Y_cont_noint,Y_bin_noint,Y_count_noint,cen,C,delta)
write.table(Mcont_noint_data,file="Mcont_noint_data.txt")






#linear combo for outcome regression
linpred = exp(-(theta0+theta1*A+theta2*M_bin+theta3*(A*M_bin)+theta4*C))
Y_surv_int = rexp(n,linpred)
#generate censoring time so that 30% of data is censored
cen<-quantile(Y_surv_int,probs = 0.75)
#censor observations after censoring time
Ycen_int<-pmin(Y_surv_int,cen)
#generate event  indicator
cens<-as.numeric(Ycen_int<cen)
delta<-as.numeric(Y_surv_int>cen)
linpred = theta0+theta1*A+theta2*M_bin+theta3*(A*M_bin)+theta4*C
Y_cont_int = rnorm(n,linpred,1)
Y_bin_int = rbinom(n,1,expit(linpred))
Y_count_int = rpois(n,abs(linpred))
Mbin_int_data = cbind(A,M_bin,Ycen_int,Y_cont_int,Y_bin_int,Y_count_int,cen,C,delta)
write.table(Mbin_int_data,file="Mbin_int_data.txt")



#linear combo for outcome regression
linpred = exp(-(theta0+theta1*A+theta2*M_cont+theta3*(A*M_cont)+theta4*C))
Y_surv_int = rexp(n,linpred)
#generate censoring time so that 30% of data is censored
cen<-quantile(Y_surv_int,probs = 0.75)
#censor observations after censoring time
Ycen_int<-pmin(Y_surv_int,cen)
#generate event  indicator
cens<-as.numeric(Ycen_int<cen)
delta<-as.numeric(Y_surv_int>cen)
linpred = theta0+theta1*A+theta2*M_cont+theta3*(A*M_cont)+theta4*C
Y_cont_int = rnorm(n,linpred,1)
Y_bin_int = rbinom(n,1,expit(linpred))
Y_count_int = rpois(n,abs(linpred))
Mcont_int_data = cbind(A,M_cont,Ycen_int,Y_cont_int,Y_bin_int,Y_count_int,cen,C,delta)
write.table(Mcont_int_data,file="Mcont_int_data.txt")


