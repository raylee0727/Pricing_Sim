library(qrmtools)
library(xlsx)
library(tidyverse)
library(tea)
library(evd)
library(sde)
CPI <- read.xlsx(file="CPIAUCSL.xls",sheetIndex=1,startRow = 12,endRow = 1317
                 ,header = F,colIndex =1:2)


index <- read.xlsx(file="dataset/I_t_data_fillna(全美).xlsx",sheetIndex=1,startRow = 2,endRow = 41
                   ,header = F,colIndex =c(2,3))
colnames(index) <- c("time","loss")


index <- drop_na(index)

index1 <- index[,2]/1000000000 

fit.MLE <- fit_GEV_MLE(index1)
est <- fit.MLE$par                 ##############
fit.MLE$SE

logLik_GEV(est,index1)            ########

cov_sita_hat <- fit.MLE$Cov      #######

##confidence interval
con_shape <- c(est[1]-1.96*fit.MLE$SE[1],est[1]+1.96*fit.MLE$SE[1])
con_loc <- c(est[2]-1.96*fit.MLE$SE[2],est[2]+1.96*fit.MLE$SE[2])
con_scale <- c(est[3]-1.96*fit.MLE$SE[3],est[3]+1.96*fit.MLE$SE[3])


#library(mvtnorm)
#x <- rmvnorm(36, mean = est , sigma = cov_sita_hat)

#library(ismev)
#gev.fit(xdat = est,ydat = cov_sita_hat,mul = est[1],sigl = est[2],shl = est[3])


#fit_GEV_MLE(index1)

#SE <- sqrt(cov_sita_hat)

#x+1.96*SE
#fit_GEV_MLE(index1, init = c("shape0", "PWM", "quantile"),
#estimate.cov = TRUE)

######3
# library(tea)   #k0?G11-15
# hill_method <- hall(index1, B = 1000, epsilon = 0.955, kaux = 2 * sqrt(length(index1))) 
# #?]??bootstrap?ҥH???G?|?@????
# 
# k0 <- hill_method$k0
# tail_index <- hill_method$tail.index
# hill_est_shape <- 1/tail_index
# hill_shape_std <- sqrt(1/((tail_index^2)*k0))
# confidence_interval <- c(hill_est_shape-1.96*hill_shape_std,hill_est_shape+1.96*hill_shape_std)
# 
# library(evd)
# hill_est <- fgev(index1, shape = 0.6553)
# hill_loc <- hill_est$estimate[1]
# hill_scale <- hill_est$estimate[2]
# 
# 
# ##5.6.1
# 
# hist(index1,nclass = 40,main = "Standard MLE",xlab = "Loss",xlim = range(-0.5,4),ylim = c(0,10),probability = TRUE)
# x <- seq(-0.5,4,0.01)
# 
# lines(x,dGEV(x,shape = est[1], loc = est[2], scale = est[3]),col="red")
# #curve(dGEV(x, shape = hill_est_shape, loc = hill_loc, scale = hill_scale),add = TRUE,col="blue")
# 
# hist(index1,nclass = 40,main = "Hill Estimator",xlab = "Loss",xlim = range(-0.5,4),ylim = c(0,10),probability = TRUE)
# curve(dGEV(x, shape = hill_est_shape, loc = hill_loc, scale = hill_scale),add = TRUE,col="red")
# 
# #5.6.2
# plot(ecdf(index1),lty = 0,pch = 1,cex = 0.5,xlim = c(-0.5,6),col.01line = NULL,main="Standard MLE",xlab="Quantile",ylab = "Probability")
# x=rGEV(100000, shape = est[1], loc = est[2], scale = est[3])
# curve(pGEV(x, shape = est[1], loc = est[2], scale = est[3]),add = TRUE,col="red")
# 
# plot(ecdf(index1),lty = 0,pch = 1,cex = 0.5,xlim = c(-0.5,6),col.01line = NULL,main="Hill Estimator",xlab="Quantile",ylab = "Probability")
# x=rGEV(100000, shape = hill_est_shape, loc = hill_loc, scale = hill_scale)
# curve(pGEV(x, shape = hill_est_shape, loc = hill_loc, scale = hill_scale),add = TRUE,col="red")
# 
# #5.6.3  ?C?????G?????@??
# sort_index <- sort(index1)
# x=rGEV(length(sort_index), shape = est[1], loc = est[2], scale = est[3])
# sort_Hsita <- sort(x)
# plot(x=sort_index,y=sort_Hsita,xlim = c(0,4),ylim = c(0,4),xlab = "Empirical",ylab = "Theoretical",main = "Standard MLE")
# abline(0,1,col="red")
# 
# sort_index <- sort(index1)
# x=rGEV(length(sort_index), shape = hill_est_shape, loc = hill_loc, scale = hill_scale)
# sort_Hsita <- sort(x)
# plot(x=sort_index,y=sort_Hsita,xlim = c(0,4),ylim = c(0,4),xlab = "Empirical",ylab = "Theoretical",main = "Hill Estimator")
# abline(0,1,col="red")

for (i in c(1:50)){
  
  hill_method <- hall(index1, B=1000, epsilon = 0.955, kaux = 2 * sqrt(length(index1))) 
  k0 <- hill_method$k0
  tail_index <- hill_method$tail.index
  hill_est_shape <- 1/tail_index
  hill_shape_std <- sqrt(1/((tail_index^2)*k0))
  confidence_interval <- c(hill_est_shape-1.96*hill_shape_std,hill_est_shape+1.96*hill_shape_std)
  
  hill_est <- fgev(index1, shape = hill_est_shape)
  hill_loc <- hill_est$estimate[1]
  hill_scale <- hill_est$estimate[2]
  
  ## 5.6.1
  png(paste0("mle_hist_",i,".png"))
  hist(index1,nclass = 40,main = "Standard MLE",xlab = "Loss",xlim = range(-0.5,4),ylim = c(0,10),probability = TRUE)
  x <- seq(-0.5,4,0.01)
  lines(x,dGEV(x,shape = est[1], loc = est[2], scale = est[3]),col="red")
  dev.off()
  
  #curve(dGEV(x, shape = hill_est_shape, loc = hill_loc, scale = hill_scale),add = TRUE,col="blue")
  png(paste0("hill_hist_",i,".png"))
  hist(index1,nclass = 40,main = "Hill Estimator",xlab = "Loss",xlim = range(-0.5,4),ylim = c(0,10),probability = TRUE)
  curve(dGEV(x, shape = hill_est_shape, loc = hill_loc, scale = hill_scale),add = TRUE,col="red")
  dev.off()
  
  #5.6.2 
  png(paste0("mle_ecdf_",i,".png"))
  plot(ecdf(index1),lty = 0,pch = 1,cex = 0.5,xlim = c(-0.5,6),col.01line = NULL,main="Standard MLE",xlab="Quantile",ylab = "Probability")
  x=rGEV(100000, shape = est[1], loc = est[2], scale = est[3])
  curve(pGEV(x, shape = est[1], loc = est[2], scale = est[3]),add = TRUE,col="red")
  dev.off()
  
  png(paste0("hill_ecdf_",i,".png"))
  plot(ecdf(index1),lty = 0,pch = 1,cex = 0.5,xlim = c(0,6),col.01line = NULL,main="Hill Estimator",xlab="Quantile",ylab = "Probability")
  x=rGEV(100000, shape = hill_est_shape, loc = hill_loc, scale = hill_scale)
  curve(pGEV(x, shape = hill_est_shape, loc = hill_loc, scale = hill_scale),add = TRUE,col="red")
  dev.off()
  
  #5.6.3  ?C?????G?????@??
  png(paste0("mle_qq_",i,".png"))
  sort_index <- sort(index1)
  x=rGEV(length(sort_index), shape = est[1], loc = est[2], scale = est[3])
  sort_Hsita <- sort(x)
  plot(x=sort_index,y=sort_Hsita,xlim = c(0,4),ylim = c(0,4),xlab = "Empirical",ylab = "Theoretical",main = "Standard MLE")
  abline(0,1,col="red")
  dev.off()
  
  png(paste0("hill_qq_",i,".png"))
  sort_index <- sort(index1)
  x=rGEV(length(sort_index), shape = hill_est_shape, loc = hill_loc, scale = hill_scale)
  sort_Hsita <- sort(x)
  plot(x=sort_index,y=sort_Hsita,xlim = c(0,4),ylim = c(0,4),xlab = "Empirical",ylab = "Theoretical",main = "Hill Estimator")
  abline(0,1,col="red")
  dev.off()
  
}

####################
#5.7.1

rate <- read.xlsx(file="USD12MD156N.xls",sheetIndex=1,startRow = 326,endRow = 1109
                  ,header = F,colIndex =c(1,2))
colnames(rate) <- c("date","LIBOR")

rate$LIBOR <- rate$LIBOR/100
rate <- rate[which(rowSums(rate==0)==0),]

rate <- na.omit(rate)

sigma_r <- sd(rate$LIBOR)*sqrt(252)

######################################################
library(sde)

GBM()
R1 <- numeric(50000)
R2 <- numeric(50000)
R3 <- numeric(50000)

for (i in 1:50000) {
  x <- GBM(x=rate$LIBOR[1],N=756,T=3,sigma=sigma_r,r=0.0013)
  R1[i] <- x[253]
  R2[i] <- x[505]
  R3[i] <- x[757]
}
mean(R1)
mean(R2)
mean(R3)

R <- cbind(R1,R2,R3)

########################################
# library(qrmtools)
# #r <- c(0.02907,0.02989,0.03073)
# 
# p <- rGEV(50000,shape = 0.6553,loc = 0.1502,scale = 0.1593)
# tmp = p[p>7.5]
# tmp
# which(c(p>7.5)==T)
# 
# tau <- cbind(which(c(p>7.5)==T),tmp)
# colnames(tau) <- c("t","It=tau")
# 
# j=1
# 
# gem <- function(n,R){
#   aa <- exp(-n*R[n])*((R[n,j]*100+11.5)+(1-rGEV(3,shape = 0.6553,loc = 0.1502,scale = 0.1593)[n])*100)
#   return(aa)
# }
# 
# bb <- c()
# 
# for (n in 1:3) {
#   bb[n] <- gem(n,R)
# }
# bb
# 
# sum(bb)/3

########################


couponCashFlow = function(R,tau,n){
  if (tau > n){
    result = (R*100+libor_spread)
  } expected_lossse {
    result = 0
  }
  return(result)
}

payoutRatio = function(tau,T,I_tau,K){
  if(tau>T){
    result = 0
  } else if(tau<=T && (I_tau >= K && I_tau <= U)){
    result = (I_tau-K)/(U-K)
  } else {
    result = 1
  }
  return(result)
}

#principal Redemption
prcplRedemption = function(tau,T,I_tau,K,n,N){
  p = payoutRatio(tau,T,I_tau,K)
  if (n<N){
    if (tau > (n-1) && tau<=n){
      result = (1-p)*FV
    } expected_lossse {
      result = 0
    }
  } else if(n==N) {
    if (tau>(N-1) && tau<=N){
      result = (1-p)*FV
    }
    else if (tau>N){
      result = FV
    } expected_lossse{
      result = 0
    }
  }
  return (result)
}

# cal C_0




#tau = 2
U = 10
K = 9
T = N = 3
FV = 100
s = 10000
libor_spread = 0.11

#2020/2/21 rate
r=0.0013
#https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yiexpected_lossdYear&year=2020

R1 <- numeric(s)
R2 <- numeric(s)
R3 <- numeric(s)

for (i in 1:s) {
  x <- GBM(x=rate$LIBOR[1],N=756,T=3,sigma=sigma_r,r=0.0013)
  R1[i] <- x[253]
  R2[i] <- x[505]
  R3[i] <- x[757]
}
mean(R1)
mean(R2)
mean(R3)

R <- cbind(R1,R2,R3)

#r = c(0.02907,0.02989,0.03073)





tmp1 = c()
tmp2 = c()
# for (j in 1:s){
#   I_tau = rGEV(s,shape=hill_est_shape,loc=hill_loc,scale=hill_scale)
#   tau = which(I_tau>K)[1]
#   
#   for (i in 1:3){
#     B[i] = exp(-1*i*r)
#     tmp1[i] = B[i]*(couponCashFlow(R[j,i],tau,i) + prcplRedemption(tau,T,I_tau[tau],K,i,N))
#   }
#   tmp = sum(tmp1)
#   tmp2[j] = tmp
# }
# result = 1/s * sum(tmp2)


B <- c()  
result <- c()
for (a in 1:9) {
  tmp1 = c()
  tmp2 = c()
  for (j in 1:s){
    I_tau = rGEV(50000,shape=hill_est_shape,loc=hill_loc,scale=hill_scale)
    tau = which(I_tau>a)[1]
    
    for (i in 1:3){
      B[i] = exp(-1*i*r)
      tmp1[i] = B[i]*(couponCashFlow(R[j,i],tau,i) + prcplRedemption(tau,T,I_tau[tau],a,i,N))
    }
    tmp = sum(tmp1)
    tmp2[j] = tmp
  }
  result[a] = 1/s * sum(tmp2)
  
}

plot(result,type = "o",xlab="Attachment point",ylab="Price")

#################################

#exceedance probabilities

prob <- function(x){
  p1u <- pGEV(x,shape=hill_est_shape,loc=hill_loc,scale=hill_scale,lower.tail = T)
  result <- 1-(p1u)^c(1,2,3)
  return(result)
}

prob_attachment <- t(apply(matrix(c(5,5,5,
                                    9,9,9,
                                    10,10,10),nrow = 3,byrow = T), 1, prob))

colnames(prob_attachment) <- c("1 year","2 years","3 years")
rownames(prob_attachment) <- c("KB=5","KA=9","UAB=10")

#return period

period <- function(x){
  result <- 1/(1-pGEV(x,shape=hill_est_shape,loc=hill_loc,scale=hill_scale,lower.tail = T))
  return(result)
}

return_period <- apply(matrix(c(5,9,10)),2, period)
colnames(return_period) <- c("return period m(u)")
rownames(return_period) <- c("KB=5","KA=9","UAB=10")

######


U=9
attachment_point = 6
#attachment_point = 9
p_tau <- c()
tau <- c()
expected_loss <- c()
conditional_exp_loss <- c()

for(i in 1:10000){
  I_tau = rGEV(50000,shape=hill_est_shape,loc=hill_loc,scale=hill_scale)
  #I_tau = rGEV(50000,shape=est[1],loc=est[2],scale=est[3])   0.0239636
  tau[i] = which(I_tau>attachment_point)[1]
  p_tau[i] <- payoutRatio(tau = tau[i],T=3,I_tau = I_tau[tau[i]],K=attachment_point)
}
expected_loss <- mean(p_tau)

conditional_exp_loss <- expected_loss/(length(which(tau<=3))/10000)


expected_loss <- c()
conditional_exp_loss <- c()
for(j in 1:10){
  p_tau <- c()
  tau <- c()
  
  for(i in 1:10000){
    I_tau = rGEV(50000,shape=hill_est_shape,loc=hill_loc,scale=hill_scale)
    tau[i] = which(I_tau>j)[1]
    p_tau[i] <- payoutRatio(tau = tau[i],T=3,I_tau = I_tau[tau[i]],K=j)
  }
  expected_loss[j] <- mean(p_tau)
  
  conditional_exp_loss[j] <- expected_loss[j]/(length(which(tau<=3))/10000)
}
plot(expected_loss*100,type="l",xlab=c("Attachment Point($ billion)"),ylab=c("EL(%)"))
plot(conditional_exp_loss*100,type="l",xlab=c("Attachment Point($ billion)"),ylab=c("CEL(%)"))

#####
#return levexpected_loss
return_level <- c()

shape <- hill_est_shape
location <- hill_loc
scale <- hill_scale
for (i in 1:300) {
  return_level[i] <- location-(scale/shape)*(1-(-log(1-(1/i)))^(-shape))
}

plot(return_level,type="l",xlab=c("Return period (years)"),ylab=c("Return level ( $billion)"))

