#question 1

raw_data<-read.csv('D:/Retro/NTHU/課程講義/大三/計算統計於商業分析之應用/HW5/verizon.csv',header = T)
data_group <- raw_data$Group
data_time <- raw_data$Time
# a. visualization
time_ILEC <- subset(raw_data,Group == "ILEC")$Time
time_CLEC <- subset(raw_data,Group == "CLEC")$Time

plot(density(time_ILEC),col='blue',xlab='time',main = 'ILEC v.s CLEC')
lines(density(time_CLEC),col='red')
legend('topright',inset=.05,title = 'Group',c('ILEC','CLEC'),lty = c(1,1),col=c("blue","red"))

# b. t.test
# h0: diff of the two mean = 0    h1: != 0
t.test(time_ILEC,time_CLEC,var.equal = FALSE,conf.level = 0.99)

#  by the result of t.test , p-value = 0.05975 > 0.01 ,and also the 99% c.i contain 0
#  we can say that we do not reject h0

# c. bootstrap mean 
sample_bootstrap <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  return(resample)
}

boot_ILEC<-replicate(2000,sample_bootstrap(time_ILEC))
boot_CLEC<-replicate(2000,sample_bootstrap(time_CLEC))

#bootstrapped samples of ILEC against bootstrapped samples of CLEC (alt t-values)
#bootstrapped samples of ILEC against the original ILEC sample (null t-values)

bootstrap_null_alt <- function(sample0,hyp_mean){
  resample<-sample(sample0,length(sample0),replace = TRUE)
  resample_se<-sd(resample)/sqrt(length(resample))
  
  t_alt <- (mean(resample)-hyp_mean)/resample_se
  t_null <- (mean(resample)-mean(sample0))/resample_se
  
  return(c(t_alt,t_null))
}

set.seed(42)
boot_t_stat <- replicate(2000,bootstrap_null_alt(time_ILEC,7.6))
boot_t_alt <- boot_t_stat[1,]
boot_t_null <- boot_t_stat[2,]

plot(density(boot_t_alt),main='t_alt v.s t_null',xlim = c(-40,5),ylim=c(0,0.5))
abline(v=mean(boot_t_alt),lty="dashed")
lines(density(boot_t_null),col = 'red')
abline(v=mean(boot_t_null),lty='dashed')
abline(v=quantile(boot_t_null,c(0.025,0.975)))

t.test(boot_t_null,boot_t_alt,var.equal = FALSE)

# Question 2  test var
# a. 
var(time_ILEC)
var(time_CLEC)
# h0 : var1 = var2

#b
f_value = var(time_CLEC)/var(time_ILEC)
f_value
qf(p=0.95,df1=length(time_CLEC)-1,df2=length(time_ILEC)-1)
# f_value > critical value => reject h0
var.test(time_CLEC,time_ILEC,alternative = 'greater')
#c bootstrap
set.seed(43)
sd_test<-function(larger_sd,smaller_sd){
  resample_larger_sd <- sample(larger_sd,length(larger_sd),replace=TRUE)
  resample_smaller_sd <- sample(smaller_sd,length(smaller_sd),replace=TRUE)
  f_alt<-var(resample_larger_sd)/var(resample_smaller_sd)
  f_null<-var(resample_larger_sd)/var(larger_sd)
  return(c(f_alt,f_null))
}

f_stats <- replicate(10000,sd_test(time_CLEC,time_ILEC))
f_alt<-f_stats[1,]
f_null<-f_stats[2,]

quantile(f_null,0.95)
plot(density(f_alt),col='red',main = 'f_alt v.s f_null')
lines(density(f_null),col='blue')
abline(v=quantile(f_null,0.95))

plot(density(time_ILEC))

#Question 3
#a.
norm_qq_plot <- function(values){
  probs1000 <- seq(0, 1, 0.001)
  q_vals <- quantile(values,prob=probs1000)
  q_norm <- qnorm(probs1000,mean(values),sd(values))
  plot(q_norm, q_vals, xlab="normal quantiles", ylab="values quantiles")
  abline(a=0,b=1, col="red", lwd=2)
}
set.seed(978234)
d1 <- rnorm(n=500, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=100, mean=45, sd=5)
d123 <- c(d1, d2, d3)

plot(density(d123))
norm_qq_plot(d123)
# b 分布接近一條直線，所以為常態分佈

# c
plot(density(boot_t_null))
norm_qq_plot(boot_t_null)

norm_qq_plot(time_CLEC)
norm_qq_plot(time_ILEC)
#84