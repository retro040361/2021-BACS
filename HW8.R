# pre-processing
media_1 <- read.csv('D:/Retro/NTHU/課程講義/大三/計算統計於商業分析之應用/HW8/pls-media/pls-media1.csv',header = T)$INTEND.0
media_2 <- read.csv('D:/Retro/NTHU/課程講義/大三/計算統計於商業分析之應用/HW8/pls-media/pls-media2.csv',header = T)$INTEND.0
media_3 <- read.csv('D:/Retro/NTHU/課程講義/大三/計算統計於商業分析之應用/HW8/pls-media/pls-media3.csv',header = T)$INTEND.0
media_4 <- read.csv('D:/Retro/NTHU/課程講義/大三/計算統計於商業分析之應用/HW8/pls-media/pls-media4.csv',header = T)$INTEND.0
media <- c(media_1,media_2,media_3,media_4)

# distributed by 黃川
pls_media <- list(m1 = media_1,m2 = media_2,m3 = media_3,m4 = media_4)
boxplot(pls_media,horizontal = T,main = 'boxplot of INTEND.0')

Q1_mean_segment <- function(n = 1, dataset){
  mean <- mean(dataset)
  segments(x0 = mean, y0 = n-0.4, x1 = mean, y1 = n+0.4, col = rgb(1,0,0), lwd = 2, lty = "dotted")
}

Q1_mean_segment(1, media_1)
Q1_mean_segment(2, media_2)
Q1_mean_segment(3, media_3)
Q1_mean_segment(4, media_4)


# Question 1
# a.
mean_1 <- mean(media_1)
mean_2 <- mean(media_2)
mean_3 <- mean(media_3)
mean_4 <- mean(media_4)

# b. visualization
plot(density(media_1),col = 'red',ylim = c(0,0.4),'density function of each group')
lines(density(media_2),col = 'green')
lines(density(media_3),col = 'blue')
lines(density(media_4))
abline(v = mean(media_1),lty = 1)
abline(v = mean(media_2),lty = 2)
abline(v = mean(media_3),lty = 3)
abline(v = mean(media_4),lty = 4)
legend('topleft',inset=.05,c('mean_1','mean_2','mean_3','mean_4'),lty = c(1,2,3,4)
)
legend('topright',inset=.05,c('media_1','media_2','media_3','media_4'),lty = c(1,1,1,1),col=c("red","green","blue","black"))

# c. From the visualization alone, do you feel that media type makes a difference on intention to share?

# Question 2 tradition one way
# a. null and alt hypo
# null: the mean of four treatment are the same
# alt : not the same

# b.Produce the traditional F-statistic for our test 
all_mean<-mean(media)
sstr <- (length(media_1)*(mean(media_1)-all_mean)^2 
      + length(media_2)*(mean(media_2)-all_mean)^2
      + length(media_3)*(mean(media_3)-all_mean)^2
      + length(media_4)*(mean(media_4)-all_mean)^2)
sse  <- ((length(media_1)-1)*var(media_1)
      + (length(media_2)-1)*var(media_2)
      + (length(media_3)-1)*var(media_3)
      + (length(media_4)-1)*var(media_4))
mstr <- sstr/3
mse <- sse/(length(media)-4)
f_value <- mstr/mse
f_value
qf(p = 0.95, df1 = 3,df2 = length(media)-4) # 95% cut off
qf(p = 0.99, df1 = 3,df2 = length(media)-4) # 99% cut off

# d 
media1 <- data.frame(type = rep(1,length(media_1)),intend = media_1)
media2 <- data.frame(type = rep(2,length(media_2)),intend = media_2)
media3 <- data.frame(type = rep(3,length(media_3)),intend = media_3)
media4 <- data.frame(type = rep(4,length(media_4)),intend = media_4)
media_all <- rbind(media1,media2,media3,media4)
oneway.test(media_all$intend~factor(media_all$type),var.equal = T)
media_all
# Question 3 Bootstrap
boot_anova <- function(m1,m2,m3,m4,num){
  null_m1<-sample(m1-mean(m1),replace = T)
  null_m2<-sample(m2-mean(m2),replace = T)
  null_m3<-sample(m3-mean(m3),replace = T)
  null_m4<-sample(m4-mean(m4),replace = T)
  null_m <- c(null_m1,null_m2,null_m3,null_m4)
  
  alt_m1 <- sample(m1,replace = T)
  alt_m2 <- sample(m2,replace = T)
  alt_m3 <- sample(m3,replace = T)
  alt_m4 <- sample(m4,replace = T)
  alt_m <- c(alt_m1,alt_m2,alt_m3,alt_m4)
  
  return(c(oneway.test(null_m ~ num,var.equal = T)$statistic,
           oneway.test(alt_m ~ num,var.equal = T)$statistic))
}

type <- media_all$type

boot_f_value <- replicate(5000,boot_anova(media_1,media_2,media_3,media_4,type))
boot_f_null <- boot_f_value[1,]
boot_f_alt <- boot_f_value[2,]

quantile(boot_f_null,0.95)
quantile(boot_f_null,0.99)

# c visualization
plot(density(boot_f_null),col = 'red',main = 'bootstrap Null f')
abline(v = quantile(boot_f_null,0.95),lty = 1)
abline(v = quantile(boot_f_null,0.99),lty = 2)
abline(v = mean(boot_f_alt),lty = 3)
lines(density(boot_f_alt),col = 'blue')
legend('topright',inset=.05,c('99% cut-off','95% cut-off','mean','boot_null_f','boot_alt_f'),lty = c(1,2,3,1,1),col = c('black','black','black','red','blue'))
