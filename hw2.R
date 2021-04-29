# question 1 
# (a)
d1 <- rnorm(n=500, mean=45, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=100, mean=15, sd=5)

# Let¡¦s combine them into a single dataset
d123 <- c(d1, d2, d3)

# Let¡¦s plot the density function of abc
plot(density(d123), col="blue", lwd=2, 
     main = "Distribution 2")

# Add vertical lines showing mean and median
abline(v=median(d123),lty = "dashed")
abline(v=mean(d123))

# (b) 
d4 <- rnorm(n=800)
plot(density(d4),col = "blue",lwd = 2,main = "Distribution 3")
abline(v = median(d4),lty = "dashed")
abline(v = mean(d4))
median(d4)
mean(d4)
# (c)

# question 2
#(a)
rdata <- rnorm(2000,mean = 0,sd = 1)
plot(density(rdata),col= "blue",lwd = 2,main = "Normal dstribution")
abline(v = mean(rdata),lty = "solid")

coeff <- c(-3,-2,-1,1,2,3)
for(i in 1:6)
  abline(v = mean(rdata)-coeff[i]*sd(rdata),lty = "dashed")

#(b)
qt <- quantile(rdata)
qt[2:4]
data_b <- c()
mean(rdata)
sd(rdata)
for(i in 2:4)
  data[i-1] <- qt[i]
  data[i-1] <- (data[i-1] - mean(rdata))/sd(rdata)
data

#(c)
random_dataset <- rnorm(2000,mean = 35 , sd = 3.5)
qt_c <- quantile(random_dataset)
qt_c[2:4]
data_c <- c()
data_c[1] <- qt_c[2];data_c[2] <- qt_c[4]
data_c
for(i in 1:2)
{  data_c[i] <- (data_c[i]-mean(random_dataset))/sd(random_dataset)
  print(i);print(data_c[i])
}
  data_c

#(d)
qt_d <- quantile(d123)
data_d <- c()
data_d[1] <- qt_d[2];data_d[2] <- qt_d[4]
for(i in 1:2)
  data_d[i] <- (data[i]-mean(d123))/sd(d123)
data_d

# question 3
#(a)
# h = 2 * IQR * n^-1/3
#It replaces 3.5£m of Scott's rule with 2 IQR, which is less sensitive than the standard deviation to outliers in data.

#(b)

rand_data <- rnorm(800,mean = 20,sd = 5)
#(i) k = ceiling(log2n)+1
sturges_k <- ceiling(log2(800))+1
h1 <- (max(rand_data)-min(rand_data))/sturges_k
h1

#(ii) k = Scott's normal  reference rule
h2 <- (3.49*sd(rand_data))/(800^(1/3))
h2

#(iii) Freedman-Diaconis' choice
h3 <- 2*IQR(rand_data)/(800^(1/3))
h3
b<-c(h1,h2,h3)
#(c)
out_data <- c(rand_data, runif(10,min = 40,max = 60))
#(i) k = ceiling(log2n)+1
sturges_k <- ceiling(log2(800))+1
h1 <- (max(out_data)-min(out_data))/sturges_k
h1

#(ii) k = Scott's normal  reference rule
h2 <- (3.49*sd(out_data))/(800^(1/3))
h2

#(iii) Freedman-Diaconis' choice
h3 <- 2*IQR(out_data)/(800^(1/3))
h3
cc <- c(h1,h2,h3)

b;cc