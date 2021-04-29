# Question 1
#(a)
norm <- rnorm(500,mean=940,sd = 190)
#create a normal distribution
rnorm_std <- (norm - mean(norm))/sd(norm)
#standardization

#(i)
mean(rnorm_std)
sd(rnorm_std)
#(ii)
plot(density(rnorm_std))


# Question 2

# Visualize the confidence intervals of samples drawn from a population
#   e.g.,
#     visualize_sample_ci(sample_size=300, distr_func=rnorm, mean=50, sd=10)
#     visualize_sample_ci(sample_size=300, distr_func=runif, min=17, max=35)
visualize_sample_ci <- function(num_samples = 100, sample_size = 100,pop_size=10000, distr_func= rnorm, ...) {
  # Simulate a large population

  population_data <- distr_func(pop_size, ...)
  pop_mean <- mean(population_data)
  pop_sd <- sd(population_data)
  
  # Simulate samples
  samples <- replicate(num_samples, 
                       sample(population_data, sample_size, replace=FALSE))
  
  # Calculate descriptives of samples
  sample_means = apply(samples, 2, FUN=mean)
  sample_stdevs = apply(samples, 2, FUN=sd)
  sample_stderrs <- sample_stdevs/sqrt(sample_size)
  ci95_low  <- sample_means - sample_stderrs*1.96
  ci95_high <- sample_means + sample_stderrs*1.96 
  ci99_low  <- sample_means - sample_stderrs*2.58
  ci99_high <- sample_means + sample_stderrs*2.58
  
  # Visualize confidence intervals of all samples
  plot(NULL, xlim=c(pop_mean-(pop_sd/2), pop_mean+(pop_sd/2)), 
       ylim=c(1,num_samples), ylab="Samples", xlab="Confidence Intervals")
  add_ci_segment(ci95_low, ci95_high, ci99_low, ci99_high,
                 sample_means, 1:num_samples, good=TRUE)
  
  # Visualize samples with CIs that don't include population mean
  bad = which(((ci95_low > pop_mean) | (ci95_high < pop_mean)) |
                ((ci99_low > pop_mean) | (ci99_high < pop_mean)))
  add_ci_segment(ci95_low[bad], ci95_high[bad], ci99_low[bad], ci99_high[bad],
                 sample_means[bad], bad, good=FALSE)
  notInclude_95 <- which((ci95_low > pop_mean) | (ci95_high < pop_mean))
  notInclude_99 <- which((ci99_low > pop_mean) | (ci99_high < pop_mean))
  cat("# of not including in 95% inteval:",length(notInclude_95)," # of not including in 99% inteval:",length(notInclude_99))
  # Draw true population mean
  abline(v=mean(population_data))
  return(c(notInclude_95,notInclude_99))
}

add_ci_segment <- function(ci95_low, ci95_high, ci99_low, ci99_high, 
                           sample_means, indices, good=TRUE) {
  segment_colors <- list(c("lightcoral", "coral3", "coral4"),
                         c("lightskyblue", "skyblue3", "skyblue4"))
  color <- segment_colors[[as.integer(good)+1]]
  
  segments(ci99_low, indices, ci99_high, indices, lwd=3, col=color[1])
  segments(ci95_low, indices, ci95_high, indices, lwd=3, col=color[2])
  points(sample_means, indices, pch=18, cex=0.6, col=color[3])
}

#(a)
visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000,distr_func = rnorm)

visualize_sample_ci(num_samples = 100, sample_size = 300, pop_size=10000,distr_func=rnorm, mean=20, sd=3)

num_3 <- visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000,distr_func = runif)
num_3[1] 
# the number of not including mean in 95% CI
num_3[2] 
# the number of not including mean in 95% CI

bookings <- read.table("D:/Retro/NTHU/課程講義/大三/計算統計於商業分析之應用/HW3/first_bookings_datetime_sample.txt", header=TRUE)
bookings$datetime[1:9]
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
plot(density(minday), main="Minute (of the day) of first ever booking", col="blue", lwd=2)

compute_sample_mean <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  mean(resample)
}

boostrap_mean<- replicate(2000,compute_sample_mean(minday))
plot(density(boostrap_mean))

mean_CI_95 <- c()
mean_CI_95[1]<- mean(boostrap_mean)-1.96*(sd(boostrap_mean)/sqrt(length(boostrap_mean)))
mean_CI_95[2]<- mean(boostrap_mean)+1.96*(sd(boostrap_mean)/sqrt(length(boostrap_mean)))                                     
mean_CI_95

# (b)
median(minday)

compute_sample_median <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  median(resample)
}
boostrap_median<- replicate(2000,compute_sample_median(minday))
plot(density(boostrap_median))

median_CI_95 <- c()
median_CI_95[1]<- mean(boostrap_median)-1.96*(sd(boostrap_median)/sqrt(length(boostrap_median)))
median_CI_95[2]<- mean(boostrap_median)+1.96*(sd(boostrap_median)/sqrt(length(boostrap_median)))                                     
median_CI_95
mean(median_CI_95)
minday_std <- (minday-mean(minday))/sd(minday) # standardization
plot(density(minday_std))
mean(minday_std)
sd(minday_std)
