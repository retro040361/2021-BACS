experiment <- read.csv('D:/Retro/NTHU/課程講義/大三/計算統計於商業分析之應用/HW9/study2Data.csv', header=TRUE)
BY_data <- with(experiment, data.frame(Subject, Axis='BY', Emotion_Condition, ACC=BY_ACC, SAD_ESRI))
RG_data <- with(experiment, data.frame(Subject, Axis='RG', Emotion_Condition, ACC=RG_ACC, SAD_ESRI))

plot(density(subset(BY_data,Emotion_Condition == "Sadness")$ACC))
lines(density(subset(BY_data,Emotion_Condition == "Neutral")$ACC))

sad_diff <- subset(BY_data,Emotion_Condition == "Sadness")$ACC - subset(RG_data,Emotion_Condition == "Sadness")$ACC
neu_diff <- subset(BY_data,Emotion_Condition == "Neutral")$ACC - subset(RG_data,Emotion_Condition == "Neutral")$ACC
lines(density(sad_diff),col = 'red')
plot(density(sad_diff),main = 'difference',col = 'red')
lines(density(neu_diff),col = 'blue')
abline(v = mean(sad_diff),lty = 1)
abline(v = mean(neu_diff),lty = 2)



data <- cbind(sad_diff,neu_diff)
boxplot(data)
Q1_mean_segment <- function(n = 1, dataset){
  mean <- mean(dataset)
  segments(x0 = n-0.4, y0 = mean, x1 = n+0.4, y1 = mean, col = rgb(1,0,0), lwd = 2, lty = "dotted")
}
Q1_mean_segment(1,sad_diff)
Q1_mean_segment(2,neu_diff)

t.test(sad_diff)
# p-value>0.05 -> do not reject null
t.test(neu_diff)
# p-value>0.05 -> do not reject null

# d.
# Do the above t-tests support a claim that there is an interaction between emotion 
# and color axis?  (i.e., does people’s accuracy of color perception along different
# color-axes depend on their emotion? Here, accuracy is an outcome variable, 
# while color-axis and emotion are independent factors)
# 根據結果mean都是0 可以推論情感並不會影響

#e
all_data <- rbind(BY_data, RG_data)
data_aov <- aov(formula = ACC ~ Axis + Emotion_Condition + Axis:Emotion_Condition, data=all_data)
summary(data_aov)


with(all_data, 
     interaction.plot(
       x.factor=Emotion_Condition, 
       trace.factor=Axis, 
       response=ACC, 
       lwd=2
     )
)
