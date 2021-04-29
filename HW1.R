#1 
raw_data = read.table("D:/辰浩/NTHU/課程講義/計算統計於商業分析之應用/HW1/customers.txt",header = T)
data = raw_data$age
print(data[5])

#2 
sorted_data = sort(data)
sorted_data
print(sorted_data[15])

#3
my_list <- c(sorted_data[1:15])

#4
sorted_data_d = sort(data,decreasing = T)
sorted_data_d
print(sorted_data_d[1:5])

#5 
average_age = mean(data)
print(average_age)

#6
sd_data = sd(data)
print(sd_data)

#7
age_diff = data - average_age
print(age_diff)

#8 
average_diff = mean(age_diff)
print(average_diff)

#9
hist(data)
plot(data)
boxplot(data)
stripchart(data)
