data<-actor_age #load the data

#see the only Age variable in statistical summary
summary(data$Age)

#calculate mean
mean(data$Age)

#Standard Deviation
sd(data$Age)


#Variance
var(data$Age)

# Median
median(data$Age)


#Inter-quartile range
IQR(data$Age)


#For the First Quartile (25th percentile, Q1)
quantile(data$Age,0.25)


#For the Third Quartile (75th percentile, Q3)
quantile(data$Age,0.75)
