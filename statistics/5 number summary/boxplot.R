#side by side boxplot creation

data<-grad_data

#columns
colnames(data)

#summary
summary(data)

#boxplot
boxplot(data, xlab="Colleges",ylab ="Graduation Rates", main="Comparison of Graduation Rates",horizontal = TRUE)
