#load the dataset

data<-read.csv("data.csv")

#checking the dimention of the data
dim(data)
 
#column names check
colnames(data)

#some information about the data 
#insurance: 0=none, 1=government, 2=private
#fh = family history of diabetes (yes/no, where 1=yes, 0=no)
#smoking: 1=current, 2=never and 3=ex

#to analysis any thing on the data set we will be prefer using different variables rather than tamper with the data
chol<-data$chol
library(ggplot2)

#making hist plot for cholestrol

ggplot(data =data,aes(x=chol))+geom_histogram()


#gender 
gender<-as.factor(data$gender)
dm<-as.factor(data$dm)

#analysis of gender
t<-table(gender)
addmargins(t)


#see proportions
round(prop.table(t),digits=3) # get proportions rounded to 3dp


#dm analysis
dm2<-factor(dm,exclude = NULL)
table(dm2)

#chol statistics
summary(chol)

#same for height and weight 
height <- data[,'height']
weight <- data[,'weight']
summary(height)
summary(weight)


#As this is a US data set, height is in inches and weight is in pounds.
#Neither height nor weight are particularly useful by themselves, however, 
#so it's common to combine them into the body mass index (BMI), which is weight divided by the 
#square of height; both measures need to be in SI units, i.e. kilograms and metres, so we need to convert:

height.si <- height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2

summary(bmi)

#How to make a categorical variable from a continuous one
bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

table(bmi_categorised, exclude = NULL) 

# frequencies of diabetes by BMI category 
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL)

# check 
dm_by_bmi_category 

# with the row percentages 
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) 


#simpe regression making
m <- glm(dm ~ 1, family=binomial (link=logit))
summary(m)


table(m$y)


#with gender
m <- glm(dm ~ gender, family=binomial (link=logit))
summary(m)


#with age
m <- glm(dm ~ age, family=binomial (link=logit))
summary(m)


# create a cross tabulation of age and diabetes status  
dm_by_age <- table(data$age, data$dm) 

# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 

# calculate the log odds 
logodds <- log(odds) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 


#age distribution
hist(data$age)

d <- density(data$age) 
plot(d,main = "")


#multiple regression
m <- glm(dm ~ age + gender + bmi, family=binomial (link=logit)) 
summary(m) 
