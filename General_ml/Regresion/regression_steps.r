#feature selection

head(mtcars)
mtcars = mtcars

#structure of mtcars
str(mtcars)

head(mtcars)
tail(mtcars)


#randomly fitting one variable to predict mpg
Linearreg_v1 = lm(mpg~wt, data = mtcars)

#summary
summary(Linearreg_v1)

#wt p value is close to 0 R value suggested that 74 % variation is explained by the variable

#it is a factor and not numerical value
mtcars$cyl<-as.factor(mtcars$cyl)
Linearreg_v2 = lm(mpg~cyl,data = mtcars)
#summary
summary(Linearreg_v2)


#





#Backward Elimination Model

backward_model_v1 = lm(mpg~.,data=mtcars)
summary(backward_model_v1)

mtcars_b = mtcars
mtcars_b$vs<-NULL


backward_model_v2<-lm(mpg~.,data=mtcars_b)
summary(backward_model_v2)

mtcars_b$cyl<-NULL


backward_model_v3<-lm(mpg~.,data=mtcars_b)
summary(backward_model_v3)

mtcars_b$carb<-NULL

backward_model_v4<-lm(mpg~.,data=mtcars_b)
summary(backward_model_v4)

mtcars_b$gear<-NULL
backward_model_v5<-lm(mpg~.,data=mtcars_b)
summary(backward_model_v5)


mtcars_b$drat<-NULL
backward_model_v6<-lm(mpg~.,data=mtcars_b)
summary(backward_model_v6)

#we are satisfied