#svm with iris data 

#i,porting the required library
library(e1071)


#As theiris dat is already loaded in the R workstation type iris to see it
head(iris,5)

#To ork with the same data we have to attach it with workstation
attach(iris)

#explain feature and target
x<-subset(iris,select=-Species)
y<-Species



#define model
model<-svm(x,y)


#summary
summary(model)

#prediction
pred<-predict(model,x)

#confusion matrix
table(pred,y)


#tuining the hyperparameter
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
#printing the result
print(svm_tune)

#new model 
svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)


#prediction
pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))
#matrix
table(pred,y)



#End of script