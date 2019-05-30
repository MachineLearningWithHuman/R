#Applying svm with caret package
library(caret)

#importing dataset
library(readr)
heart_tidy <- read_csv("C:/Users/USER/Downloads/heart_tidy.csv")


#visualizing the structure of data
str(heart_tidy)


#Total colomn
length(heart_tidy)


#head ofthe data
head(heart_tidy)

#train and test

set.seed(3033)
intrain <- createDataPartition(y = heart_tidy$`0_2`, p= 0.9, list = FALSE)
training <- heart_tidy[intrain,]
testing <- heart_tidy[-intrain,]

#checking missing value
anyNA(heart_tidy)


#collective summary
summary(heart_tidy)





#converting target into factor
training[["0_2"]] = factor(training[["0_2"]])
training[["target"]]=training[["0_2"]]
training=training[-14]

#test
testing[["0_2"]] = factor(testing[["0_2"]])
testing[["target"]]=testing[["0_2"]]
testing=testing[-14]



#model svm
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(target ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)


#checking result 
svm_Linear


#test data
 test_pred <- predict(svm_Linear, newdata = testing)
 test_pred
 
 
 #confusion matrix acc:89.66%
 confusionMatrix(test_pred, testing$target )
 
 
 
 #plot
 grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
 set.seed(3233)
 svm_Linear_Grid <- train(target ~., data = training, method = "svmLinear",
                          trControl=trctrl,
                          preProcess = c("center", "scale"),
                          tuneGrid = grid,
                          tuneLength = 10)
 
 
 svm_Linear_Grid
 #see plot1
 plot(svm_Linear_Grid)
 
 
 
 test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
 test_pred_grid
 confusionMatrix(test_pred_grid, testing$target )
 
 
 #try non linear kernel yourself
 