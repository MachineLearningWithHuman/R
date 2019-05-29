#loading the titanic data 
#DATASET already loaded into our workspace

view(train)
view(test)

#structure of the dataset
str(train)
train$Survived<-as.factor(train$Survived)


#lets use only price and age inprediction 

train_f<-train[c(6,10)]
train_t<-train[2]
train_t$Survived = factor(train_t$Survived, levels = c(0, 1)) 


test_f<-test[c(5,9)]


#scalling features
train_f<-scale(train_f)
test_f<-scale(test_f)

train_c<-cbind(train_f,train_t)
# Fitting SVM to the Training set 
install.packages('e1071') 
library(e1071) 

classifier = svm(formula = Survived ~ ., 
                 data = train_c, 
                 type = 'C-classification', 
                 kernel = 'linear')
classifier

#prediction
y_pred = predict(classifier, newdata = test_f) 



# installing library ElemStatLearn 
library(ElemStatLearn) 

# Plotting the training data set results 
set = train_c
X1 = seq(-2, 2, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) 

grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('Age', 'Fare') 
y_grid = predict(classifier, newdata = grid_set) 

plot(set[, -3], 
     main = 'SVM (Training set)', 
     xlab = 'Age', ylab = 'Fare', 
     xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) 

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine')) 

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3')) 


