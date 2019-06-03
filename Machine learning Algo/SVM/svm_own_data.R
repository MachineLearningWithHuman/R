#Support Vector Machines in R


#Linear SVM Classifier

#generating data

set.seed(10111)
#normal distribution
x = matrix(rnorm(40), 20, 2)

#target vriable
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1

#plotting the data see plot1.png file
plot(x, col = y + 3, pch = 19)


#loading the library
library(e1071)

#making data and model
data = data.frame(x, y = as.factor(y))
svm_model = svm(y ~ ., data = data, kernel = "linear", cost = 10, scale = FALSE)
print(svm_model)


#ploting decision boundary plot2
plot(svm_model, data)



#making our own plot
make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}



xgrid = make.grid(x)
xgrid[1:10,]


#plot3
ygrid = predict(svm_model, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svm_model$index,], pch = 5, cex = 2)