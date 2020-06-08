library(mlbench)
data <- data("BostonHousing")
head(BostonHousing)

#summary
summary(BostonHousing)

#str
str(BostonHousing)

#basic plot
plot(BostonHousing$age) #age distribution

#scatter
plot(BostonHousing$age,BostonHousing$tax) #no corelation

#plot 
plot(BostonHousing)

#modify plot
plot(BostonHousing$age,type = "b")
plot(BostonHousing$age,type = "h")

#label and titles
plot(BostonHousing$age, xlab = 'age', ylab = 'Number', main = 'age representation', col = 'green')

#go to bar
barplot(BostonHousing$age,xlab = "age",main="age representation")
barplot(BostonHousing$age,xlab = "age",main="age representation",horiz = TRUE)

#hist
hist(BostonHousing$crim)
hist(BostonHousing$crim,main = "...",xlab = ",,,",col = "red")

#boxplot
boxplot(BostonHousing$age)
boxplot(BostonHousing[,0:4],main="many")

#grid of chart
par(mfrow=c(3,3), mar=c(2,5,2,1), las=1, bty="n")
boxplot(BostonHousing$age)
boxplot(BostonHousing[,0:4],main="many")
hist(BostonHousing$crim)
hist(BostonHousing$crim,main = "...",xlab = ",,,",col = "red")
plot(BostonHousing$age,type = "b")
plot(BostonHousing$age,type = "h")
barplot(BostonHousing$age,xlab = "age",main="age representation")
barplot(BostonHousing$age,xlab = "age",main="age representation",horiz = TRUE)
plot(BostonHousing$age)


#basic is over
library(lattice)
attach(mtcars)
head(mtcars)
gear<-as.factor(gear)
cyl <- as.factor(cyl)

#density plot
densityplot(~mpg,main="Density Plot",  xlab="Miles per Gallon")

#scatter plot
splom(mtcars[c(1,3,4,5,6)], main="MTCARS Data")

xyplot(mpg~wt|cyl*gear,  
       main="Scatterplots : Cylinders and Gears",  
       ylab="Miles/Gallon", xlab="Weight of Car")

library(ggplot2)

#Loading the dataset
attach(mtcars)
# create factors with value labels 

mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),  
                      labels=c("3gears", "4gears", "5gears"))  
mtcars$am <- factor(mtcars$am,levels=c(0,1),  
                    labels=c("Automatic","Manual"))  
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),  
                     labels=c("4cyl","6cyl","8cyl"))

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + geom_point()
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = as.factor(cyl))) + geom_point()
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, size = qsec)) + geom_point()
p  <-  ggplot(mtcars,aes(mpg, wt, shape  =  factor(cyl)))
p + geom_point(aes(colour  =  factor(cyl)), size  =  4) + geom_point(colour  =  "grey90", size  =  1.5)

#plotly
library(plotly)
p <- plot_ly(data = mtcars, x = ~hp, y = ~wt)
p
p <- plot_ly(data = mtcars, x = ~hp, y = ~wt, marker = list(size = 10, color = 'rgba(255, 182, 193, .9)', line = list(color = 'rgba(152, 0, 0, .8)', width = 2)))
p

#marker and line
data1 <- rnorm(100, mean = 10)   
data2 <- rnorm(100, mean = 0)   
data3 <- rnorm(100, mean = -10)   
x <- c(1:100)
data <- data.frame(x, data1, data2, data3)
p <- plot_ly(data, x = ~x)%>%   
  
  add_trace(y = ~data1, name = 'data1',mode = 'lines')%>%             
  add_trace(y = ~data2, name = 'data2', mode = 'lines+markers')%>% 
  add_trace(y = ~data3, name = 'data3', mode = 'markers')
p


#custom
p <- plot_ly(data = mtcars, x =~hp, y = ~wt,color = ~hp, size = ~hp )
p



#geographic data
data <- read.csv('ABC_locations.csv', sep=",")
head(data)

#map shape
window()
plot(data$Longitude,data$Latitude)
library(maps)

#basemap of US
map(database = "state")
symbols(data$Longitude, data$Latitude, squares =rep(1, length(data$Longitude)), inches=0.03, add=TRUE)
symbols(data$Longitude, data$Latitude,bg = 'red', fg = 'red', squares =rep(1, length(data$Longitude)), inches=0.03, add=TRUE)
