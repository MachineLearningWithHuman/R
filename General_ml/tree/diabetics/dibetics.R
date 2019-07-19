#decision tree

## You will need following libraries for this exercise 
library(dplyr) 
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

## Following code will help you suppress the messages and warnings during package loading      
options(warn = -1) 


cardio <- read.csv("processed.cleveland.data", header = FALSE, na.strings = '?')  

#looking at head
head(cardio)

#adding columns names
names(cardio) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", 
                    "thalach","exang", "oldpeak","slope", "ca", "thal", "status")


str(cardio)


## We can use lapply to convert data types across multiple columns  
cardio[c("sex", "cp", "fbs","restecg", "exang", 
         "slope", "ca", "thal", "status")] <- lapply(cardio[c("sex", "cp", "fbs","restecg",
                                                              "exang", "slope", "ca", "thal", "status")], factor)
## You can verify the data frame 
str(cardio)



##  We will use the 'forcats' package included in the s'tidyverse' package
##  The function to be used will be fct_collpase 
cardio$status <- forcats::fct_collapse(cardio$status, "1" = c("1","2", "3", "4"))  


## Let's also change the labels under the "status" from (0,1) to (normal, abnormal)  
levels(cardio$status) <- c("normal", "abnormal")  

## levels under sex can also be changed to (female, male)   
## We can change level names in other categorical variables as well but we are not doing that  
levels(cardio$sex) <- c("female", "male")  


## Overall summary of all the columns 
summary(cardio)


# Counting the missing values in the datframe 
sum(is.na(cardio))



## Removing missing values  
cardio <- na.omit(cardio)


## plotting a histrogram for status
barplot(cardio$status,height=2)


## frequency plots for quantitative variables, split by status  
cardio %>%
  gather(-sex, -cp, -fbs, -restecg, -exang, -slope, -ca, -thal, -status, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = ..count.. , colour = status)) +
  scale_color_manual(values=c("#008000", "#FF0000"))+
  geom_density() +
  facet_wrap(~var, scales = "free",  nrow = 2) +
  theme_bw()


## frequency plots for qualitative variables, split by status  
cardio %>%
  gather(-age, -trestbps, -chol, -thalach, -oldpeak, -status, key = "var", value = "value") %>%
  ggplot(aes(x = value, color = status)) + 
  scale_color_manual(values=c("#008000", "#FF0000"))+
  geom_histogram(stat = 'count', fill = "white") +
  facet_wrap(~var, nrow = 3) +
  facet_wrap(~var, scales = "free",  nrow = 3) +
  theme_bw()


## Now you can randomly split your data in to 70% training set and 30% test set   
## You should set seed to ensure that you get the same training vs/ test split every time you run the code    
set.seed(1) 

## randomly extract row numbers in cardio dataset which will be included in the training set  
train.index <- sample(1:nrow(cardio), round(0.70*nrow(cardio),0))

## subset cardio data set to include only the rows in train.index to get cardio.train  
cardio.train <- cardio[train.index, ]

## subset cardio data set to include only the rows NOT in train.index to get cardio.test  
## Did you note the negative sign?
cardio.test <- cardio[-train.index,  ]


## using all the predictors and setting cp = 0.05 
cardio.train.fit <- rpart(status ~ . , data = cardio.train, method = "class", cp = 0.05)

## Using fancyRpartPlot() from "rattle" package
fancyRpartPlot(cardio.train.fit, palettes = c("Greens", "Reds"), sub = "")



## using all the predictors and setting all other arguments to default 
cardioFull <- rpart(status ~ . , data = cardio.train, method = "class", cp = 0)

## Using fancyRpartPlot() from "rattle" package
fancyRpartPlot(cardioFull, palettes = c("Greens", "Reds"),sub = "")


## printing the CP table for the fully-grown tree 
printcp(cardioFull)


## plotting the cp 
plotcp(cardioFull, lty = 3, col = 2, upper = "splits" )


## selecting the best cp, corresponding to the minimum value in xerror 
bestcp <- cardioFull$cptable[which.min(cardioFull$cptable[,"xerror"]),"CP"]

## print the best cp
bestcp


## Prune the tree using the best cp.
cardio.pruned <- prune(cardioFull, cp = bestcp)

## You can now plot the pruned tree 
fancyRpartPlot(cardio.pruned, palettes = c("Greens", "Reds"), sub = "")

## printing the 
summary(cardio.pruned)  


## You can now use your pruned tree model to predict the status for your test data 
cardio.predict <- predict(cardio.pruned, cardio.test, type = "class")

# confusion matrix (training data)
conf.matrix <- table(cardio.test$status, cardio.predict)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Predicted", colnames(conf.matrix), sep = ":")
print(conf.matrix)