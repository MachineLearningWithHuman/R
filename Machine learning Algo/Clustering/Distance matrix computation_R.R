#Distance matrix computation

# Subset of the data
set.seed(123)
ss <- sample(1:50, 15)   # Take 15 random rows
df <- USArrests[ss, ]    # Subset the 15 rows
df.scaled <- scale(df)   # Standardize the variables


#Computing euclidean distance
dist.eucl <- dist(df.scaled, method = "euclidean")


#Note that, allowed values for the option method include one of: "euclidean",
#"maximum", "manhattan", "canberra", "binary", "minkowski"

# Reformat as a matrix
# Subset the first 3 columns and rows and Round the values
round(as.matrix(dist.eucl)[1:3, 1:3], 1)



#Computing correlation based distances
# Compute
library("factoextra")
dist.cor <- get_dist(df.scaled, method = "pearson")

# Display a subset
round(as.matrix(dist.cor)[1:3, 1:3], 1)


#The R code below applies the daisy() function on flower data which contains factor, ordered and numeric variables:
library(cluster)
# Load data
data(flower)
head(flower, 3)



# Data structure
str(flower)



# Distance matrix
dd <- daisy(flower)
round(as.matrix(dd)[1:3, 1:3], 2)


library(factoextra)
fviz_dist(dist.eucl) #see plot1

#We described how to compute distance matrices using either Euclidean or correlation-based measures
