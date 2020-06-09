#PCA variable in same unit
options(repr.plot.width=8, repr.plot.height=5)

library(tidyverse)
library(dplyr)

library(corrplot)

#load the data
iris = read.csv("https://raw.githubusercontent.com/venky14/Machine-Learning-with-Iris-Dataset/master/Iris.csv")

#data
head(iris)

#we don't need species and id
species = iris$Species  
iris = iris %>% select(!c(Id,Species))


#same unit- no scaling required
correlations = cor(iris)
corrplot(correlations, method="circle", addCoef.col = "white", 
         title = 'Fig.1. Correlation', mar=c(0,0,1,0)) 

#apply pca into the data
pcp = prcomp(iris, center = TRUE, scale. = FALSE)  # center it but do not scale it
plot(pcp)

library(ggplot2)
variance = data_frame(PC = paste0('PC', seq(1,4)), variance = round(100.00*pcp$sdev^2/sum(pcp$sdev^2),4))
variance %>% ggplot(aes(x = PC, y = variance)) + geom_bar(stat = 'identity') + ggtitle('Fig.2. Variance Explained') + 
  theme(axis.title = element_text(size=14), plot.title = element_text(size = 16,colour="darkblue", hjust = 0.5),
        axis.text = element_text(size=14)) + geom_text(aes(label = variance), vjust = -0.4) + xlab('') + 
  ylab('Variance Explained (%)')

#importance
round(pcp$rotation, 4)

#visualization
library(reshape2)
rotation = as.data.frame(pcp$rotation)
rotation$dimension = row.names(rotation)
rotation = rotation %>% melt(key = dimension, value = pc) %>% mutate(magnitude = abs(value))
rotation %>% ggplot(aes(x  = dimension, y = variable , fill = magnitude)) + geom_tile()+xlab('') + ylab('') +
  scale_fill_gradient2(low = "sky blue", high = "red") + ggtitle('Fig.3. Loadings of PCs') + 
  theme(axis.title = element_text(size=14), plot.title = element_text(size = 18,colour="darkblue", hjust = 0.5),
        axis.text = element_text(size=14)) + geom_text(aes(label = round(magnitude,3)))

#sample value
round(head(pcp$x,10),5)

#corelation
round(cor(pcp$x), 5)

#2 pc
df = as.data.frame(pcp$x)
df$Species = species
df %>% ggplot(aes(x = PC1, y = PC2, color = Species)) + geom_point() +
  ggtitle('Fig.4. The first two PCs') + theme(axis.title = element_text(size=14),
                                              plot.title = element_text(size = 18, hjust = 0.5),
                                              axis.text = element_text(size=14))




#------------------------------------
#different units
df = read.csv('https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/climate_change.csv')
head(df)

#change in temp
df %>% ggplot(aes(x = Year, y = Temp)) + geom_bar(stat = 'identity') +
  ggtitle('Fig. 7. Global Temperature Anomaly') + 
  theme(axis.title = element_text(size=14), plot.title = element_text(size = 18, hjust = 0.5),
        axis.text = element_text(size=14))
correlations = cor(df %>% select(-Temp))
corrplot(correlations, method="number", diag=FALSE,  title = 'Fig.8. Correlation', mar=c(0,0,1,0))

#apply pca
pca1 = prcomp(df %>% select(-Temp), center = TRUE, scale. = TRUE)

scaled = scale(df %>% select(-Temp), center = TRUE, scale = TRUE)
my_svd  = svd(scaled)
all(pca1$x, my_svd$v)


#variance explaine
variance = data_frame(PC = paste0('PC', seq(1,10)), variance = round(100.00*pca1$sdev^2/sum(pca1$sdev^2),4)) %>%
  mutate(PC = factor(PC,levels = PC[order(variance,decreasing =F)]))
variance %>% ggplot(aes(x = PC, y = variance)) + geom_bar(stat = 'identity', fill = 'skyblue') + ggtitle('Fig.9. Variance Explained') + 
  theme(axis.title = element_text(size=14), plot.title = element_text(size = 16,colour="darkblue", hjust = 0.5),
        axis.text = element_text(size=14)) + geom_text(aes(label = variance), hjust =0.5) + xlab('') + 
  ylab('Variance Explained (%)') + coord_flip()

#loading
pca1$rotation
round(cor(pca1$x),10)
