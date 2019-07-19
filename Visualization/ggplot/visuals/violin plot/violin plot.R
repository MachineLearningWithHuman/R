#Advance ggplot codes

data<-mtcars
head(data)

#factorization
data$cyl<-as.factor(data$cyl)


library(ggplot2)
p<-ggplot(data,aes(y=mpg,x=cyl))+geom_violin()

p

# Rotate the violin plot
p + coord_flip()

# Set trim argument to FALSE
ggplot(data, aes(y=mpg,x=cyl)) + 
geom_violin(trim=FALSE)

# violin plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=23, size=2)
# violin plot with median points
p + stat_summary(fun.y=median, geom="point", size=2, color="red")
#Add median and quartile
p + geom_boxplot(width=0.1)

#Add mean and standard deviation
p + stat_summary(fun.data="mean_sdl",
                 geom="crossbar", width=0.2 )
p + stat_summary(fun.data=mean_sdl, 
                 geom="pointrange", color="red")

# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m- 3*sd(x)
  ymax <- m+3*sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

p + stat_summary(fun.data=data_summary,color="blue")


#color change
p<-ggplot(data,aes(y=mpg,x=cyl,color=cyl))+geom_violin()
p

#filling
p<-ggplot(data,aes(y=mpg,x=cyl,fill=cyl))+geom_violin()
p

#legend
p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none") # Remove legend


#changeing the orders
p + scale_x_discrete(limits=c("6", "8", "4"))

#Violin plot with multiple groups
p<-ggplot(data,aes(y=mpg,x=cyl,fill=vs))+geom_violin()
p


#you can add more 
#copyright satyajit maitra