#loading the mtcars data and ggplot2
library(ggplot2)

str(mtcars)


#scatter plotting between cyl and mpg
ggplot(mtcars,aes(x=cyl,y=mpg))+geom_point()

#changing cyl to factor
mtcars$cyl<-as.factor(mtcars$cyl)

#plotting again
ggplot(mtcars,aes(x=cyl,y=mpg))+geom_point()


#basic of grammer of graphics
ggplot(mtcars, aes(x = wt, y = mpg, color = disp,size=disp)) +
  geom_point()

#trying shape
ggplot(mtcars, aes(x = wt, y = mpg, color = disp,size=disp,shape=disp)) +
  geom_point()
       

#Exploring diamond dataset and ploting a sctter plot 
# Explore the diamonds data frame with str()
str(diamonds)

# Add geom_point() with +
ggplot(diamonds, aes(x = carat, y = price))+geom_point()

# Add geom_point() and geom_smooth() with +
ggplot(diamonds, aes(x = carat, y = price))+geom_point()+geom_smooth()


#add clarity to color
ggplot(diamonds, aes(x = carat, y = price,col=clarity)) +
  geom_point()+
  
  geom_smooth()

#add clarity to color
ggplot(diamonds, aes(x = carat, y = price,col=clarity)) +
geom_smooth()


#using grammer to formulate plot
# Create the object containing the data and aes layers: dia_plot
dia_plot<-ggplot(diamonds,aes(x=carat,y=price))

# Add a geom layer with + and geom_point()
dia_plot+geom_point()

# Add the same geom layer, but with aes() inside
dia_plot+geom_point(aes(col=clarity))



set.seed(1)

# The dia_plot object has been created 
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))
dia_plot

# Expand dia_plot by adding geom_point() with alpha set to 0.2

dia_plot<-dia_plot+geom_point(alpha=0.2)
dia_plot

# Plot dia_plot with additional geom_smooth() with se set to FALSE
dia_plot<-dia_plot+geom_smooth(se=F)
dia_plot

# Copy the command from above and add aes() with the correct mapping to geom_smooth()
dia_plot<-dia_plot+geom_smooth(aes(col=clarity),se=F)
dia_plot