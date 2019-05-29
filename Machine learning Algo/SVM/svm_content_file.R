

#linear space
x<-c(1,2,3,4,5,6,7,8,9)
y<-c(9,8,7,6,5,4,3,2,1)
z<-(x*x) + (y*y)

plot(x,y,col="red")



# non linear space

X<-x**2
Y<-y**2
Z<-X+Y
plot(X,Y)
