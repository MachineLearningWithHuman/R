#view data
head(population)


#First, we will take a simple random sample of the data. For the sake of consistency, we will make the random sample the same size (192)
random_sample = population[sample(length(population$Course),192),]
random_sample

# Now we will determine whether the four variables' behavior for the random sample is comparable to their behavior for the population
random_sample_percent = 100*prop.table(table(random_sample$Handed))
random_sample_percent

pop_percent = 100*prop.table(table(population$Handed))
pop_percent


barplot(rbind(pop_percent,random_sample_percent), beside=T, col=c(0,1),legend.text=T,xlab="Handedness",ylab="Percent in Group",args.legend=list(x="topleft"))


pie(pop_percent,labels=paste(c("left=","right="),round(pop_percent,0),"%"),main="Population"); 
pie(random_sample_percent,labels=paste(c("left=","right="),round(random_sample_percent,0),"%"),main="Random Sample")


summary(population$Verbal)
summary(random_sample$Verbal)
