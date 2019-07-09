#seeing the head of the data
head(depression)


#viewing the age variable
depression$Age

#converting numerical variable into categorical
depression$Gender = replace(depression$Gender,depression$Gender==1,'Female')
depression$Gender = replace(depression$Gender,depression$Gender==2,'Male')

#observe the change
depression$Gender
