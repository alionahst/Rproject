#salt tolerance index 

# for the loop down
STI <- numeric()
new <- c(1:6, 8:15, 17:25)



# THIS WORKS !! but we need to do 5 times
a1<- subset(table$Shoot_Length, table$Treatment=="Control")
a2<- subset(table$Shoot_Length, table$Treatment=="Treatment")
tab1 <- data.frame(a1,a2)
tab1 <- tab1 %>%
  mutate(STI_Shoot_Length = a1/ a2)  %>%
  select (STI_Shoot_Length)

#### try to make loop
for (i in names(table[4:8])){
  for (j in new){
    a1<- subset(table$[i], table$Treatment=="Control")
    a2<- subset(table$[i], table$Treatment=="Treatment")
  }
  #store in the column 
}


# other loop that is weird
for (i in names(table[4:8])){
# create the column
    for (j in new){
    x <- subset(table[[i]], table$Number == j)
  STI <- c(STI, x[1]/x[2])
  }
  #store in the column 
}
STI
salt_tolerance_evaluation <- data.frame(STI)






MFV <- c()


