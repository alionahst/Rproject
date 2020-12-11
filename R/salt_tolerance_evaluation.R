
Treatment_table <- table[table$Treatment=="Treatment",]

#salt tolerance index 
STI <- numeric()

new <- c(1:6, 8:15, 17:25)


for (i in names(table[4:8])){
  for (j in new){
    x <- subset(table[[i]], table$Number == j)
  STI <- c(STI, x[1]/x[2])
  }
}
STI

salt_tolerance_evaluation <- data.frame(STI)






MFV <- c()


