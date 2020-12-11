


a1 <- aov(Shoot_Length ~ Treatment, data = table)
shapiro.test(a1$residuals)

a1 <- aov(Root_length ~ Treatment, data = table)
shapiro.test(a1$residuals)

a1 <- aov(Root_length ~ Treatment, data = table)

a1 <- aov(Root_length ~ Treatment, data = table)

a1 <- aov(Root_length ~ Treatment, data = table)

a1 <- aov(Root_length ~ Treatment, data = table)


#want to make a loop
Normality <- data.frame()
a1 <- numeric()
for (i in names(table)[-(1:3)]){
  a1 <- aov(i ~ Treatment, data = table)
  x <- shapiro.test(a1$residuals) 
  Normality <- c(Normality, x)
}
  
  
  
  

