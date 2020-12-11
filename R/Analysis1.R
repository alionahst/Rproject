


a1 <- aov(Shoot_Length ~ Treatment, data = table)
shapiro.test(a1$residuals)

a1 <- aov(Shoot_Length ~ Treatment, data = table)
shapiro.test(a1$residuals)


#want to make a table to 
Normality <- data.frame()
for (i in names(table)[-(1:3)]){
  a1 <- aov(i ~ Treatment, data = table)
  x <- shapiro.test(a1$residuals) 
  Normality <- c(Normality, x)
}
  
  
  
  

