#want to make a loop
Normality <- data.frame()
a1 <- numeric()
for (i in names(table)[-(1:3)]){
  a1 <- aov(i ~ Treatment, data = table)
  x <- shapiro.test(a1$residuals) 
  Normality <- c(Normality, x)
}


boxplot(Shoot_Length ~ Treatment, data = table)

a1 <- aov(Shoot_Length ~ Treatment, data = table)
shapiro.test(a1$residuals)
#Normality
anova(a1)
# statistical difference

a1 <- aov(Root_length ~ Treatment, data = table)
shapiro.test(a2$residuals)
#Normality
anova(a1)
# statistical difference

a1 <- aov(Plant_Height ~ Treatment, data = table)
shapiro.test(a1$residuals)
#Normality
anova(a1)
# statistical difference


a1 <- aov(Number_Leaves ~ Treatment, data = table)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Number_Leaves ~ Treatment, data = table)
# statistical difference


a1 <- aov(Leaf_Area ~ Treatment, data = table)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Leaf_Area ~ Treatment, data = table)
# statistical difference


a1 <- aov(Root_length ~ Treatment, data = table)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Root_Length ~ Treatment, data = table)
#statistical difference


a1 <- aov(Fresh_Weight ~ Treatment, data = table)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Fresh_Weight ~ Treatment, data = table)


a1 <- aov(Dry_Weight ~ Treatment, data = table)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Dry_Weight ~ Treatment, data = table)





  
  
  

