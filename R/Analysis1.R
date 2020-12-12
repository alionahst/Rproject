#want to make a loop
Normality <- data.frame()
a1 <- numeric()
for (i in names(table)[-(1:3)]){
  a1 <- aov(i ~ Treat_Contr, data = table)
  x <- shapiro.test(a1$residuals) 
  Normality <- c(Normality, x)
}


boxplot(Shoot_Length ~ Treat_Contr, data = Morpho_t)
boxplot(Root_Length ~ Treat_Contr, data = Morpho_t)
boxplot(Height_Height ~ Treat_Contr, data = Morpho_t)
boxplot(Number_Leaves ~ Treat_Contr, data = Morpho_t)
boxplot(Leaf_Area ~ Treat_Contr, data = Morpho_t)


a1 <- aov(Shoot_Length ~ Treat_Contr, data = Morpho_t)
shapiro.test(a1$residuals)
#Normality
kruskal.test(Shoot_Length ~ Treat_Contr, data = Morpho_t)
# statistical difference

a2 <- aov(Root_Length ~ Treat_Contr, data = Morpho_t)
shapiro.test(a2$residuals)
#Normality
anova(a1)
# statistical difference


a1 <- aov(Plant_Height ~ Treat_Contr, data = Morpho_t)
shapiro.test(a1$residuals)
# No normality
anova(a1)
# statistical difference


a1 <- aov(Number_Leaves ~ Treat_Contr, data = Morpho_t)
shapiro.test(a1$residuals)
# No normality 
kruskal.test(Number_Leaves ~ Treat_Contr, data = table)
# statistical difference


a1 <- aov(Leaf_Area ~ Treat_Contr, data = Morpho_t)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Leaf_Area ~ Treat_Contr, data = table)
# statistical difference


a1 <- aov(Root_length ~ Treat_Contr, data = Morpho_t)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Root_Length ~ Treat_Contr, data = table)
#statistical difference


a1 <- aov(Fresh_Weight ~ Treat_Contr, data = Morpho_t)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Fresh_Weight ~ Treat_Contr, data = table)


a1 <- aov(Dry_Weight ~ Treat_Contr, data = Morpho_t)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Dry_Weight ~ Treat_Contr, data = table)


library(agricolae)
res <- duncan.test(a1, "Treat_Contr", DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
res
plot(res)
  
  
  

