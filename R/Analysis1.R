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
#No normality
kruskal.test(Shoot_Length ~ Treat_Contr, data = Morpho_t)
# statistical difference

a2 <- aov(Root_Length ~ Treat_Contr, data = Morpho_t)
shapiro.test(a2$residuals)
#No normality
kruskal.test(Root_Length ~ Treat_Contr, data = Morpho_t)
# statistical difference


a3 <- aov(Plant_Height ~ Treat_Contr, data = Morpho_t)
shapiro.test(a3$residuals)
# No normality
kruskal.test(Plant_Height ~ Treat_Contr, data = Morpho_t)
# statistical difference


a4 <- aov(Number_Leaves ~ Treat_Contr, data = Morpho_t)
shapiro.test(a4$residuals)
# No normality 
kruskal.test(Number_Leaves ~ Treat_Contr, data = Morpho_t)
# statistical difference


a5 <- aov(Leaf_Area ~ Treat_Contr, data = Morpho_t)
shapiro.test(a5$residuals)
# no normality 
kruskal.test(Leaf_Area ~ Treat_Contr, data = Morpho_t)
# statistical difference


for (i in names(Morpho_t[4:8])) { 
  boxplot(Morpho_t[, i] ~ Morpho_t$Treat_Contr, 
          ylab = names(Morpho_t[i]), 
          xlab = "Treatment"
  )
  print(kruskal.test(Morpho_t[, i] ~ Morpho_t$Treat_Contr))
}




########### 


a1 <- aov(Fresh_Weight ~ Treat_Contr, data = Morpho_t)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Fresh_Weight ~ Treat_Contr, data = table)


a1 <- aov(Dry_Weight ~ Treat_Contr, data = Morpho_t)
shapiro.test(a1$residuals)
# no normality 
kruskal.test(Dry_Weight ~ Treat_Contr, data = table)




#Mean and SD for variables
Mean_Treat <- x %>% 
  summarise(Shoot_Length = mean(Shoot_Length),
            Root_Length = mean(Root_Length), 
            Plant_Height = mean(Plant_Height), 
            Number_Leaves = mean(Number_Leaves), 
            Leaf_Area = mean(Leaf_Area)) 
Mean_Control <- y %>% 
  summarise(Shoot_Length = mean(Shoot_Length),
            Root_Length = mean(Root_Length), 
            Plant_Height = mean(Plant_Height), 
            Number_Leaves = mean(Number_Leaves), 
            Leaf_Area = mean(Leaf_Area)) 
Treatment <- c('Treatment','Control')
Means <- rbind(Mean_Treat,Mean_Control)
Means$Treat_Contr <- Treatment

Sd_Treatment <- x %>%
  select(1:5)%>%
  summarise(SD_SL = sd(Shoot_Length),
            SD_RL = sd(Root_Length),
            SD_PH = sd(Plant_Height),
            SD_NL = sd(Number_Leaves),
            SD_LA = sd(Leaf_Area))
Sd_Control <-   y %>%
  summarise(SD_SL = sd(Shoot_Length),
            SD_RL = sd(Root_Length),
            SD_PH = sd(Plant_Height),
            SD_NL = sd(Number_Leaves),
            SD_LA = sd(Leaf_Area))
Treatment <- c('Treatment','Control')
SD <- rbind(Sd_Treatment,Sd_Control)
SD$Treat_Contr <- Treatment
  

