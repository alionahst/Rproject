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
x <- data.frame(dplyr::filter(table, table$Treat_Contr=="Treatment"))
x <- x[-(1:3)]
y <- data.frame(dplyr::filter(table, table$Treat_Contr=="Control"))
y <- y[-(1:3)]

Mean_Treat <- x %>% 
  summarise(Shoot_Length = mean(Shoot_Length),
            Root_Length = mean(Root_Length), 
            Plant_Height = mean(Plant_Height), 
            Number_Leaves = mean(Number_Leaves), 
            Leaf_Area = mean(Leaf_Area),
            Fresh_Weight = mean(Fresh_Weight),
            Dry_Weight = mean(Dry_Weight),
            RWC = mean(RWC),
            Na = mean(Na),
            K = mean(K),
            Ca = mean(Ca),
            Mg = mean(Mg),
            K_NA = mean(K_Na),
            Electrolyte_Leakage = mean(Electrolyte_Leakage),
            Chlorophyll_Content = mean(Chlorophyll_Content),
            Photsynthesis_Rate = mean (Photsynthesis_Rate, na.rm=T),
            Intercellular_CO2 = mean(Intercellular_CO2, na.rm=T),
            Transpiration_Rate = mean(Transpiration_Rate, na.rm=T),
            Stomatal_Conductance = mean(Stomatal_Conductance, na.rm=T)) 
Mean_Control <- y %>% 
  summarise(Shoot_Length = mean(Shoot_Length),
            Root_Length = mean(Root_Length), 
            Plant_Height = mean(Plant_Height), 
            Number_Leaves = mean(Number_Leaves), 
            Leaf_Area = mean(Leaf_Area),
            Fresh_Weight = mean(Fresh_Weight),
            Dry_Weight = mean(Dry_Weight),
            RWC = mean(RWC),
            Na = mean(Na),
            K = mean(K),
            Ca = mean(Ca),
            Mg = mean(Mg),
            K_NA = mean(K_Na),
            Electrolyte_Leakage = mean(Electrolyte_Leakage),
            Chlorophyll_Content = mean(Chlorophyll_Content),
            Photsynthesis_Rate = mean (Photsynthesis_Rate, na.rm=T),
            Intercellular_CO2 = mean(Intercellular_CO2, na.rm=T),
            Transpiration_Rate = mean(Transpiration_Rate, na.rm=T),
            Stomatal_Conductance = mean(Stomatal_Conductance, na.rm=T)) 
Treatment <- c('Treatment','Control')
Means <- rbind(Treatment, Mean_Treat)
Means$Treat_Contr <- Treatment

Sd_Treatment <- x %>%
    summarise(SD_SL = sd(Shoot_Length),
            SD_RL = sd(Root_Length),
            SD_PH = sd(Plant_Height),
            SD_NL = sd(Number_Leaves),
            SD_LA = sd(Leaf_Area),
            SD_FW = sd(Fresh_Weight),
            SD_DW = sd(Dry_Weight),
            SD_RWC = sd(RWC),
            SD_Na = sd(Na),
            SD_K = sd(K),
            SD_Ca = sd(Ca),
            SD_Mg = sd(Mg),
            SD_K_NA = sd(K_Na),
            SD_EL = sd(Electrolyte_Leakage),
            SD_CC = sd(Chlorophyll_Content),
            SD_PR = sd (Photsynthesis_Rate, na.rm=T),
            SD_I_CO2 = sd(Intercellular_CO2, na.rm=T),
            SD_TR = sd(Transpiration_Rate, na.rm=T),
            SD_SC = sd(Stomatal_Conductance, na.rm=T))
Sd_Control <-   y %>%
  summarise(SD_SL = sd(Shoot_Length),
            SD_RL = sd(Root_Length),
            SD_PH = sd(Plant_Height),
            SD_NL = sd(Number_Leaves),
            SD_LA = sd(Leaf_Area),
            SD_FW = sd(Fresh_Weight),
            SD_DW = sd(Dry_Weight),
            SD_RWC = sd(RWC),
            SD_Na = sd(Na),
            SD_K = sd(K),
            SD_Ca = sd(Ca),
            SD_Mg = sd(Mg),
            SD_K_NA = sd(K_Na),
            SD_EL = sd(Electrolyte_Leakage),
            SD_CC = sd(Chlorophyll_Content),
            SD_PR = sd (Photsynthesis_Rate, na.rm=T),
            SD_I_CO2 = sd(Intercellular_CO2, na.rm=T),
            SD_TR = sd(Transpiration_Rate, na.rm=T),
            SD_SC = sd(Stomatal_Conductance, na.rm=T))
Treatment <- c('Treatment','Control')
SD <- rbind(Sd_Treatment,Sd_Control)
SD$Treat_Contr <- Treatment
  

