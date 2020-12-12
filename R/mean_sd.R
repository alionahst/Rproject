

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










