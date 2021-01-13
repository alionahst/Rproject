#Mean and SD for variables
x <- data.frame(dplyr::filter(table, table$Treat_Contr=="Treatment"))
x <- x[-(1:3)]
y <- data.frame(dplyr::filter(table, table$Treat_Contr=="Control"))
y <- y[-(1:3)]

Mean_Treat <- x %>% 
  summarise(Mean_SL = mean(Shoot_Length),
            Mean_SL = mean(Root_Length), 
            Mean_PH = mean(Plant_Height), 
            Mean_LN = mean(Leaf_Number), 
            Mean_LA = mean(Leaf_Area),
            Mean_FW = mean(Fresh_Weight),
            Mean_DW = mean(Dry_Weight),
            Mean_RWC = mean(RWC),
            Mean_Na = mean(Na),
            Mean_K = mean(K),
            Mean_Ca = mean(Ca),
            Mean_Mg = mean(Mg),
            Mean_K_Na = mean(K_Na),
            Mean_EL = mean(Electrolyte_Leakage),
            Mean_CC = mean(Chlorophyll_Content),
            Mean_PR = mean (Photsynthesis_Rate, na.rm=T),
            Mean_ICO2 = mean(Intercellular_CO2, na.rm=T),
            Mean_TR = mean(Transpiration_Rate, na.rm=T),
            Mean_S = mean(Stomatal_Conductance, na.rm=T)) 
Mean_Control <- y %>% 
  summarise(Mean_SL = mean(Shoot_Length),
            Mean_SL = mean(Root_Length), 
            Mean_PH = mean(Plant_Height), 
            Mean_LN = mean(Leaf_Number), 
            Mean_LA = mean(Leaf_Area),
            Mean_FW = mean(Fresh_Weight),
            Mean_DW = mean(Dry_Weight),
            Mean_RWC = mean(RWC),
            Mean_Na = mean(Na),
            Mean_K = mean(K),
            Mean_Ca = mean(Ca),
            Mean_Mg = mean(Mg),
            Mean_K_Na = mean(K_Na),
            Mean_EL = mean(Electrolyte_Leakage),
            Mean_CC = mean(Chlorophyll_Content),
            Mean_PR = mean (Photsynthesis_Rate, na.rm=T),
            Mean_ICO2 = mean(Intercellular_CO2, na.rm=T),
            Mean_TR = mean(Transpiration_Rate, na.rm=T),
            Mean_S = mean(Stomatal_Conductance, na.rm=T)) 
Treatment <- c('Treatment','Control')
Means <- rbind(Mean_Treat, Mean_Control)
Means$Treat_Contr <- Treatment

Sd_Treatment <- x %>%
  summarise(SD_SL = sd(Shoot_Length),
            SD_RL = sd(Root_Length),
            SD_PH = sd(Plant_Height),
            SD_LN = sd(Leaf_Number),
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
            SD_LN = sd(Leaf_Number),
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