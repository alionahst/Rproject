# 1-results for the traits inversely related to salt tolerance: which are electrolyte leakage, Na, K and Ca
# we know when to use the second equation based on pearson correlation.
MFV <- STI %>%
  mutate(SL = (STI$Shoot_Length - min(STI$Shoot_Length))/(max(STI$Shoot_Length)- min(STI$Shoot_Length)),
         RL = (STI$Root_Length - min(STI$Root_Length))/(max(STI$Root_Length)- min(STI$Root_Length)),
         PH = (STI$Plant_Height - min(STI$Plant_Height))/(max(STI$Plant_Height)- min(STI$Plant_Height)),
         NL = (STI$Number_Leaves - min(STI$Number_Leaves))/(max(STI$Number_Leaves)- min(STI$Number_Leaves)),
         LA = (STI$Leaf_Area - min(STI$Leaf_Area))/(max(STI$Leaf_Area)- min(STI$Leaf_Area)),
         FW = (STI$Fresh_Weight - min(STI$Fresh_Weight))/(max(STI$Fresh_Weight)- min(STI$Fresh_Weight)) ,
         DW = (STI$Dry_Weight - min(STI$Dry_Weight))/(max(STI$Dry_Weight)- min(STI$Dry_Weight)),
         RWC = (STI$Relative_water_content - min(STI$Relative_water_content))/(max(STI$Relative_water_content)- min(STI$Relative_water_content)),
         EL = 1-(STI$Electrolyte_Leakage - min(STI$Electrolyte_Leakage))/(max(STI$Electrolyte_Leakage)- min(STI$Electrolyte_Leakage)),
         CC = (STI$Chlorophyll_Content - min(STI$Chlorophyll_Content))/(max(STI$Chlorophyll_Content)- min(STI$Chlorophyll_Content)),
         Na = 1-(STI$Na - min(STI$Na))/(max(STI$Na)- min(STI$Na)),
         K = 1-(STI$K - min(STI$K))/(max(STI$K)- min(STI$K)),
         Ca =1- (STI$Ca - min(STI$Ca))/(max(STI$Ca)- min(STI$Ca)),
         Mg=(STI$Mg - min(STI$Mg))/(max(STI$Mg)- min(STI$Mg)),
         K_Na= (STI$K_Na - min(STI$K_Na))/(max(STI$K_Na)- min(STI$K_Na)),
         PR= (STI$Photsynthesis_Rate - min(STI$Photsynthesis_Rate, na.rm=T))/(max(STI$Photsynthesis_Rate, na.rm=T)- min(STI$Photsynthesis_Rate, na.rm=T)),
         ICO2= (STI$Intercellular_CO2 - min(STI$Intercellular_CO2, na.rm=T))/(max(STI$Intercellular_CO2, na.rm=T)- min(STI$Intercellular_CO2, na.rm=T)),
         TR= (STI$Transpiration_Rate - min(STI$Transpiration_Rate, na.rm=T))/(max(STI$Transpiration_Rate, na.rm=T)- min(STI$Transpiration_Rate, na.rm=T)),
         SC= (STI$Stomatal_Conductance - min(STI$Stomatal_Conductance, na.rm=T))/(max(STI$Stomatal_Conductance, na.rm=T)- min(STI$Stomatal_Conductance, na.rm=T)))%>%
  select(SL, RL, PH, NL, LA, FW, DW, RWC, EL, CC, Na, K, Ca, Mg, K_Na, PR, ICO2, TR, SC)

