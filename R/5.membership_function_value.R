#Membership function value 
library(dplyr)
# 1-results for the traits inversely related to salt tolerance: which are electrolyte leakage, Na, K and Ca
MFV <- STI %>%
  mutate(SL = (STI$STI_SL - min(STI$STI_SL))/(max(STI$STI_SL)- min(STI$STI_SL)),
         RL = (STI$STI_RL - min(STI$STI_RL))/(max(STI$STI_RL)- min(STI$STI_RL)),
         PH = (STI$STI_PH - min(STI$STI_PH))/(max(STI$STI_PH)- min(STI$STI_PH)),
         LN = (STI$STI_LN - min(STI$STI_LN))/(max(STI$STI_LN)- min(STI$STI_LN)),
         LA = (STI$STI_LA - min(STI$STI_LA))/(max(STI$STI_LA)- min(STI$STI_LA)),
         FW = (STI$STI_FW - min(STI$STI_FW))/(max(STI$STI_FW)- min(STI$STI_FW)),
         DW = (STI$STI_DW - min(STI$STI_DW))/(max(STI$STI_DW)- min(STI$STI_DW)),
         RWC = (STI$STI_RWC - min(STI$STI_RWC))/(max(STI$STI_RWC)- min(STI$STI_RWC)),
         EL = 1-(STI$STI_EL - min(STI$STI_EL))/(max(STI$STI_EL)- min(STI$STI_EL)),
         CC = (STI$STI_CC - min(STI$STI_CC))/(max(STI$STI_CC)- min(STI$STI_CC)),
         Na = 1-(STI$STI_Na - min(STI$STI_Na))/(max(STI$STI_Na)- min(STI$STI_Na)),
         K = 1-(STI$STI_K - min(STI$STI_K))/(max(STI$STI_K)- min(STI$STI_K)),
         Ca = 1- (STI$STI_Ca - min(STI$STI_Ca))/(max(STI$STI_Ca)- min(STI$STI_Ca)),
         Mg = (STI$STI_Mg - min(STI$STI_Mg))/(max(STI$STI_Mg)- min(STI$STI_Mg)),
         K_Na = (STI$STI_K_Na - min(STI$STI_K_Na))/(max(STI$STI_K_Na)- min(STI$STI_K_Na)),
         PR = (STI$STI_PR - min(STI$STI_PR, na.rm=T))/(max(STI$STI_PR, na.rm=T)- min(STI$STI_PR, na.rm=T)),
         ICO2 = (STI$STI_ICO2 - min(STI$STI_ICO2, na.rm=T))/(max(STI$STI_ICO2, na.rm=T)- min(STI$STI_ICO2, na.rm=T)),
         TR = (STI$STI_TR - min(STI$STI_TR, na.rm=T))/(max(STI$STI_TR, na.rm=T)- min(STI$STI_TR, na.rm=T)),
         SC = (STI$STI_SC - min(STI$STI_SC, na.rm=T))/(max(STI$STI_SC, na.rm=T)- min(STI$STI_SC, na.rm=T)))%>%
  select(SL, RL, PH, LN, LA, FW, DW, RWC, EL, CC, Na, K, Ca, Mg, K_Na, PR, ICO2, TR, SC)