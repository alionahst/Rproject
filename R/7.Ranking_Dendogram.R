#install.packages('dendextend')
library(dendextend)
library(dplyr)


# Result from PCA analysis (from PCA_analysis.R)
print(MFV_var_scores_ranked)

# We used 6 highest ranked variables to include in the final MFV based ranking of salt tolerance
# Add into the data the mean MFV column from the 6 important variables 
MFV <- MFV %>%
  mutate(Mean = rowMeans(MFV[c(1:4, 6:7)], na.rm=T))%>%
  select(SL, RL, PH, NL, LA, FW, DW, RWC, EL, CC, Na, K, Ca, Mg, K_Na, PR, ICO2, TR, SC, Mean)


# create the ranking table out of MFV mean from 6 important variables
MFV_Ranked <- MFV[order(-MFV$Mean),]
MFV_Ranked <- MFV_Ranked[20]
MFV_Ranked$Category <- c("Highly tolerant",(rep("Tolerant", 4)), (rep("Moderately tolerant", 15)), (rep("Sensitive", 4)), "Highly sensitive" )



#table with MFV from 6 important variables of PCA
important_var <- na.omit(MFV)
important_var <- MFV[c(1:4, 6:7)]

#make the dendogram
dend <- important_var %>% 
  scale %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram %>%
  set("branches_k_color", 
      value = c("#CC66FF", "#33CCFF","#99FF66", "#00CC33"),
      k = 4)%>% # make the 4 groups and color for groups
  plot(main = "MFV Dendrogram") # plot dendogram

