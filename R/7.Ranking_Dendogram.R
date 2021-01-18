#install.packages('dendextend')
library(dendextend)
library(dplyr)

# We used 6 highest ranked variables to include in the final MFV based ranking of salt tolerance
# Add into the data the mean MFV column from the 6 important variables 
MFV <- MFV %>%
  mutate(Mean = rowMeans(MFV[c(1:4, 6:7)], na.rm=T))%>%
  select(SL, RL, PH, LN, LA, FW, DW, RWC, EL, CC, Na, K, Ca, Mg, K_Na, PR, ICO2, TR, SC, Mean)

# create the ranking table out of MFV mean from 6 important variables
MFV_Ranked <- MFV[order(-MFV$Mean),]
MFV_Ranked <- MFV_Ranked[20]
MFV_Ranked$Category <- c(rep("Tolerant", 8), rep("Moderately tolerant", 13), rep("Sensitive", 4))

#data frame with MFV from 6 important variables of PCA
important_var <- MFV[c(1:4, 6:7)]

#make the dendrogram
dend <- important_var %>% 
  scale %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram %>%
  set("branches_k_color",                                   #setting the color 
      value = c("#FF3333", "#3366CC", "#009966"),           #of the branches 
      k = 3)%>% # make the 3 groups and color for groups    #according to the clusters
  plot(main = "MFV Dendrogram") # plot dendrogram