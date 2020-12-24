
# Result from PCA analysis (from PCA_analysis.R)
print(MFV_var_scores_ranked)

# We used 8 highest ranked variables to include in the final MFV based ranking
# of salt tolerance
MFV <- MFV %>%
  mutate(Mean = rowMeans(MFV[c(1:7, 15)], na.rm=T))%>%
  #mutate(Mean = rowMeans(MFV[1:5], na.rm=T))%>% 
  select(SL, RL, PH, NL, LA, FW, DW, RWC, EL, CC, Na, K, Ca, Mg, K_Na, PR, ICO2, TR, SC, Mean)



MVF_try <- na.omit(MFV)
MVF_try <- MFV[-(c(8,16:20))]

# create the ranking table
MFV_Ranked <- MFV[order(-MFV$Mean),]
MFV_Ranked <- MFV_Ranked[20]

#table with MFV from 8 important variables of PCA
important_var <- MFV[c(1:7, 15)]
names(important_var)

# dendograme 
mydi <- dist(important_var) #matrice of distance calculation 
myclust <- hclust(mydi,  method = "ward.D2")

dend <- plot(myclust, labels = NULL, hang = -1, 
     main = "Cluster dendrogram", sub = NULL,
     xlab = "Accessions", ylab = "Mean MFV")




dend <- MVF_try %>% # change MFV_Ranked with important_var in function of professor answer 
  scale %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram
dend %>% plot

#install.packages('dendextend')
library(dendextend)

# Customized colors
dend %>% 
  set("branches_k_color", 
      value = c("red", "pink","blue", "green", "yellow"), 
      k = 5) %>% 
  plot(main = "Ranking")








#other attempt

mydi <- dist(important_var) #matrice of distance calculation 
myclust <- hclust(mydi,  method = "ward.D2")

dend <- plot(myclust, labels = NULL, hang = -1, 
             main = "Cluster dendrogram", sub = NULL,
             xlab = "Accessions", ylab = "Mean MFV")

