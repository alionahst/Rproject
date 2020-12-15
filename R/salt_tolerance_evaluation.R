
x <- data.frame(dplyr::filter(table, table$Treat_Contr=="Treatment"))
x <- x[-(1:3)]
y <- data.frame(dplyr::filter(table, table$Treat_Contr=="Control"))
y <- y[-(1:3)]


#salt tolerance index for morphological traits
STI_Shoot_Length <- x[1]/y[1]
STI_Root_Length <- x[2]/y[2]
STI_Plant_Height <- x[3]/y[3]
STI_Number_Leaves <- x[4]/y[4]
STI_Leaf_Area <- x[5]/y[5]

#trying to make a for loop
#STI1 <- data.frame()
#vecteur <- c()
#for (i in names(x)){
#  for (j in 1:4){
#    name <- paste("STI", i ,sep = "_" )
#    name <- x[j]/x[j]
#    vecteur <- c(vecteur, name)}}

#salt tolerance index for biomass
STI_Fresh_Weight <- x[6]/y[6]
STI_Dry_Weight <- x[7]/y[7]

#salt tolerance index for RWC, EL and Chlorophyll content
STI_Relative_water_content  <- x[8]/y[8]
names(STI_Relative_water_content)[1] <- "Relative_water_content" # change the name of the column of the data
STI_Electrolyte_Leakage  <- x[14]/y[14]
STI_Chlorophyll_Content <- x[15]/y[15]

# salt tolerance index for Mineral ion content
STI_Na <- x[9]/y[9]
STI_K <- x[10]/y[10]
STI_Ca <- x[11]/y[11]
STI_Mg <- x[12]/y[12]
STI_K_Na <- x[13]/y[13]

# salt tolerance index for Gas exchange attributes
STI_Photosynthesis_rate <- x[16]/y[16]
STI_Intercellular_CO2 <- x[17]/y[17]
STI_Transpiration_Rate <- x[18]/y[18]
STI_Stomatal_Conductance <- x[19]/y[19]

STI <- data.frame(STI_Shoot_Length,
                  STI_Root_Length, 
                  STI_Plant_Height, 
                  STI_Number_Leaves, 
                  STI_Leaf_Area, 
                  STI_Fresh_Weight, 
                  STI_Dry_Weight,
                  STI_Relative_water_content, 
                  STI_Electrolyte_Leakage,
                  STI_Na,
                  STI_K,
                  STI_Ca,
                  STI_Mg,
                  STI_K_Na,
                  STI_Chlorophyll_Content,
                  STI_Photosynthesis_rate,
                  STI_Intercellular_CO2,
                  STI_Transpiration_Rate,
                  STI_Stomatal_Conductance)

summary(STI)

#Membership function value for morphological trait, biomass
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



# make the mean of the mean 
MFV <- MFV %>%
  mutate(Mean = rowMeans(MFV[1:5], na.rm=T))%>% #mutate(Mean = rowMeans(MFV[*column needed*], na.rm=T))%>%
  select(SL, RL, PH, NL, LA, FW, DW, RWC, EL, CC, Na, K, Ca, Mg, K_Na, PR, ICO2, TR, SC, Mean)

# dendograme 






# Pearson test 
#peason test with MFV table 
cor.test(MFV$SL,MFV$RL, method="pearson") #
cor.test(MFV$SL,MFV$PH, method="pearson") #
cor.test(MFV$SL,MFV$NL, method="pearson") #
cor.test(MFV$SL,MFV$LA, method="pearson") #
cor.test(MFV$SL,MFV$FW, method="pearson") #
cor.test(MFV$SL,MFV$DW, method="pearson") #
cor.test(MFV$SL,MFV$RWC, method="pearson")
cor.test(MFV$SL,MFV$EL, method="pearson") #
cor.test(MFV$SL,MFV$CC, method="pearson") #
cor.test(MFV$SL,MFV$Na, method="pearson")
cor.test(MFV$SL,MFV$K, method="pearson")
cor.test(MFV$SL,MFV$Ca, method="pearson")
cor.test(MFV$SL,MFV$Mg, method="pearson")
cor.test(MFV$SL,MFV$K_Na, method="pearson") #
cor.test(MFV$SL,MFV$PR, method="pearson") #
cor.test(MFV$SL,MFV$ICO2, method="pearson")
cor.test(MFV$SL,MFV$TR, method="pearson")
cor.test(MFV$SL,MFV$SC, method="pearson")

cor.test(MFV$RL,MFV$PH, method="pearson") #
cor.test(MFV$RL,MFV$NL, method="pearson") #
cor.test(MFV$RL,MFV$LA, method="pearson") #
cor.test(MFV$RL,MFV$FW, method="pearson") #
cor.test(MFV$RL,MFV$DW, method="pearson") #
cor.test(MFV$RL,MFV$EL, method="pearson") #
cor.test(MFV$RL,MFV$CC, method="pearson") #
cor.test(MFV$RL,MFV$K_Na, method="pearson") #
cor.test(MFV$RL,MFV$PR, method="pearson") #

cor.test(MFV$PH,MFV$NL, method="pearson") #
cor.test(MFV$PH,MFV$LA, method="pearson") #
cor.test(MFV$PH,MFV$FW, method="pearson") #
cor.test(MFV$PH,MFV$DW, method="pearson") #
cor.test(MFV$PH,MFV$EL, method="pearson") #
cor.test(MFV$PH,MFV$CC, method="pearson") #
cor.test(MFV$PH,MFV$K_Na, method="pearson") #
cor.test(MFV$PH,MFV$PR, method="pearson") #

cor.test(MFV$NL,MFV$LA, method="pearson") #
cor.test(MFV$NL,MFV$FW, method="pearson") #
cor.test(MFV$NL,MFV$DW, method="pearson") #
cor.test(MFV$NL,MFV$EL, method="pearson") #
cor.test(MFV$NL,MFV$CC, method="pearson") #
cor.test(MFV$NL,MFV$K_Na, method="pearson") #
cor.test(MFV$NL,MFV$PR, method="pearson") #

cor.test(MFV$LA,MFV$FW, method="pearson") #
cor.test(MFV$LA,MFV$DW, method="pearson") #
cor.test(MFV$LA,MFV$EL, method="pearson") #
cor.test(MFV$LA,MFV$CC, method="pearson") #
cor.test(MFV$LA,MFV$K_Na, method="pearson") #
cor.test(MFV$LA,MFV$PR, method="pearson") #

cor.test(MFV$FW,MFV$DW, method="pearson") #
cor.test(MFV$FW,MFV$EL, method="pearson") #
cor.test(MFV$FW,MFV$CC, method="pearson") #
cor.test(MFV$FW,MFV$K_Na, method="pearson") #
cor.test(MFV$FW,MFV$PR, method="pearson") #

cor.test(MFV$DW,MFV$EL, method="pearson") #
cor.test(MFV$DW,MFV$CC, method="pearson") #
cor.test(MFV$DW,MFV$K_Na, method="pearson") #
cor.test(MFV$DW,MFV$PR, method="pearson") #

cor.test(MFV$EL,MFV$CC, method="pearson") #
cor.test(MFV$EL,MFV$K_Na, method="pearson") #
cor.test(MFV$EL,MFV$PR, method="pearson") #

cor.test(MFV$CC,MFV$K_Na, method="pearson") #
cor.test(MFV$CC,MFV$PR, method="pearson") #

cor.test(MFV$K_Na,MFV$PR, method="pearson") #

#install.packages('Hmisc')
#install.packages('ggpubr')
#install.packages('tidyverse')
#install.packages('corrplot')
library(dplyr)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
library(ggpubr)
library(tidyverse)
library(Hmisc)
library(corrplot)




# Pearson correlation test on data table  
# variables used in the paper 
my_data <- select(table, Shoot_Length, Root_Length, Plant_Height, Number_Leaves, Leaf_Area, Fresh_Weight, Dry_Weight, Chlorophyll_Content, Electrolyte_Leakage, K_Na, Photsynthesis_Rate)
my_data <- na.omit(my_data)
# all the variables
all_data <- na.omit(table[4:23])

#get p-value of my_data
cor_1 <- rcorr(as.matrix(my_data))
cor_1_P <- as.data.frame(cor_1$P) # put p values into a data frame 
# get p-value of all_data
cor_2 <- rcorr(as.matrix(all_data))
cor_2_P <- as.data.frame(cor_2$P) # put p values into a data frame 


res1 <- cor.mtest(all_data, conf.level = .95)
M1 <-cor(all_data)
corrplot(M1, p.mat = res1$p, sig.level = .05, type = "lower")

res2 <- cor.mtest(my_data, conf.level = .95)
M2 <-cor(my_data)
corrplot(M2, p.mat = res1$p, sig.level = .05, type = "lower")

