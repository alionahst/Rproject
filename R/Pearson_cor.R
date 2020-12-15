

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

