## ---- PearsonCor

# Pearson correlation test on all the variables

#install.packages('Hmisc')
#install.packages('corrplot')
library(Hmisc)
library(corrplot)

#create the table that omit NA with all variables (not taking the first 3 columns)
all_data <- na.omit(table[4:23])

# get p-value of all_data
cor_1 <- rcorr(as.matrix(all_data))
cor_1_P <- as.data.frame(cor_1$P) # put p values into a data frame 

# make the table
res1 <- cor.mtest(all_data, conf.level = .95)
M1 <-cor(all_data)
corrplot(M1, p.mat = res1$p, sig.level = .05, type = "lower")



# Correlation between mean MFV (of 8 important ) and elevation
Elev_Cor <- as.data.frame(MFV$Mean)
Elev_Cor$Elevation <- Acc_loc$Elevation

cor_1 <- rcorr(as.matrix(test))
cor_1$r[2,1]