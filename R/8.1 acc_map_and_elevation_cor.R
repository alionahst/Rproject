# Correlation between mean MFV (of 8 important ) and elevation
Elev_Cor <- as.data.frame(MFV$Mean)
Elev_Cor$Elevation <- Acc_loc$Elevation

cor_1 <- rcorr(as.matrix(test))
cor_1$r[2,1]


# maps