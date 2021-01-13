x <- data.frame(dplyr::filter(table, table$Treat_Contr=="Treatment"))
x <- x[-(1:3)]
y <- data.frame(dplyr::filter(table, table$Treat_Contr=="Control"))
y <- y[-(1:3)]
#salt tolerance index for morphological traits
STI_SL <- x[1]/y[1]
STI_RL <- x[2]/y[2]
STI_PH <- x[3]/y[3]
STI_LN <- x[4]/y[4]
STI_LA <- x[5]/y[5]
#salt tolerance index for biomass
STI_FW <- x[6]/y[6]
STI_DW <- x[7]/y[7]
#salt tolerance index for RWC, EL and Chlorophyll content
STI_RWC  <- x[8]/y[8]
STI_EL  <- x[14]/y[14]
STI_CC <- x[15]/y[15]
#salt tolerance index for Mineral ion content
STI_Na <- x[9]/y[9]
STI_K <- x[10]/y[10]
STI_Ca <- x[11]/y[11]
STI_Mg <- x[12]/y[12]
STI_K_Na <- x[13]/y[13]
# salt tolerance index for gas exchange attributes
STI_PR <- x[16]/y[16]
STI_ICO2 <- x[17]/y[17]
STI_TR <- x[18]/y[18]
STI_SC <- x[19]/y[19]
#Creating a summary data.frame
STI <- data.frame(STI_SL,
                  STI_RL, 
                  STI_PH, 
                  STI_LN, 
                  STI_LA, 
                  STI_FW, 
                  STI_DW,
                  STI_RWC, 
                  STI_EL,
                  STI_Na,
                  STI_K,
                  STI_Ca,
                  STI_Mg,
                  STI_K_Na,
                  STI_CC,
                  STI_PR,
                  STI_ICO2,
                  STI_TR,
                  STI_SC)
#setting names of columns to compact the table view
STI_names <- c("STI_SL", "STI_RL", "STI_PH", "STI_LN", "STI_LA", "STI_FW", "STI_DW", "STI_RWC", "STI_EL", "STI_Na", "STI_K",
               "STI_Ca", "STI_Mg", "STI_K_Na", "STI_CC", "STI_PR", "STI_ICO2", "STI_TR", "STI_SC")
names(STI) <- STI_names