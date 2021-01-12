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