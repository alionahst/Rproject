#salt tolerance index for morphological traits

x <- data.frame(dplyr::filter(table, table$Treat_Contr=="Treatment"))
x <- x[4:8]
y <- data.frame(dplyr::filter(table, table$Treat_Contr=="Control"))
y <- y[4:8]

STI_Shoot_Length <- x[1]/y[1]
STI_Root_Length <- x[2]/y[2]
STI_Plant_Height <- x[3]/y[3]
STI_Number_Leaves <- x[4]/y[4]
STI_Leaf_Area <- x[5]/y[5]
STI <- data.frame(STI_Shoot_Length,STI_Root_Length, STI_Plant_Height, STI_Number_Leaves, STI_Leaf_Area)

#trying to make a for loop
#STI1 <- data.frame()
#vecteur <- c()
#for (i in names(x)){
#  for (j in 1:4){
#    name <- paste("STI", i ,sep = "_" )
#    name <- x[j]/x[j]
#    vecteur <- c(vecteur, name)}}


#Membership function value for morphological trait 

MFV <- SL<- RL <- PH <- NL<- LA<- c()

MFV <- STI %>%
  mutate(SL= (STI$Shoot_Length - min(STI$Shoot_Length))/(max(STI$Shoot_Length)- min(STI$Shoot_Length)),
         RL=(STI$Root_Length - min(STI$Root_Length))/(max(STI$Root_Length)- min(STI$Root_Length)),
         PH=(STI$Plant_Height - min(STI$Plant_Height))/(max(STI$Plant_Height)- min(STI$Plant_Height)),
         NL=(STI$Number_Leaves - min(STI$Number_Leaves))/(max(STI$Number_Leaves)- min(STI$Number_Leaves)),
         LA=(STI$Leaf_Area - min(STI$Leaf_Area))/(max(STI$Leaf_Area)- min(STI$Leaf_Area)))%>%
  select(SL, RL, PH,NL, LA)


