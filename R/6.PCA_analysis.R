####------PCA on MFV-------###
install.packages("factoextra")
library(ggplot2)
library(factoextra)

#doing the PCA, adding the scaling into the function
MFV_salt.pca <- prcomp((na.omit(MFV[-c(8, 11:14, 17, 20)])), 
                       scale. = TRUE)
#results of the PCA
#summary(MFV_salt.pca) - only needed for information to write r markdown

#plotting the results in order to evaluate the principal components
fviz_eig(MFV_salt.pca,
         title = "Variance contribution of principal components",
         xlab = "Principal components")

#plotting the results of the PCA
#making a scatterplot of PC1 and PC2
fviz_pca_ind(MFV_salt.pca,
             repel = TRUE, # Avoid text overlapping
             col.ind = "cos2",  # Individuals color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             #title = "Variance contribution of principal components",
             #xlab = "Principal component 1 (34.4%)",
             #ylab = "Principal component 2 (18.9%)",
)
fviz_pca_biplot(MFV_salt.pca,
             repel = TRUE, # Avoid text overlapping
             col.var = "contrib", # Variables color
             col.ind = "black",  # Individuals color by the quality of representation
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Variance contribution of principal components",
             xlab = "PCA 1 (34.4%)",
             ylab = "PCA 2 (18.9%)"
)
fviz_pca_var(MFV_salt.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             legend.title = "Groups"
)

fviz_pca_biplot(MFV_salt.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_pca_ind(MFV_salt.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
            
             legend.title = "Groups",
             repel = TRUE
)

# Contributions of variables to PC1
fviz_contrib(MFV_salt.pca, choice = "var", axes = 1, top = 13,
             sortcontrib = "asc",
             title = "Contribution of variables to PC1")
# Contributions of variables to PC2
fviz_contrib(MFV_salt.pca, choice = "var", axes = 2, top = 13,
             title = "Contribution of variables to PC2")



#creating data.frame for data labels in future plot
MFV_labels <- data.frame(Acc_loc$Elevation[-c(7, 16)])
row.names(MFV_labels) <- paste0("Ess-", c(1:6, 8:15, 17:25))
#creating the plot
MFV_pc12 <- autoplot(MFV_salt.pca, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5,                #plotting the variables
         data = MFV_labels, shape = FALSE, label.size = 3,              #plotting the accessions
         frame = TRUE)                                                  #plotting the frame (to see if there are any groups)
MFV_pc12 + ggtitle("Scatterplot of PC1 and PC2")                            #adding the title

#putting absolute values of the loading scores from PCA1 into a vector
MFV_loading_scores <- abs(MFV_salt.pca$rotation[,1])
#ranking the variables
MFV_var_scores_ranked <- sort(MFV_loading_scores, decreasing = TRUE)
print(MFV_var_scores_ranked)
# We used 8 highest ranked variables to include in the final MFV-based ranking
# of salt tolerance accessions
