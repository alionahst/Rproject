####------PCA on MFV-------###
#install.packages("factoextra")
library(ggplot2)
library(factoextra)

#doing the PCA, adding the scaling into the function
MFV_salt <- na.omit(MFV[-c(8, 11:14, 17, 20)])
MFV_salt.pca <- prcomp(MFV_salt, 
                       scale. = TRUE)

#plotting the results in order to evaluate the principal components
PCA_eval <- fviz_eig(MFV_salt.pca,
                     title = "Variance contribution of principal components",
                     xlab = "Principal components")

#creating vector for colors to correspond to ranking of accessions
colors_PCA <- c(3:1, 3, 2, 2, 2, 3, 2, 3, 3, 3:1, 1, 1, 3, rep(2, 6))

#making a scatterplot of PC1 and PC2
PCA_ind <- fviz_pca_ind(MFV_salt.pca,
                        repel = TRUE, # Avoid text overlapping
                        geom = c("text", "point"),
                        labelsize = 5,
                        pointsize = 2,
                        habillage = colors_PCA,
                        addEllipses = TRUE,
                        ellipse.level = 0.95,
                        palette = c("#FF3333", "#009966", "#3366CC"),
                        title = "PCA scatterplot")

PCA_var <- fviz_pca_var(MFV_salt.pca,
                        col.var = "#2E9FDF",
                        repel = TRUE,
                        title = "PCA loadings")

#Contributions of variables to PC1
PCA_contr <- fviz_contrib(MFV_salt.pca, choice = "var", axes = 1, top = 13,
                          sortcontrib = "asc",
                          title = "Contribution of variables to PC1")