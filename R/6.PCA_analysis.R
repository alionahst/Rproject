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
print(PCA_eval)

#creating vector for colors to correspond to ranking of accessions
colors_PCA <- c("3. Tolerant", "2. Moderately tolerant", "1. Sensitive",
                "3. Tolerant", "2. Moderately tolerant", "2. Moderately tolerant",
                "2. Moderately tolerant", "3. Tolerant", "2. Moderately tolerant",
                "3. Tolerant", "3. Tolerant", "3. Tolerant", "2. Moderately tolerant",
                "1. Sensitive", "1. Sensitive", "1. Sensitive", "3. Tolerant",
                "2. Moderately tolerant", "2. Moderately tolerant",
                "2. Moderately tolerant", "2. Moderately tolerant",
                "2. Moderately tolerant", "2. Moderately tolerant")

#making a scatterplot of PC1 and PC2
PCA_ind <- fviz_pca_ind(MFV_salt.pca,
                        repel = TRUE, # Avoid text overlapping
                        labelsize = 5,
                        pointsize = 2,
                        habillage = colors_PCA,
                        addEllipses = TRUE,
                        ellipse.level = 0.95,
                        mean.point = FALSE,
                        palette = c("#FF3333", "#009966", "#3366CC"),
                        title = "PCA score plot")
print(PCA_ind)

#making loadings plot
PCA_var <- fviz_pca_var(MFV_salt.pca,
                        col.var = "#2E9FDF",
                        repel = TRUE,
                        title = "PCA loadings")
print(PCA_var)

#Contributions of variables to PC1
PCA_contr <- fviz_contrib(MFV_salt.pca,
                          choice = "var",
                          axes = 1,
                          top = 13,
                          sortcontrib = "asc",
                          title = "Contribution of variables to PC1")
print(PCA_contr)