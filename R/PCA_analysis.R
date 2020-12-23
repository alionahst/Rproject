####------PCA on MFV-------###

#doing the PCA, adding the scaling into the function
MFV_salt.pca <- prcomp((na.omit(MFV[-c(8, 17, 20)])), scale. = TRUE)
#results of the PCA
MFV_salt.pca$sdev
summary(MFV_salt.pca)
#check, if the sums equal to quantity of observations (=17)
sum((MFV_salt.pca$sdev)^2)
#plotting the results in order to evaluate the principal components
MFV_plot <- screeplot(MFV_salt.pca,
                      npcs = length(MFV_salt.pca$sdev),
                      type = "lines")
#plotting the results of the PCA
library(ggplot2)
#install.packages("ggfortify")
library(ggfortify)
#making a scatterplot of PC1 and PC2
#creating data.frame for data labels in future plot
MFV_labels <- data.frame(Acc_loc$Elevation[-c(7, 16)])
row.names(MFV_labels) <- paste0("Ess-", c(1:6, 8:15, 17:25))
#creating the plot
pc12 <- autoplot(MFV_salt.pca, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5,                #plotting the variables
         data = MFV_labels, shape = FALSE, label.size = 3,              #plotting the accessions
         frame = TRUE)                                                  #plotting the frame (to see if there are any groups)
pc12 + ggtitle("Scatterplot of PC1 and PC2")                            #adding the title

#putting absolute values of the loading scores from PCA1 into a vector
MFV_loading_scores <- abs(MFV_salt.pca$rotation[,1])
#ranking the variables
MFV_var_scores_ranked <- sort(MFV_loading_scores)
print(MFV_var_scores_ranked)
# We used 8 highest ranked variables to include in the final MFV-based ranking
# of salt tolerance accessions
