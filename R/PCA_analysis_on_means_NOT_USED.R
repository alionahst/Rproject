###------PCA on all of the variables------###

#doing the PCA, adding the scaling into the function
all_salt.pca <- prcomp(na.omit(table[c(4:10, 12:19, 21:22)]), scale. = TRUE)
#results of the PCA
all_salt.pca$sdev
summary(all_salt.pca)
#check, if the sums equal to quantity of observations (=17)
sum((all_salt.pca$sdev)^2)
#plotting the results in order to evaluate the PCs
all_plot <- screeplot(all_salt.pca,
                      npcs = length(all_salt.pca$sdev),
                      type = "lines")
#plotting the results of the PCA
library(ggplot2)
#install.packages("ggfortify")
library(ggfortify)
#making a scatterplot of PC1 and PC2
#creating data.frame for data labels in future plot
all_labels <- data.frame(table[-c(13, 14, 31, 32), c(3, 23)])
row.names(all_labels) <- paste0(as.vector(table$Code[-c(13, 14, 31, 32)]),
                                rep(c(" T", " C"), times = 23)) 
#creating the plot
all_pc12 <- autoplot(all_salt.pca, loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE, loadings.label.size = 3,                #plotting the variables
                 data = all_labels, colour = 'Treat_Contr', 
                 shape = FALSE, label.size = 3,              #plotting the accessions
                 frame = TRUE)                                                  #plotting the frame (to see if there are any groups)
all_pc12 + ggtitle("Scatterplot of PC1 and PC2")                            #adding the title

#putting absolute values of the loading scores from PCA1 into a vector
all_loading_scores <- abs(all_salt.pca$rotation[,1])
#ranking the variables
all_var_scores_ranked <- sort(all_loading_scores)
print(all_var_scores_ranked)
# We used 8 highest ranked variables to include in the final MFV-based ranking
# of salt tolerance accessions