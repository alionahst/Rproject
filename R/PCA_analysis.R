####------PCA on MFV-------###

#doing the PCA, adding the scaling into the function
MFV_salt.pca <- prcomp((na.omit(MFV[-c(8, 17, 20)])), scale. = TRUE)
#results of the PCA
MFV_salt.pca$sdev
summary(MFV_salt.pca)
#check, if the sums equal to quantity of observations (=20)
sum((MFV_salt.pca$sdev)^2)
#plotting the results in order to evaluate the PCs
MFV_plot <- screeplot(MFV_salt.pca, type = "lines")
autoplot(MFV_salt.pca, loadings = TRUE)
#another variant of the plot
#install.packages("factoextra")
library(factoextra)
fviz_eig(MFV_salt.pca)
#make a scatterplot of PC1 and PC2
plot(MFV_salt.pca$x[,1],MFV_salt.pca$x[,2])
text(MFV_salt.pca$x[,1],MFV_salt.pca$x[,2], 
     table$Code[-c(14, 15, 32, 33)], cex=0.7, pos=4, col=c(1:2))
MFV_pca.var <- round(MFV_salt.pca$sdev^2/sum(MFV_salt.pca$sdev^2)*100, 1)
barplot(MFV_pca.var)
#plotting the results of the PCA
library(ggplot2)
#install.packages("ggfortify")
library(ggfortify)


MFV_pca.data <- data.frame(data_labels = table$Code[-c(13, 14, 31, 32)],
                       color_labels = rep(c(1:2), 23),
                       X_axis = as.vector(MFV_salt.pca$x[,1]),
                       Y_axis = as.vector(MFV_salt.pca$x[,2]))
dev.off()
ggplot(data = MFV_pca.data, aes(x = X_axis, y = Y_axis, label = data_labels, color = color_labels ))+
        geom_text()+
        xlab(paste("PC1 - ", MFV_pca.var[1], "%", sep=""))+
        ylab(paste("PC2 - ", MFV_pca.var[2], "%", sep=""))+
        theme_bw()+
        ggtitle("MFV PCA graph")

#putting absolute values of the loading scores from PCA1 into a vector
MFV_loading_scores <- abs(MFV_salt.pca$rotation[,1])
#ranking the variables
MFV_var_scores_ranked <- sort(MFV_loading_scores)
print(MFV_var_scores_ranked)
# We used 8 highest ranked variables to include in the final MFV-based ranking
# of salt tolerance accessions
