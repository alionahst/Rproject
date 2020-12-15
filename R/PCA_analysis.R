###------PCA on all of the variables------###
#doing the PCA, adding the scaling into the function
all_salt.pca <- prcomp((na.omit(table[c(4:22)])), scale. = TRUE)
#results of the PCA
all_salt.pca$sdev
summary(all_salt.pca)
#check, if the sums equal to quantity of observations (=20)
sum((all_salt.pca$sdev)^2)
#plotting the results in order to evaluate the PCs
plot <- screeplot(all_salt.pca, type = "lines")
#another variant of the plot
#install.packages("factoextra")
library(factoextra)
fviz_eig(all_salt.pca)
#make a scatterplot of PC1 and PC2
plot(all_salt.pca$x[,1],all_salt.pca$x[,2])
text(all_salt.pca$x[,1],all_salt.pca$x[,2], 
     table$Code[-c(14, 15, 32, 33)], cex=0.7, pos=4, col=c(1:2))
pca.var <- round(all_salt.pca$sdev^2/sum(all_salt.pca$sdev^2)*100, 1)
barplot(pca.var)

library(ggplot2)
pca.data <- data.frame(data_labels = table$Code[-c(13, 14, 31, 32)],
                       color_labels = table$Treat_Contr[-c(13, 14, 31, 32)],
                       X_axis = all_salt.pca$x[,1],
                       Y_axis = all_salt.pca$x[,2])
dev.off()
ggplot(data = pca.data, aes(x = X_axis, y = Y_axis, label = data_labels, color = color_labels ))+
        geom_text()+
        xlab(paste("PC1 - ", pca.var[1], "%", sep=""))+
        ylab(paste("PC2 - ", pca.var[2], "%", sep=""))+
        theme_bw()+
        ggtitle("PCA graph")

all_salt.pca$rotation

loading_scores <- data.frame(abs(all_salt.pca$rotation[, c(1:10)]))
                             
#var_scores_PCA1 <- abs(loading_scores)
#var_scores_ranked <- sort(var_scores)
#names_ranked <- names(var_scores_ranked)



#Another way of deciding how many components to retain is
#to use Kaiser’s criterion: that we should only retain
#principal components for which the variance is above 1
#(when principal component analysis was applied to
#standardised data). We can check this by finding the
#variance of each of the principal components:
#(all_salt.pca$sdev)^2

#A third way to decide how many principal components to retain
#is to decide to keep the number of components required to explain at least
#some minimum amount of the total variance. For example, if it is important
#to explain at least 80% of the variance, we would retain the first five
#principal components, as we can see from the output of “summary(wine.pca)”
#that the first five principal components explain 80.2% of the variance
#(while the first four components explain just 73.6%, so are not sufficient).

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
#another variant of the plot
#install.packages("factoextra")
library(factoextra)
fviz_eig(MFV_salt.pca)
#make a scatterplot of PC1 and PC2
plot(MFV_salt.pca$x[,1],MFV_salt.pca$x[,2])
text(MFV_salt.pca$x[,1],MFV_salt.pca$x[,2], 
     table$Code[-c(14, 15, 32, 33)], cex=0.7, pos=4, col=c(1:2))
MFV_pca.var <- round(MFV_salt.pca$sdev^2/sum(MFV_salt.pca$sdev^2)*100, 1)
barplot(pca.var)

library(ggplot2)
MFV_pca.data <- data.frame(data_labels = table$Code[-c(13, 14, 31, 32)],
                       color_labels = rep(c(1:23), each = 2),
                       X_axis = MFV_salt.pca$x[,1],
                       Y_axis = MFV_salt.pca$x[,2])
dev.off()
ggplot(data = pca.data, aes(x = X_axis, y = Y_axis, label = data_labels, color = color_labels ))+
        geom_text()+
        xlab(paste("PC1 - ", pca.var[1], "%", sep=""))+
        ylab(paste("PC2 - ", pca.var[2], "%", sep=""))+
        theme_bw()+
        ggtitle("MFV PCA graph")

MFV_salt.pca$rotation

MFV_loading_scores <- abs(MFV_salt.pca$rotation[,1])

MFV_var_scores_ranked <- sort(MFV_loading_scores)
print(MFV_var_scores_ranked)

# We used 8 highest ranked variables to include in the final MFV based ranking
# of salt tolerance
