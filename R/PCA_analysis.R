#standardizing the variables inside the table:
stand_table <- as.data.frame(scale(table[4:23]), na.rm = TRUE)
stand_table <- as.data.frame(scale(na.fail(table[4:23])))

salt.pca1 <- princomp(stand_table)


summary(salt.pca1)

sum((salt.pca1$sdev)^2)

screeplot(salt.pca1, type="lines")
install.packages("factoextra")
library(factoextra)
fviz_eig(salt.pca1)
#Another way of deciding how many components to retain is
#to use Kaiser’s criterion: that we should only retain
#principal components for which the variance is above 1
#(when principal component analysis was applied to
#standardised data). We can check this by finding the
#variance of each of the principal components:
summary(salt.pca1)
PC1
print(su1)
(salt.pca1$sdev)^2

#A third way to decide how many principal components to retain
#is to decide to keep the number of components required to explain at least
#some minimum amount of the total variance. For example, if it is important
#to explain at least 80% of the variance, we would retain the first five
#principal components, as we can see from the output of “summary(wine.pca)”
#that the first five principal components explain 80.2% of the variance
#(while the first four components explain just 73.6%, so are not sufficient).

salt.pca1$rotation[,1]

sum((salt.pca1$rotation[,1])^2)


plot(salt.pca1$x[,1],salt.pca1$x[,2]) # make a scatterplot
text(salt.pca1$x[,1],salt.pca1$x[,2], 
     table$Treat_Contr, cex=0.7, pos=4, col="red")

plot(salt.pca1$x[,1],salt.pca1$x[,3]) # make a scatterplot
text(salt.pca1$x[,1],salt.pca1$x[,3],
     table$Treat_Contr, cex=0.7, pos=4, col="red")

plot(salt.pca1$x[,1],salt.pca1$x[,4]) # make a scatterplot
text(salt.pca1$x[,1],salt.pca1$x[,4],
     table$Treat_Contr, cex=0.7, pos=4, col="red")

plot(salt.pca1$x[,1],salt.pca1$x[,5]) # make a scatterplot
text(salt.pca1$x[,1],salt.pca1$x[,5],
     table$Treat_Contr, cex=0.7, pos=4, col="red")

plot(salt.pca1$x[,1],salt.pca1$x[,6]) # make a scatterplot
text(salt.pca1$x[,1],salt.pca1$x[,6],
     table$Treat_Contr, cex=0.7, pos=4, col="red")

plot(salt.pca1$x[,1],salt.pca1$x[,7]) # make a scatterplot
text(salt.pca1$x[,1],salt.pca1$x[,7],
     table$Treat_Contr, cex=0.7, pos=4, col="red")

plot(salt.pca1$x[,1],salt.pca1$x[,8]) # make a scatterplot
text(salt.pca1$x[,1],salt.pca1$x[,8],
     table$Treat_Contr, cex=0.7, pos=4, col="red")
