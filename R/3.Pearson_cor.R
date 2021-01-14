## ---- PearsonCor

# Pearson correlation test on all the variables

#install.packages('Hmisc')
#install.packages('corrplot')
library(Hmisc)
library(corrplot)

# create a table with all the variables but without 3 first columns
all_data <- na.omit(table[4:23])
#perform Pearson correlation test
res1 <- cor.mtest(all_data, conf.level = .95)
#Plot the results
M1 <-cor(all_data)
corrplot(M1, p.mat = res1$p, sig.level = .05, type = "lower", tl.srt=25)

# get p-value of all_data
#cor_1 <- rcorr(as.matrix(all_data)) #not used
#cor_1_P <- as.data.frame(cor_1$P) # put p values into a data frame