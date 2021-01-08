install.packages("ggpubr")
library(ggpubr)
#MORPHOLOGICAL TRAITS SHAPIRO AND KRUSKAL 

spm <-  numeric()
nor_spm <- c()
for(i in names(Morpho_t[4:8])) { 
  an1 <- aov(Morpho_t[, i] ~ Morpho_t$Treat_Contr)
  spm[i] <- shapiro.test(an1$residuals)$p.value
  nor_spm <- c(nor_spm, ifelse (spm[i] <= 0.05, "False", "True"))
}
spm_table <- data.frame ("P-value" = spm, "Normality" = nor_spm)

#for (i in names(Morpho_t[4:8])) { 
 # boxplot(Morpho_t[, i] ~ Morpho_t$Treat_Contr, 
  #        ylab = names(Morpho_t[i]), 
   #       xlab = names(Morpho_t[3])
#  )
 # print(kruskal.test(Morpho_t[, i] ~ Morpho_t$Treat_Contr))
#}

for (i in names(Morpho_t[4:8])) {
  bmorpho <- ggboxplot(Morpho_t, 
                 x = "Treat_Contr",
                 y = names(Morpho_t[i]), 
                 color = names(Morpho_t[3]),
                 palette = c("blue", "red"),
                 add = "jitter") + 
    stat_compare_means(data = Morpho_t, method = "kruskal.test", label.x.npc = "center",
                       label.y.npc = "top")
  print(bmorpho)
} 


#Biomasse 

spb <- numeric()
nor_spb <- c()
for(i in names(Weight_ion[4:5])) { 
  an2 <- aov(Weight_ion[, i] ~ Weight_ion$Treat_Contr)
  spb[i] <- shapiro.test(an2$residuals)$p.value
  nor_spb <- c(nor_spb, ifelse (spb[i] <= 0.05, "False", "True"))
}
spb_table <- data.frame ("P-value" = spb, "Normality" = nor_spb)


#for (i in names(Weight_ion[4:5])) { 
 # boxplot(Weight_ion[, i] ~ Weight_ion$Treat_Contr, 
 #         ylab = names(Weight_ion[i]), 
  #        xlab = names(Weight_ion[3])
 # )
 # print(kruskal.test(Weight_ion[, i] ~ Weight_ion$Treat_Contr))
#}
for (i in names(Weight_ion[4:5])) {
  bwei <- ggboxplot(Weight_ion, 
                 x = "Treat_Contr",
                 y = names(Weight_ion[i]), 
                 color = names(Weight_ion[3]),
                 palette = c("blue", "red"),
                 add = "jitter") + 
    stat_compare_means(data = Weight_ion, method = "kruskal.test", label.x.npc = "center",
                       label.y.npc = "top")
  print(bwei)
} 


###  RWC, EL and chlorophyl content

spe <- numeric()
nor_spe <- c()
for(i in names(Weight_ion[c(6,12)])) { 
  an3 <- aov(Weight_ion[, i] ~ Weight_ion$Treat_Contr)
  spe[i] <- shapiro.test(an3$residuals)$p.value
  nor_spe <- c(nor_spe, ifelse (spe[i] <= 0.05, "False", "True"))
}
spe_table <- data.frame ("P-value" = spe, "Normality" = nor_spe)


#for (i in names(Weight_ion[c(6,12)])) { 
 # boxplot(Weight_ion[, i] ~ Weight_ion$Treat_Contr, 
  #        ylab = names(Weight_ion[i]), 
   #       xlab = names(Weight_ion[3])
#  )
 # print(kruskal.test(Weight_ion[, i] ~ Weight_ion$Treat_Contr))
#}

for (i in names(Weight_ion[c(6,12)])) {
  bwei2 <- ggboxplot(Weight_ion, 
                 x = "Treat_Contr",
                 y = names(Weight_ion[i]), 
                 color = names(Weight_ion[3]),
                 palette = c("blue", "red"),
                 add = "jitter") + 
    stat_compare_means(data = Weight_ion, method = "kruskal.test", label.x.npc = "center",
                       label.y.npc = "top")
  print(bwei2)
} 

spc <- numeric()
nor_spc <- c()
for(i in names(Chloro_c[4])) { 
  an4 <- aov(Chloro_c[, i] ~ Chloro_c$Treat_Contr)
  spc[i] <- shapiro.test(an4$residuals)$p.value
  nor_spc <- c(nor_spc, ifelse (spc[i] <= 0.05, "False", "True"))
}
spc_table <- data.frame ("P-value" = spc, "Normality" = nor_spc)

#for (i in names(Chloro_c[4])) { 
 # boxplot(Chloro_c[, i] ~ Chloro_c$Treat_Contr, 
  #        ylab = names(Chloro_c[i]), 
   #       xlab = names(Chloro_c[3])
  #)
  #print(kruskal.test(Chloro_c[, i] ~ Chloro_c$Treat_Contr))
#}

for (i in names(Chloro_c[4])) {
  bchlo <- ggboxplot(Chloro_c, 
                 x = "Treat_Contr",
                 y = names(Chloro_c[i]), 
                 color = names(Chloro_c[3]),
                 palette = c("blue", "red"),
                 add = "jitter") + 
    stat_compare_means(data = Chloro_c, method = "kruskal.test", label.x.npc = "center",
                       label.y.npc = "top")
  print(bchlo)
} 


#### Mineral ion content

spw <- numeric()
nor_spw <- c()
for(i in names(Weight_ion[7:11])) { 
  an5 <- aov(Weight_ion[, i] ~ Weight_ion$Treat_Contr)
  spw[i] <- shapiro.test(an5$residuals)$p.value
  nor_spw <- c(nor_spw, ifelse (spw[i] <= 0.05, "False", "True"))
}
spw_table <- data.frame ("P-value" = spw, "Normality" = nor_spw)

#for (i in names(Weight_ion[7:11])) { 
#  boxplot(Weight_ion[, i] ~ Weight_ion$Treat_Contr, 
#          ylab = names(Weight_ion[i]), 
 #         xlab = names(Weight_ion[3])
 # )
  #print(kruskal.test(Weight_ion[, i] ~ Weight_ion$Treat_Contr))
#}

for (i in names(Weight_ion[7:11])) {
  bwei3 <- ggboxplot(Weight_ion, 
                 x = "Treat_Contr",
                 y = names(Weight_ion[i]), 
                 color = names(Weight_ion[3]),
                 palette = c("blue", "red"),
                 add = "jitter") + 
    stat_compare_means(data = Weight_ion, method = "kruskal.test", label.x.npc = "center",
                       label.y.npc = "top")
  print(bwei3)
} 

## Gas parameters

spg <- numeric()
nor_spg <- c()
for (i in names(Gas_e[c(4,5,6:7)])) { 
  an6 <- aov(Gas_e[, i] ~ Gas_e$Treat_Contr)
  spg[i] <- shapiro.test(an6$residuals)$p.value
  nor_spg <- c(nor_spg, ifelse (spg[i] <= 0.05, "False", "True"))
}

spg_table <- data.frame ("P-value" = spg, "Normality" = nor_spg)

a17 <- aov(Intercellular_CO2 ~ Treat_Contr, data = Gas_e)
shapiro.test(a17$residuals)
# NORMALITYYYYY
#ANOVAAAA
summary(a17)
a17_aov <- agricolae::HSD.test(a17, "Treat_Contr")


banova <- ggboxplot(Gas_e,
          x="Treat_Contr", 
          y= "Intercellular_CO2",
          color = names(Gas_e[3]),
          palette = c("blue", "red"),
          add = "jitter") 
banova  +   stat_compare_means(method = "anova")  

#kruskal.test(Intercellular_CO2 ~ Treat_Contr, data = Gas_e)
# No statistical difference


#for (i in names(Gas_e[4:7])) { 
 # boxplot(Gas_e[, i] ~ Gas_e$Treat_Contr, 
  #        ylab = names(Gas_e[i]), 
  #        xlab = names(Gas_e[3])
  #)
  #print(kruskal.test(Gas_e[, i] ~ Gas_e$Treat_Contr))
#}

for (i in names(Gas_e[4,6:7])) {
  bgas <- ggboxplot(Gas_e, 
                 x = "Treat_Contr",
                 y = names(Gas_e[i]), 
                 color = names(Gas_e[3]),
                 palette = c("blue", "red"),
                 add = "jitter") + 
    stat_compare_means(data = Gas_e, method = "kruskal.test", label.x.npc = "center",
                       label.y.npc = "top")
  print(bgas)
} 


