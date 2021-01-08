#install.packages("ggpubr")
#install.packages("dplyr")
library(ggpubr)
library(dplyr)

#MORPHOLOGICAL TRAITS 

# Shapiro test
spm <-  numeric()
nor_spm <- c()
for(i in names(Morpho_t[4:8])) { 
  an1 <- aov(Morpho_t[, i] ~ Morpho_t$Treat_Contr)
  spm[i] <- shapiro.test(an1$residuals)$p.value
  nor_spm <- c(nor_spm, ifelse (spm[i] <= 0.05, "False", "True"))
}
spm_table <- data.frame ("P-value" = spm, "Normality" = nor_spm)

# boxplot and statistical differences
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

# Shapiro test
spb <- numeric()
nor_spb <- c()
for(i in names(Weight_ion[4:5])) { 
  an2 <- aov(Weight_ion[, i] ~ Weight_ion$Treat_Contr)
  spb[i] <- shapiro.test(an2$residuals)$p.value
  nor_spb <- c(nor_spb, ifelse (spb[i] <= 0.05, "False", "True"))
}
spb_table <- data.frame ("P-value" = spb, "Normality" = nor_spb)

# boxplot and statistical differences
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
# Shapiro test
spe <- numeric()
nor_spe <- c()
for(i in names(Weight_ion[c(6,12)])) { 
  an3 <- aov(Weight_ion[, i] ~ Weight_ion$Treat_Contr)
  spe[i] <- shapiro.test(an3$residuals)$p.value
  nor_spe <- c(nor_spe, ifelse (spe[i] <= 0.05, "False", "True"))
}
spe_table <- data.frame ("P-value" = spe, "Normality" = nor_spe)

nor_spc <- c()
an4 <- aov(Chloro_c$Chlorophyll_Content ~ Chloro_c$Treat_Contr)
spc <- shapiro.test(an4$residuals)$p.value
nor_spc <- c(nor_spc, ifelse (spc <= 0.05, "False", "True"))

spc_table <- data.frame ("P-value" = spc, "Normality" = nor_spc)
row.names(spc_table) <- "Chlorophyll_Content"

spec_table <- dplyr::bind_rows(spe_table, spc_table)


# boxplot and statistical differences
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

bchlo <- ggboxplot(Chloro_c, 
                 x = "Treat_Contr",
                 y = "Chlorophyll_Content", 
                 color = names(Chloro_c[3]),
                 palette = c("blue", "red"),
                 add = "jitter") 
bchlo + stat_compare_means(data = Chloro_c, method = "kruskal.test", label.x.npc = "center",
                       label.y.npc = "top")


#### Mineral ion content
# Shapiro test
spw <- numeric()
nor_spw <- c()
for(i in names(Weight_ion[7:11])) { 
  an5 <- aov(Weight_ion[, i] ~ Weight_ion$Treat_Contr)
  spw[i] <- shapiro.test(an5$residuals)$p.value
  nor_spw <- c(nor_spw, ifelse (spw[i] <= 0.05, "False", "True"))
}
spw_table <- data.frame ("P-value" = spw, "Normality" = nor_spw)

# boxplot and statistical differences
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
# Shapiro test
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

# boxplot and statistical differences
banova <- ggboxplot(Gas_e_withoutNA,
          x="Treat_Contr", 
          y= "Intercellular_CO2",
          color = names(Gas_e_withoutNA[3]),
          palette = c("blue", "red"),
          add = "jitter") 
banova  +   stat_compare_means(method = "anova")  


Gas_e_withoutNA <- na.omit(Gas_e)
for (i in names(Gas_e_withoutNA[c(4,6:7)])) {
  bgas <- ggboxplot(Gas_e_withoutNA, 
                 x = "Treat_Contr",
                 y = names(Gas_e_withoutNA[i]), 
                 color = names(Gas_e_withoutNA[3]),
                 palette = c("blue", "red"),
                 add = "jitter") + 
    stat_compare_means(data = Gas_e_withoutNA, method = "kruskal.test", label.x.npc = "center",
                       label.y.npc = "top")
  print(bgas)
} 


