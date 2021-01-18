###----Normality and significance tests----###

##Required packages and libraries
#install.packages("ggpubr")
#install.packages("dplyr")
library(ggpubr)
library(dplyr)

##Morphological traits (shoot length, root length, plant height,
##number of leaves, leaf area)

#Performing Shapiro–Wilk test of normality
spm <- numeric()
nor_spm <- c()
for(i in names(Morpho_t[4:8])) { 
  an1 <- aov(Morpho_t[, i] ~ Morpho_t$Treat_Contr)
  spm[i] <- shapiro.test(an1$residuals)$p.value
  nor_spm <- c(nor_spm, ifelse (spm[i] <= 0.05, "False", "True"))
}
#Creating summary table with the results
spm_table <- data.frame ("P-value" = spm, "Normality" = nor_spm)

for (i in (4:8)) {
  bmorpho <- ggboxplot(Morpho_t,
                       x = "Treat_Contr",
                       y = names(Morpho_t[i]),
                       color = names(Morpho_t[3]),
                       palette = c("blue", "red"),
                       add = "jitter") +
    stat_compare_means(data = Morpho_t, method = "kruskal.test",
                       label.x.npc = "center",
                       label.y.npc = "top") +
    theme(legend.position = "None") +
    labs(x = "", title = paste("Boxplot of the", names(Morpho_t[i])),
         caption = paste("Figure 2.", (i-3), " Distribution of the mean values of the ", names(Morpho_t[i]), sep = ""))
  print(bmorpho)
}

##Normality test of biomass traits (fresh weight, dry weight)

#Performing Shapiro–Wilk test of normality
spb <- numeric()
nor_spb <- c()
for(i in names(Weight_ion[4:5])) { 
  an2 <- aov(Weight_ion[, i] ~ Weight_ion$Treat_Contr)
  spb[i] <- shapiro.test(an2$residuals)$p.value
  nor_spb <- c(nor_spb, ifelse (spb[i] <= 0.05, "False", "True"))
}

#Creating summary table with the results
spb_table <- data.frame ("P-value" = spb, "Normality" = nor_spb)

#Performing significance test and plotting the results and values distribution
for (i in (4:5)) {
  bwei <- ggboxplot(Weight_ion, 
                    x = "Treat_Contr",
                    y = names(Weight_ion[i]), 
                    color = names(Weight_ion[3]),
                    palette = c("blue", "red"),
                    add = "jitter") + 
    stat_compare_means(data = Weight_ion, method = "kruskal.test",
                       label.x.npc = "center",
                       label.y.npc = "top") +
    theme(legend.position = "None") +
    labs(x = "", title = paste("Boxplot of the", names(Weight_ion[i])),
         caption = paste("Figure 2.", (i+2), " Distribution of the mean values of the ", names(Weight_ion[i]), sep = ""))
  print(bwei)
} 

##Relative water content (RWC), electrolyte leakage (EL),
##chlorophyll content (SPAD or CC)

#Performing Shapiro–Wilk test of normality on the RWC and EL
spe <- numeric()
nor_spe <- c()
for(i in names(Weight_ion[c(6,12)])) { 
  an3 <- aov(Weight_ion[, i] ~ Weight_ion$Treat_Contr)
  spe[i] <- shapiro.test(an3$residuals)$p.value
  nor_spe <- c(nor_spe, ifelse (spe[i] <= 0.05, "False", "True"))
}

#Creating summary table with the results for RWC and EL
spe_table <- data.frame ("P-value" = spe, "Normality" = nor_spe)

#Performing Shapiro–Wilk test of normality on the CC
nor_spc <- c()
an4 <- aov(Chloro_c$Chlorophyll_Content ~ Chloro_c$Treat_Contr)
spc <- shapiro.test(an4$residuals)$p.value

#Creating summary table with the results for CC
nor_spc <- c(nor_spc, ifelse (spc <= 0.05, "False", "True"))
spc_table <- data.frame ("P-value" = spc, "Normality" = nor_spc)
row.names(spc_table) <- "Chlorophyll_Content"

#Uniting the summary tables with the results for RWC+EL and CC
spec_table <- dplyr::bind_rows(spe_table, spc_table)


#for RWC and EL
for (i in (c(6,12))) {
  bwei2 <- ggboxplot(Weight_ion, 
                     x = "Treat_Contr",
                     y = names(Weight_ion[i]), 
                     color = names(Weight_ion[3]),
                     palette = c("blue", "red"),
                     add = "jitter") + 
    stat_compare_means(data = Weight_ion, method = "kruskal.test",
                       label.x.npc = "center",
                       label.y.npc = "top") +
    theme(legend.position = "None") +
    labs(x = "", title = paste("Boxplot of the", names(Weight_ion[i])),
         caption = paste("Figure 2.", ifelse((i<7), paste(i+2), paste(i-3)), " Distribution of the mean values of the ", names(Weight_ion[i]), sep = ""))
  print(bwei2)
} 
#for CC
bchlo <- ggboxplot(Chloro_c, 
                   x = "Treat_Contr",
                   y = "Chlorophyll_Content", 
                   color = names(Chloro_c[3]),
                   palette = c("blue", "red"),
                   add = "jitter") 
bchlo + stat_compare_means(data = Chloro_c, method = "kruskal.test", 
                           label.x.npc = "center",
                           label.y.npc = "top") +
  theme(legend.position = "None") +
  labs(x = "", title = paste("Boxplot of the", names(Chloro_c[4])),
       caption = paste("Figure 2.10 Distribution of the mean values of the", names(Chloro_c[4])))

##Mineral ion content (Na, K, Ca, Mg, K/Na)

#Performing Shapiro–Wilk test of normality
spw <- numeric()
nor_spw <- c()
for(i in names(Weight_ion[7:11])) { 
  an5 <- aov(Weight_ion[, i] ~ Weight_ion$Treat_Contr)
  spw[i] <- shapiro.test(an5$residuals)$p.value
  nor_spw <- c(nor_spw, ifelse (spw[i] <= 0.05, "False", "True"))
}

#Creating summary table with the results
spw_table <- data.frame ("P-value" = spw, "Normality" = nor_spw)

#Performing significance test and plotting the results and values distribution
for (i in (7:11)) {
  bwei3 <- ggboxplot(Weight_ion, 
                     x = "Treat_Contr",
                     y = names(Weight_ion[i]), 
                     color = names(Weight_ion[3]),
                     palette = c("blue", "red"),
                     add = "jitter") + 
    stat_compare_means(data = Weight_ion, method = "kruskal.test",
                       label.x.npc = "center",
                       label.y.npc = "top") +
    theme(legend.position = "None") +
    labs(x = "", title = paste("Boxplot of the", names(Weight_ion[i])),
         caption = paste("Figure 2.", (i+4), " Distribution of the mean values of the ", names(Weight_ion[i]), sep = ""))
  print(bwei3)
}

##Gas parameters (photosynthesis rate, intercellular CO2, transpiration rate,
##stomatal conductance)

#Performing Shapiro–Wilk test of normality
spg <- numeric()
nor_spg <- c()
for (i in names(Gas_e[c(4:7)])) { 
  an6 <- aov(Gas_e[, i] ~ Gas_e$Treat_Contr)
  spg[i] <- shapiro.test(an6$residuals)$p.value
  nor_spg <- c(nor_spg, ifelse (spg[i] <= 0.05, "False", "True"))
}

#Creating summary table with the results
spg_table <- data.frame ("P-value" = spg, "Normality" = nor_spg)

#Performing significance test and plotting the results and values distribution
#for intercellular CO2 (with ANOVA results, as this is the only normally distributed value)
Gas_e_withoutNA <- na.omit(Gas_e)
banova <- ggboxplot(Gas_e_withoutNA,
                    x="Treat_Contr", 
                    y= "Intercellular_CO2",
                    color = names(Gas_e_withoutNA[3]),
                    palette = c("blue", "red"),
                    add = "jitter") 
banova  +   stat_compare_means(method = "anova") +
  theme(legend.position = "None") +
  labs(x = "", title = paste("Boxplot of the", names(Gas_e_withoutNA[5])),
       caption = paste("Figure 2.16 Distribution of the mean values of the", names(Gas_e_withoutNA[5])))
#for photosynthesis rate, transpiration rate, stomatal conductance
#(with Kruskal-Wallis results)
for (i in c(4,6:7)) {
  bgas <- ggboxplot(Gas_e_withoutNA, 
                    x = "Treat_Contr",
                    y = names(Gas_e_withoutNA[i]), 
                    color = names(Gas_e_withoutNA[3]),
                    palette = c("blue", "red"),
                    add = "jitter") + 
    stat_compare_means(data = Gas_e_withoutNA, method = "kruskal.test",
                       label.x.npc = "center",
                       label.y.npc = "top") +
    theme(legend.position = "None") +
    labs(x = "", title = paste("Boxplot of the", names(Gas_e_withoutNA[i])),
         caption = paste("Figure 2.", ifelse((i<5), paste(i+13), paste(i+12)), " Distribution of the mean values of the ", names(Gas_e_withoutNA[i]), sep = ""))
  print(bgas)
}