## Data download and transfromation 
library(dplyr)
library(tidyr)
#install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
#install.packages("readxl")
library(readxl)

# downoald excel sheet from internet and store it into Excel object
Excel <- "https://dfzljdn9uc3pi.cloudfront.net/2020/9749/1/Raw_data_Afsar_et_al.%2C_2020-PeerJ_20.5.2020.xlsx"

# create the folder datafile if it doesnt exist
D <- "data"
if (!dir.exists(D)) dir.create(D)

f <- paste0 (D, "/data.xlsx")
# dowload excel into the file datafile (created beforehand) under the name data.xlsx
download.file(Excel, f, mode="wb")
# get the name of excel sheets
excel.sheet <- excel_sheets(f)

# created a list with the 4 sheet table from excel 
X <- list()
for (i in excel.sheet){
  X[[i]] <- as.data.frame(read_xlsx(f,i))
}

# organise column names and fill the first 2 columns
for(i in names(X)){
  names(X[[i]])[1:3] <- c("Number", "Code", "Treat_Contr")
  X[[i]] <- X[[i]][-1,]
  #X[[i]] <- X[[i]][,-1] - deletion of numbers
  X[[i]] <- fill(X[[i]], 1:2)
}

#deleting the accession #7 and #16 from Morphological traits,
#Weight_ion, Chloro_c, because there is no data for that accessions in the Gas_e
#for(i in names(X)){
#  X[[i]] <- X[[i]][!X[[i]]$Code == "Es-7", ]
#  X[[i]] <- X[[i]][!X[[i]]$Code == "Es-16", ]
#}

#transforming columns data format 
for (i in seq_along(X)){
  X[[i]][2:3] <- lapply(X[[i]][2:3], as.factor)
  X[[i]][-(2:3)] <- lapply(X[[i]][-(2:3)], as.numeric)
}

# creating data frames from different xlsx sheets
Morpho_t <- X$`Morphological traits`
Weight_ion <- X$`FW DW RWC Ions EL`
Chloro_c <- X$`Chlorophyll content`
Gas_e <- X$`Gas Exchange parameters`

#Changing names of the columns for more convinient use
names(Morpho_t)[4:8] <- c('Shoot_Length', 
                          'Root_Length', 
                          'Plant_Height', 
                          'Number_Leaves', 
                          'Leaf_Area')
names(Chloro_c)[4] <- c('Chlorophyll_Content')
names(Weight_ion) [4:12] <- c('Fresh_Weight', 
                              'Dry_Weight', 
                              'RWC',
                              'Na',
                              'K', 
                              'Ca', 
                              'Mg', 
                              'K_Na', 
                              'Electrolyte_Leakage')
names(Gas_e) [4:7] <- c('Photsynthesis_Rate', 
                        'Intercellular_CO2', 
                        'Transpiration_Rate', 
                        'Stomatal_Conductance')

# adding NA cells into the missing 7th and 16th accession
# creating a data frame with the missing data first
Gas_e_miss <- data.frame (Number = rep(c(7, 16), each = 8),
                          Code = rep(c("Es-7", "Es-16"), each = 8),
                          Treat_Contr = rep(c("Treatment", "Control",
                                            "Treatment", "Control"), each = 4),
                          Photsynthesis_Rate = rep(NA, 16),
                          Intercellular_CO2 = rep(NA, 16),
                          Transpiration_Rate = rep(NA, 16),
                          Stomatal_Conductance = rep(NA, 16))
#changing the numbers in "Number" column
Gas_e$Number <- replace(Gas_e$Number, , rep(c(1:6, 8:15, 17:25), each = 8))
# adding NA cells into the missing 7th and 16th accession
if (!(is.element('Es-7', Gas_e$Code))) Gas_e <- rbind(Gas_e, Gas_e_miss)
#Sorting the variables according to Number
Gas_e <- Gas_e[order(Gas_e$Number),]
#Rewriting row names to correspond the ordered data
row.names(Gas_e) <- c(1:200)

# downloading data for the location of accessions
word <- "https://dfzljdn9uc3pi.cloudfront.net/2020/9749/1/Table_S1.docx"
w <- paste0(D, "/location.docx")
download.file(word, w, mode="wb")

#creating data frame for the location of accessions
#install.packages("docxtractr")
library(docxtractr)
Acc_loc <- docx_extract_all_tbls(read_docx(w, track_changes = NULL),
                                 guess_header = TRUE, preserve = FALSE,
                                 trim = TRUE)
Acc_loc <- as.data.frame(Acc_loc)
#removing m from Elevation column 
Acc_loc$Elevation <- sub(' m', '', Acc_loc$Elevation)
#transforming data in the accession_loc
Acc_loc[c(1,6)] <- lapply(Acc_loc[c(1,6)], as.numeric)
Acc_loc[-c(1,6)] <- lapply(Acc_loc[-c(1,6)], as.factor)
#In order to fit the length of this table to the length of the ultimate table
#we need to double all the rows
Acc_loc_double <- Acc_loc[rep(1:25, each = 2), ]

###
#experimenting with mapping the data
map <- data.frame(Code = Acc_loc$Code,
                  Latitude = Acc_loc$Latitude,
                  Longitude = Acc_loc$Longitude)
m <- ggplot(map, aes(x = Latitude,
                     y = Longitude))
m +
  geom_jitter() +
  geom_label(aes(label = Code))


# create summary table for morphological trait
a <- Morpho_t %>% 
  group_by(Number, Code, Treat_Contr) %>% 
  summarise(Shoot_Length = mean(Shoot_Length),
            Root_Length = mean(Root_Length), 
            Plant_Height = mean(Plant_Height), 
            Number_Leaves = mean(Number_Leaves), 
            Leaf_Area = mean(Leaf_Area))

# remove NA values from Weight_ion table
Weight_ion <- na.omit(Weight_ion)
#Create summary table for Weight ion
b <- Weight_ion %>% 
  group_by(Number, Code, Treat_Contr) %>% 
  summarise(Fresh_Weight= mean(Fresh_Weight),
            Dry_Weight= mean(Dry_Weight),
            RWC = mean(RWC),
            Na =mean(Na),
            K =mean(K),
            Ca =mean(Ca),
            Mg= mean(Mg),
            K_Na =mean(K_Na),
            Electrolyte_Leakage= mean(Electrolyte_Leakage))
#take only the columns number 4 to 12 to avoid repetition of number, code and treatment
b <- b[4:12] 

#Create summary table for Weight_ion
c <- Chloro_c %>% 
  group_by(Number, Code, Treat_Contr) %>% 
  summarise(Chlorophyll_Content = mean(Chlorophyll_Content))
#take only the columns number 4 to avoid repetition of number, code and treatment
c <- c[4]  

#Create summary table for Gas_e
d <- Gas_e %>% 
  group_by(Number, Code, Treat_Contr) %>% 
  summarise(Photsynthesis_Rate= mean(Photsynthesis_Rate),
            Intercellular_CO2= mean(Intercellular_CO2), 
            Transpiration_Rate= mean(Transpiration_Rate), 
            Stomatal_Conductance=mean(Stomatal_Conductance))
#take only the columns number 4 to 7 to avoid repetition of number, code and treatment
d <- d[4:7]

#add Acc_loc data to the table
e <- Acc_loc_double
e <- e[6]

#create data frame with all the summarized tables 
table <- data.frame(a, b, c, d, e)