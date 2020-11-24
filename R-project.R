## Data dowload and transfromation 


install.packages("ggplot2", dependencies = TRUE)
library("ggplot2")
install.packages("readxl")
library("readxl")

#downoald excel sheet from internet and store it into Excel object
Excel <- "https://dfzljdn9uc3pi.cloudfront.net/2020/9749/1/Raw_data_Afsar_et_al.%2C_2020-PeerJ_20.5.2020.xlsx"

# create the folder datafile if it doesnt exist
d <- "datafile"
if (!dir.exists(d)) dir.create(d)

f <- paste0 (d, "/data.xlsx") 
#dowload excel into the file datafile (created beforehand) under the name data.xlsx
download.file(Excel, f, mode="wb")
# get the name of excel sheets
excel.sheet <- excel_sheets(f)

#X <- list()
#for (i in excel.sheet){
#  X[[i]] <- read_xlsx(f,i)
#}
#names(X$`Gas Exchange parameters`[1:2]) <- names(X$`Gas Exchange parameters`[1, 1:2])
#names(X$`Gas Exchange parameters`)

Morphological_traits <- read_xlsx(f,1) #data.1
FW_DW_RWC_Ions_EL <- read_xlsx(f,2)
Chlorophyll_content <- read_xlsx(f,3)
Gas_Exchange_parameter <- read_xlsx(f,4)
general <- read_xlsx(f)

table_names <- c(Morphological_traits, FW_DW_RWC_Ions_EL, Chlorophyll_content, Gas_Exchange_parameter)




library(tidyr)
d1 <- d2 <- d3 <- d4 <- data.frame()

i = Morphological_traits
for(i in table_names){
  names(i)[1:2] <- i[1, 1:2]
}

#  names(i) [3] <- "Treatment"
#  i <- i[-1,] 
#  i <- fill(i, 1:2) 
#  }



names(Morphological_traits)[1:2] <- Morphological_traits[1, 1:2]
names(Morphological_traits) [3] <- "Treatment"

Morphological_traits <- Morphological_traits[-1,] 

library(tidyr)
Morphological_traits<- fill(Morphological_traits, 1:2)

