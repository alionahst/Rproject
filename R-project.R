## Data dowload and transfromation 


#install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
#install.packages("readxl")
library(readxl)

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

# created a list with the 4 sheet table from excel 
X <- list()
for (i in excel.sheet){
  X[[i]] <- as.data.frame(read_xlsx(f,i))
}

# organise column names and fill the first 2 columns
library(tidyr)
for(i in names(X)){
  names(X[[i]])[1:3] <- c("Sr_No","Code", "Treatment")
  X[[i]] <- X[[i]][-1,]
  X[[i]] <- fill(X[[i]], 1:2)
}

Morph_tr <- X$`Morphological traits`
Weight_ions <- X$`FW DW RWC Ions EL`
Chlor_cont <- X$`Chlorophyll content`
Gas_par <- X$`Gas Exchange parameters`




word <- "https://dfzljdn9uc3pi.cloudfront.net/2020/9749/1/Table_S1.docx"
w <- paste0(d, "/location.docx")
download.file(word, w, mode="wb")

#install.packages("docxtractr")
library(docxtractr)
accession_loc <- docx_extract_all(read_docx(w, track_changes = NULL), guess_header = TRUE, preserve = FALSE, trim = TRUE)
accession_loc<- as.data.frame(accession_loc)
