install.packages("ggplot", dependencies = TRUE)
library("ggplot")
install.packages("readxl")
library("readxl")

#downoald excel sheet from internet and store it into Excel object
Excel <- "https://dfzljdn9uc3pi.cloudfront.net/2020/9749/1/Raw_data_Afsar_et_al.%2C_2020-PeerJ_20.5.2020.xlsx"

#dowload excel into the file datafile under the name data.xlsx
download.file(Excel,"datafile/data.xlsx")
# get the name of excel sheets
excel.sheet <- excel_sheets("datafile/data.xlsx")

#don't understand that part 
X <- list()
for (i in excel.sheet){
  X[[i]] <- read_xlsx("data/Data.xlsx",i)
}


data1 <- read_xlsx("data/Data.xlsx",1)
data2 <- read_xlsx("data/Data.xlsx",2)
data3 <- read_xlsx("data/Data.xlsx",3)
data4 <- read_xlsx("data/Data.xlsx",4)
