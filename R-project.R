install.packages("readxl")

data1 <- readxl::read_excel("/Users/lannysadikin/Documents/M1\ UPV/Data\ Aquisition/Rproject/data.xlsx", 1)
data1 <- data1[1:8] # to remove extra columns
data1 <- na.omit(data1) # to remove extra rows
data2 <- readxl::read_excel("/Users/lannysadikin/Documents/M1\ UPV/Data\ Aquisition/Rproject/data.xlsx", 2)
data3 <- readxl::read_excel("/Users/lannysadikin/Documents/M1\ UPV/Data\ Aquisition/Rproject/data.xlsx", 3)
data4 <- readxl::read_excel("/Users/lannysadikin/Documents/M1\ UPV/Data\ Aquisition/Rproject/data.xlsx", 4)

