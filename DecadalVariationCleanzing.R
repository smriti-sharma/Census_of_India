# SETTING DIRECTORY =====================================================================
setwd("~/DM/DM Census data")

# INSTALLING LIBRARIES AND PACKAGES
install.packages("readxl")
install.packages("corrplot")
install.packages("mice")
install.packages("dplyr")
library("readxl")
library(dplyr)
library(corrplot)
library(mice) # For handling NA's


# LOADING DECADAL VARIATION CENSUS FILE
decalad_file <- read_excel("Decadal_variation.xls")

# STRUCTURE AND SUMMARY OF DATA
str(decalad_file)
summary(decalad_file)
View(decalad_file)

# CLEANING DATA =========================================================================
  # removing extra row
  decalad_file_modify <- decalad_file[-1,]         # remving blank row
  # removing extra col
  decalad_file_modify <- decalad_file[,-6]         # removing Varitation absolute
  decalad_file_modify <- decalad_file_modify[,-6]  # removing Variation percentage
  decalad_file_modify <- decalad_file_modify[,-1]  # removing State Code
  decalad_file_modify <- decalad_file_modify[,-1]  # removing District Code
  # removing extra row
  decalad_file_modify <- decalad_file_modify[-(1:16),]# removing comments
  View(decalad_file_modify)

  # swapping all rows of column 1 and 2 
  rows <- nrow(decalad_file_modify)
  temp <- decalad_file_modify[(1:rows),1]
  decalad_file_modify[(1:rows),1]<-decalad_file_modify[(1:rows),2]
  decalad_file_modify[(1:rows),2]<-temp
  View(decalad_file_modify)
  
  # swapping heading of column 1 and 2
  wemp <- colnames(decalad_file_modify[,1])
  colnames(decalad_file_modify[,1])<-colnames(decalad_file_modify[,2])
  colnames(decalad_file_modify[,2])<-wemp
  View(decalad_file_modify)
  
  # replacing NAs
  decalad_file_modify[decalad_file_modify == 'N.A.'] <- 0
  decalad_file_modify[decalad_file_modify == 'N.A'] <- 0
  
  # change in population of females in J&K from 1901:2011
  v <- decalad_file_modify$Females[(1:12),]/sum(decalad_file_modify$Females[(1:12),])
  hist(as.numeric(decalad_file_modify$Females)[(1:12),],xlab = "year",ylab = "Female Population",col = "yellow",border = "blue")
  View(v)
  
  
  
  
  
  
  
  data <- t(decalad_file_modify[(1:12),1])
  colnames(data) <- as.character(unlist(data[1,]))
  data = data[-1, ]
  data<- t(decalad_file_modify[(1:12),(3:5)])
  data[4,]<-decalad_file_modify[13,(2:12)]
  View(data)
  
  
  