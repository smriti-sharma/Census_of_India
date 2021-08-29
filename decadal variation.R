setwd("~/Documents/MSC/sem2/DM/program")
library(mice)
library(stringr)

# Loading data...
data <- read.csv("Decadal_variation.csv" , header = T , sep = ",")
View(data)

#Selecting relevant data...
data <- data[,c(3,4,6,8,9)]
View(data)

#Removing irrelevant information from selected data...
for( i in 1: nrow(data))
  if(data$India.State.Union.Territory[i] == "")
      data$India.State.Union.Territory[i] <- data$India.State.Union.Territory[i-1]
View(data)
  
data <- data[!(data[1] == "INDIA"),]
View(data)  

for(i in seq(from = 1901 , to = 2011 , by = 10))
  assign(paste0("temp.",i) ,data[data[2] == i , ])

ls()
for(i in seq(from = 1901 , to = 2011 , by = 10))
  View(get(paste0("temp.",i)))

#Renaming column names...
for(i in seq(from = 1901 , to = 2011 , by = 10))
{
    x = get(paste0("temp.",i))
    colnames(x)[2:ncol(x)]<-paste(colnames(x)[2:ncol(x)],i,sep=".")
    assign(paste0("temp.",i),x)
}

#Removing irrelevant column...
for(i in seq(from = 1901 , to = 2011 , by = 10))
{
  x = get(paste0("temp.",i))
  x<-x[,-2]
  assign(paste0("temp.",i),x)
}

temp.1901<-temp.1901[,-2]
  

#merging columns
data<-temp.1901
for(i in seq(from = 1911 , to = 2011, by = 10))
{
  x<-get(paste0("temp.",i))
  data<-merge(data , x , by= colnames(data)[1] , all = T)
}
View(data)

#///////Cleaning of DATA/////

#//1.Removing ','...
for( i in 2:ncol(data)) 
  data[,i] <- gsub(",","",data[,i],fixed = TRUE)#converted to double.
View(data)

#//2.Cleaning state column...
data$India.State.Union.Territory <- gsub("[^[:alnum:]///' ]", "", data$India.State.Union.Territory)
View(data)

#//3.Type Casting of columns...
for(i in 1:nrow(data)) {
  for(j in 2:ncol(data)) {
    if(!is.na(data[i,j]) && tolower(data[i,j]) == toupper(data[i,j]))
    {
      data[i,j] <- gsub(" ","",data[i,j])
      data[i,j] <- as.numeric(data[i,j])
    }
    else
      data[i,j] <- NA
  }
}
for(j in 2:ncol(data)) 
  data[,j] <- type.convert(data[,j])
View(data)

#//4.Handling NA's...
sum(is.na(data))
#We have total 78 NA's...replacing them by cart...
tempdata <- data[,-1]
View(tempdata)
tempdata <- mice(tempdata, m =1 , maxit = 25 , method = "cart" , remove_collinear = FALSE , print = FALSE)#cart for classification and regression. we don't use pmm here because resultant dataframe is not invertible.
data[,-1]<- complete(tempdata,1)
View(data)
sum(is.na(data))

#//5.Normalizing...
female_2011 = sum(data[,36])
male_2011 = sum(data[,35])
population_of_2011 = female_2011 + male_2011

for(i in 2:ncol(data))
  data[,i] <- (data[,i]/population_of_2011)*100
View(data) 


#////////////////////////////////EXPORTING DATA//////////////////////////////////////
  write.csv(data,file = "decadal_variation_exported.csv")

