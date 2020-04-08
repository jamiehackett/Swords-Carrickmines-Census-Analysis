#Statistical analysis 
#Student No: X163876576
#Jamie Hackett

######## Packages install & loading ########
library(data.table)
library(stringr)
library(dplyr)

######## Data inject and cleanup ########

#Setting working directory
setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/Advanced BDA/CA")

#Loading the data
census <- read.csv("Census.csv")
areas <- read.csv("Areas.csv")

#Joining the data in a new dataframe
merged <- merge(census, areas, by=c("GUID"))

#Creating a new dataframe that just focuses on Swords and Carrickmines.
data_swords <- merged %>% filter(str_detect(EDNAME, "Swords"))
#Drop the first three columns
data_swords <- data_swords[ -c(1:3)]
#Grouping the data
data_swords <- data.frame(t(colSums(Filter(is.numeric, data_swords))))
#Adding column Area so I can identify
data_swords$location <- "Swords"

#Same for Carrickmines
data_carrickmines <- merged %>% filter(str_detect(EDNAME, "Foxrock-Carrickmines"))
#Drop the first three columns
data_carrickmines <- data_carrickmines[ -c(1:3)]
#Grouping the data
data_carrickmines <- data.frame(t(colSums(Filter(is.numeric, data_carrickmines))))
#Adding column Area so I can identify
data_carrickmines$location <- "Carrickmines"

######## Featuring engineering for each of the dataframes ########
data_swords$T0_4Total <- sum(data_swords$T1_1AGE0T, data_swords$T1_1AGE1T, data_swords$T1_1AGE2T, data_swords$T1_1AGE3T, data_swords$T1_1AGE4T)

data-

data_carrickmines$T0_4Total <- sum(data_carrickmines$T1_1AGE0T, data_carrickmines$T1_1AGE1T, data_carrickmines$T1_1AGE2T, data_carrickmines$T1_1AGE3T, data_carrickmines$T1_1AGE4T)

######## Combining dataframes ######## 
data <- rbind(data_carrickmines, data_swords)

write.csv(data, file ="outbound.")
help(write.csv)

######## Bit of featuring engineering to combine some age sets in bins of 5 years ######## 
data$T0_4Total <- sum(data$T1_1AGE0T, data$T1_1AGE1T, data$T1_1AGE2T, data$T1_1AGE3T, data$T1_1AGE4T)

data$T0_4Total

data$T0_4Total <- data %>% mutate(sumrow = data$T1_1AGE0T, data$T1_1AGE1T, data$T1_1AGE2T, data$T1_1AGE3T, data$T1_1AGE4T)

