#Statistical analysis 
#Student No: X163876576
#Jamie Hackett

######## Packages install & loading ########
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tibble)

######## Data inject and cleanup ########

#Setting working directory
setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/Advanced BDA/CA")

#Loading the data
census <- read.csv("Census.csv")
areas <- read.csv("Areas.csv")

#Joining the data in a new dataframe
merged <- merge(census, areas, by=c("GUID"))

#Creating a new dataframe that just focuses on Baldoyle and Sandford
data_baldoyle <- merged %>% filter(str_detect(EDNAME, "Baldoyle"))
#Drop the first three columns
data_baldoyle <- data_baldoyle[ -c(1:3)]
#Adding column Area so I can identify
data_baldoyle$location <- "Baldoyle"
#Grouping the data
data_baldoyle_sum <- data.frame(t(colSums(Filter(is.numeric, data_baldoyle))))
#Adding column Area so I can identify
data_baldoyle_sum$location <- "Baldoyle"


#Same for Carrickmines
data_sandyford <- merged %>% filter(str_detect(EDNAME, "Dundrum-Sandyford"))
#Drop the first three columns
data_sandyford <- data_sandyford[ -c(1:3)]
#Adding column Area so I can identify
data_sandyford$location <- "Sandyford"
#Grouping the data
data_sandyford_sum <- data.frame(t(colSums(Filter(is.numeric, data_sandyford))))
#Adding column Area so I can identify
data_sandyford_sum$location <- "Sandyford"


######## Featuring engineering for each of the dataframes ########
data_baldoyle_sum$T0_4Total <- sum(data_baldoyle_sum$T1_1AGE0T, data_baldoyle_sum$T1_1AGE1T, data_baldoyle_sum$T1_1AGE2T, data_baldoyle_sum$T1_1AGE3T, data_baldoyle_sum$T1_1AGE4T)

data_baldoyle_sum$T5_9Total <- sum(data_baldoyle_sum$T1_1AGE5T, data_baldoyle_sum$T1_1AGE6T, data_baldoyle_sum$T1_1AGE7T, data_baldoyle_sum$T1_1AGE8T, data_baldoyle_sum$T1_1AGE9T)

data_baldoyle_sum$T10_14Total <- sum(data_baldoyle_sum$T1_1AGE10T, data_baldoyle_sum$T1_1AGE11T, data_baldoyle_sum$T1_1AGE12T, data_baldoyle_sum$T1_1AGE13T, data_baldoyle_sum$T1_1AGE14T)

data_baldoyle_sum$T15_19Total <- sum(data_baldoyle_sum$T1_1AGE15T, data_baldoyle_sum$T1_1AGE16T, data_baldoyle_sum$T1_1AGE17T, data_baldoyle_sum$T1_1AGE18T, data_baldoyle_sum$T1_1AGE19T)

data_sandyford_sum$T0_4Total <- sum(data_sandyford_sum$T1_1AGE0T, data_sandyford_sum$T1_1AGE1T, data_sandyford_sum$T1_1AGE2T, data_sandyford_sum$T1_1AGE3T, data_sandyford_sum$T1_1AGE4T)

data_sandyford_sum$T5_9Total <- sum(data_sandyford_sum$T5_1AGE5T, data_sandyford_sum$T1_1AGE6T, data_sandyford_sum$T1_1AGE7T, data_sandyford_sum$T1_1AGE8T, data_sandyford_sum$T1_1AGE9T)

data_sandyford_sum$T10_14Total <- sum(data_sandyford_sum$T5_1AGE10T, data_sandyford_sum$T1_1AGE11T, data_sandyford_sum$T1_1AGE12T, data_sandyford_sum$T1_1AGE13T, data_sandyford_sum$T1_1AGE14T)

data_sandyford_sum$T15_19Total <- sum(data_sandyford_sum$T5_1AGE15T, data_sandyford_sum$T1_1AGE16T, data_sandyford_sum$T1_1AGE17T, data_sandyford_sum$T1_1AGE18T, data_sandyford_sum$T1_1AGE19T)

######## Combining dataframes######## 
data_sum <- rbind(data_baldoyle_sum, data_sandyford_sum)

######## Loading in fixed data ######## 
data_sum_edited <- read.csv("outbound.csv")

######## Histogram of age distribution ######## 
Age <- data_sum_edited[,c(806:810)]
Age <- data_sum_edited[,c(91:104, 806:810)]

Age <- Age[, c(15:19, 1:14)]

#Two historgrams for each location
histBaldoyle <- subset(Age, location == "Baldoyle")
histSandyford <- subset(Age, location == "Sandyford")

#Changing them into more user friendly histograms
histBaldoyle <- histBaldoyle %>% gather(Age_Bracket, value, X0_4, X5_9, X10_14, X15_19, X20_24, X25_29, X30_34, X35_39, X40_44, X45_49, X50_54, X55_59, X60_64, X65_69, X70_74, X75_79, X80_84, X85.)

histSandyford <- histSandyford %>% gather(Age_Bracket, value, X0_4, X5_9, X10_14, X15_19, X20_24, X25_29, X30_34, X35_39, X40_44, X45_49, X50_54, X55_59, X60_64, X65_69, X70_74, X75_79, X80_84, X85.)


ggplot(histBaldoyle, aes(x = Age_Bracket, y = value)) + geom_col(fill = "#2fc7da", colour = "black") + ggtitle("Age distribution of Baldoyle") + ylab("Count") + xlab ("Age Bracket") + theme_classic()
  
  
ggplot(histSandyford, aes(x = Age_Bracket, y = value)) + geom_col(fill = "#fcba15", colour = "black") + ggtitle("Age distribution of Sandyford") + ylab("Count") + xlab ("Age Bracket") + theme_classic()

######## Descriptives of Average family sizes ######## 
data_unsum <- rbind(data_baldoyle, data_sandyford)


Family_Baldolye <- data_baldoyle[, c(820, 205:210)]
summary(Family_Baldolye$T4_1_TF)

Family_Sandyford <- data_sandyford[, c(820, 205:210)]
summary(Family_Sandyford$T4_1_TF)

Family_Both <- rbind(Family_Baldolye, Family_Sandyford)

######## Histograms of Average family sizes ######## 
ggplot(Family_Baldolye, aes(x=T4_1_TF)) + geom_histogram(bins=12, fill = "#2fc7da", colour = "black") + ggtitle("Total Family size distribution of Baldoyle") + ylab("Count") + xlab ("Family Size") + theme_classic() 

ggplot(Family_Sandyford, aes(x=T4_1_TF)) + geom_histogram(bins=12, fill = "#fcba15", colour = "black") + ggtitle("Total Family size of Sandyford") + ylab("Count") + xlab ("Family Size") + theme_classic()

ggplot(Family_Both, aes(x=T4_1_TF, color=location, fill=location)) + geom_histogram(bins=12, position="identity", alpha=0.5) + ggtitle("Total Family size of Sandyford") + ylab("Count") + xlab ("Family Size") + theme_classic()

######## Boxplots of Average family sizes ########
ggplot(Family_Both, aes(x=location, y=T4_1_TF, fill=location)) + geom_boxplot() + ggtitle("Total Family size of Sandyford") + ylab("Family Size") + xlab ("Location") + theme_classic()

######## QQPlots for normality of Average family sizes ######## 
#Baldoyle
qqnorm(Family_Baldolye$T4_1_TF, frame = F)
qqline(Family_Baldolye$T4_1_TF, col = "#2fc7da")

#Sandyford
qqnorm(Family_Sandyford$T4_1_TF, frame = F)
qqline(Family_Sandyford$T4_1_TF, col = "#fcba15")

######## Shapiro-Wilk test for normality ########
shapiro.test(Family_Baldolye$T4_1_TF)
shapiro.test(Family_Sandyford$T4_1_TF)

######## Since data is normal Student's T-Test ######## 
res.ftest <- var.test(T4_1_TF ~ location, data = Family_Both)
res.ftest
t.test(Family_Baldolye$T4_1_TF, Family_Sandyford$T4_1_TF, alternative = "two.sided", paired = F)
