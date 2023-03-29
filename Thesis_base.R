#Tittle: Chemical Contamination Arizona
# Author: Lina Benitez
# Initial Date: November 01 2022
#Test

##1. Install Packages


install.packages("tidycensus")
install.packages("tidyverse")
install.packages("sp")
install.packages("plotly")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("here")
install.packages("scales")
install.packages("xlsx")
install.packages("corrplot")
install.packages("ape")
install.packages("plm")
install.packages("pglm")
install.packages("lmtest")
install.packages("texreg")
install.packages("sandwich")
install.packages("GWPR.light")
install.packages("writexl")


##---api key--##

library(tidycensus)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(sp)
library(xlsx)
library(corrplot)
library(here)
library(ape)
library(plm)
library(sandwich)
library(pglm)
library(lmtest)
library(texreg)
library(GWPR.light)
library("writexl")
census_api_key("9cad7f1490792ca12565fe027f4f5f146ac6a799")

##2. Download data from 2020##

################################################################################
########################YEAR 2020###############################################

##---Get data at the neighborhood level---##

Neighborhood2020 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P1_001N',
  year= 2020
  
)

#Hispanic##
Hispanic2020 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P2_002N',
  year= 2020
  
)
?get_decennial


#White##
White2020 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P2_005N',
  year= 2020
  
)

#Black##
Black2020 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P2_006N',
  year= 2020
  
)

#American Indian##
AmericanIndian2020 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P2_007N',
  year= 2020
  
)

#Asian##
Asian2020 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P2_008N',
  year= 2020
  
)

#Native Hawaian Population##
NativeHawaian2020 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P2_009N',
  year= 2020
  
)
#Create table with all the total populations
Join_table<-inner_join(x = Neighborhood2020, y = Join_table, by = "GEOID")
Population_table2020<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x,NAME.y.y.y) )

#Export population table
write.csv(Population_table2020,"C:/Users/linab/Documents/Thesis/Thesis/Population_table2.csv", row.names = FALSE)

#Import population table with percentages to R##
Population_table2020<-read_csv("C:/Users/linab/Documents/Thesis/Thesis/Population_table.csv")

#Histogram with percentage of races per census tract##
Hispanic<-as.numeric(Population_table2020$PERCENT_HISPANIC)
White<-as.numeric(Population_table2020$PERCENT_WHITE)
Black<-as.numeric(Population_table2020$PERCENT_BLACK)
Native_American<-as.numeric(Population_table2020$PERCENT_AMERICANINDIAN)
Asian<-as.numeric(Population_table2020$PERCENT_ASIAN)
Native_Hawaian<-as.numeric(Population_table2020$PERCENT_NATIVE_HAWAIAN)

hist(Hispanic,
     main="Percent of hispanic per tract level",
     xlab="% of hispanic",
     col="darkmagenta",
     freq=TRUE
)

hist(White,
     main="Percent of White per tract level",
     xlab="% of white",
     col="blue",
     freq=TRUE
)

hist(Black,
     main="Percent of black per tract level",
     xlab="% of black",
     col="darkmagenta",
     freq=TRUE
)

hist(Native_American,
     main="Percent of Native_American per tract level",
     xlab="% of Native_American",
     col="blue",
     freq=TRUE
)
hist(Asian,
     main="Percent of Asian per tract level",
     xlab="% of Asian",
     col="blue",
     freq=TRUE
)
hist(Native_Hawaian,
     main="Percent of Native_Hawaian per tract level",
     xlab="% of Native_Hawaian",
     col="blue",
     freq=TRUE
)

##Median household income in 2020 URL: https://data.census.gov/cedsci/table?q=b19013&g=0400000US04%241400000&tid=ACSDT5Y2020.B19013##
Income_2020<-read_csv("C:/Users/linab/Documents/Thesis/Thesis/Income_Table_tractlevel.csv")
Income2020<-Income_2020$`Median household income in the past 12 months (in 2020 inflation-adjusted dollars)`
hist(Income2020,
     main="Median household income in 2020",
     xlab="Income 2020. USD",
     col="blue",
     freq=TRUE
)
mean(Income2020)

Neighborhood2010 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P001001',
  year= 2010,
  geometry= TRUE
  
)

Neighborhood2000 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P001001',
  year= 2000,
  geometry= TRUE
  
)

Population_2020<-Neighborhood2020$value
Hispanic_2020<-Neighborhood2020$value
Total_Population_2020<-sum(Population_2020,na.rm = FALSE)

#Create Table household ownership rate
Join_tableownership<-inner_join(x = az_ownership, y = az_occupied, by = "GEOID")
Ownership_table2020<- subset(Join_tableownership, select = -c(NAME.y,variable.x, variable.y) )

#Export population table
write.csv(Ownership_table2020,"C:/Users/linab/Documents/Thesis/Thesis/Ownership_2020.csv", row.names = FALSE)

#Get elderly people more than 65 years
az_elderly20 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0029',
  year= 2020)

#Get people less than 5 years
az_child20 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2020)

#Get total population
az_total20 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2020)

#Get female led household population
az_femhou201 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2020)

az_femhou202 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2020)

family2020 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2020)

Table2020<-inner_join(x = az_femhou201, y = az_femhou202, by = "GEOID")
Table2020<-inner_join(x = Table2020, y = az_child20, by = "GEOID")
Table2020<-inner_join(x = Table2020, y = family2020, by = "GEOID")
Table2020<-inner_join(x = Table2020, y = az_total20, by = "GEOID")
write.csv(Table2020,"C:/Users/linab/Documents/Spring 2023/Thesis/table2020.csv", row.names = FALSE)


#Create Table
Join_table<-inner_join(x = az_elderly20, y = az_total20, by = "GEOID")
Join_table<-inner_join(x = Join_table, y = az_ownership2020, by = "GEOID")
Join_table<-inner_join(x = Join_table, y = az_occupied2020, by = "GEOID")
Population_table2020<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Export population table
write.csv(Population_table2020,"C:/Users/linab/Documents/Spring 2023/Thesis/Population_table20.csv", row.names = FALSE)

################################################################################
########################YEAR 2021###############################################

##---Get data a the neighborhood level---##

Neighborhood2021 <- tidycensus::get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2021
  
)

#Hispanic##
Hispanic2021 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2021
  
)

#White##
White2021 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2021
  
)

#Black##
Black2021 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2021
  
)

#American Indian##
AmericanIndian2021 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2021
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2021, y = Black2021, by = "GEOID")
Join_table<-inner_join(x = White2021, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2021, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2021, y = Join_table, by = "GEOID")
Population_table2021<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2021 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B19013_001',
  year= 2021
)

#Ownership 2021
az_ownership2021 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2021,
  variables =  "DP04_0046")

az_occupied2021 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2021,
  variables =  "DP04_0045")


#Get elderly people more than 65 years
az_elderly21 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0024',
  year= 2021)

#Get population with less than 5 years
az_child21 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2021)

#Get total population
az_total21 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2021)

#Get female led household percentage
az_femhou211 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2021)

az_femhou212 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2021)

family2021 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2021)

#Joined table
Table2021<-inner_join(x = az_femhou211, y = az_femhou212, by = "GEOID")
Table2021<-inner_join(x = Table2021, y = az_child21, by = "GEOID")
Table2021<-inner_join(x = Table2021, y = family2021, by = "GEOID")
Table2021<-inner_join(x = Table2021, y = az_total21, by = "GEOID")
write.csv(Table2021,"C:/Users/linab/Documents/Spring 2023/Thesis/table21.csv", row.names = FALSE)

Join_table<-inner_join(x = Income2021, y = Population_table2021, by = "GEOID")
Join_table<-inner_join(x = az_ownership2021, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2021, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly21, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female21, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total21, y = Join_table, by = "GEOID")
Population_table2021<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2021,"C:/Users/linab/Documents/Thesis/Thesis/Population_table2021.csv", row.names = FALSE)

################################################################################
########################YEAR 2019###############################################

##---Get data a the neighborhood level---##

Neighborhood2019 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2019
  
)

#Hispanic##
Hispanic2019 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2019
  
)

#White##
White2019 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2019
  
)

#Black##
Black2019 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2019
  
)

#American Indian##
AmericanIndian2019 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2019
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2019, y = Black2019, by = "GEOID")
Join_table<-inner_join(x = White2019, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2019, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2019, y = Join_table, by = "GEOID")
Population_table2019<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2019 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B19013_001',
  year= 2019
)

#Ownership 2019
az_ownership2019 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2019,
  variables =  "DP04_0046")

az_occupied2019 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2019,
  variables =  "DP04_0045")


#Get elderly people more than 65 years
az_elderly19 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0024',
  year= 2019)

#Get total population
az_total19 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2019)

#Get population with less than 5 years
az_child19 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2019)

#Get female led household percentage
az_femhou191 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2019)

az_femhou192 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2019)

family2019 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2019)

#Joined table
Table2019<-inner_join(x = az_femhou191, y = az_femhou192, by = "GEOID")
Table2019<-inner_join(x = Table2019, y = az_child19, by = "GEOID")
Table2019<-inner_join(x = Table2019, y = family2019, by = "GEOID")
Table2019<-inner_join(x = Table2019, y = az_total19, by = "GEOID")
write.csv(Table2019,"C:/Users/linab/Documents/Spring 2023/Thesis/table2019.csv", row.names = FALSE)


Join_table<-inner_join(x = Income2019, y = Population_table2019, by = "GEOID")
Join_table<-inner_join(x = az_ownership2019, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2019, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly19, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female19, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total19, y = Join_table, by = "GEOID")
Population_table2019<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2019,"C:/Users/linab/Documents/Thesis/Thesis/Population_table2019.csv", row.names = FALSE)



################################################################################
########################YEAR 2018###############################################

##---Get data a the neighborhood level---##

Neighborhood2018 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2018
  
)

#Hispanic##
Hispanic2018 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2018
  
)

#White##
White2018 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2018
  
)

#Black##
Black2018 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2018
  
)

#American Indian##
AmericanIndian2018 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2018
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2018, y = Black2018, by = "GEOID")
Join_table<-inner_join(x = White2018, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2018, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2018, y = Join_table, by = "GEOID")
Population_table2018<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2018 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'S1902_C03_001',
  year= 2018
)

#Ownership 2018
az_ownership2018 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2018,
  variables =  "DP04_0046")

az_occupied2018 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2018,
  variables =  "DP04_0045")


#Get elderly people more than 65 years
az_elderly18 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0024',
  year= 2018)

#Get population with less than 5 years
az_child18 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2018)

#Get total population
az_total18 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2018)

#Get female led household percentage
az_femhou181 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2018)

az_femhou182 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2018)

family2018 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2018)


Table2018<-inner_join(x = az_femhou181, y = az_femhou182, by = "GEOID")
Table2018<-inner_join(x = Table2018, y = az_child18, by = "GEOID")
Table2018<-inner_join(x = Table2018, y = family2018, by = "GEOID")
Table2018<-inner_join(x = Table2018, y = az_total18, by = "GEOID")
write.csv(Table2018,"C:/Users/linab/Documents/Spring 2023/Thesis/table2018.csv", row.names = FALSE)

#Get female population
az_female18 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0027',
  year= 2018)

#Get total population
az_total18 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2018)

#Joined Table
Join_table<-inner_join(x = Income2018, y = Population_table2018, by = "GEOID")
Join_table<-inner_join(x = az_ownership2018, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2018, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly18, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female18, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total18, y = Join_table, by = "GEOID")
Population_table2018<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2018,"C:/Users/linab/Documents/Thesis/Thesis/Population_table2018.csv", row.names = FALSE)

################################################################################
########################YEAR 2017###############################################

##---Get data a the neighborhood level---##
#Get population with less than 5 years
az_child17 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2017)

#Get total population
az_total17 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2017)

#Get female led percentage
az_femhou171 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2017)

az_femhou172 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2017)

family2017 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2017)

#Joined Table
Table2017<-inner_join(x = az_femhou171, y = az_femhou172, by = "GEOID")
Table2017<-inner_join(x = Table2017, y = az_child17, by = "GEOID")
Table2017<-inner_join(x = Table2017, y = family2017, by = "GEOID")
Table2017<-inner_join(x = Table2017, y = az_total17, by = "GEOID")
write.csv(Table2017,"C:/Users/linab/Documents/Spring 2023/Thesis/table2017.csv", row.names = FALSE)


################################################################################
########################YEAR 2016###############################################

##---Get data a the neighborhood level---##

Neighborhood2016 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2016
  
)

#Hispanic##
Hispanic2016 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2016
  
)

#White##
White2016 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2016
  
)

#Black##
Black2016 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2016
  
)

#American Indian##
AmericanIndian2016 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2016
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2016, y = Black2016, by = "GEOID")
Join_table<-inner_join(x = White2016, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2016, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2016, y = Join_table, by = "GEOID")
Population_table2016<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2016 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'S1901_C01_013',
  year= 2016
)

#Ownership 2016
az_ownership2016 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2016,
  variables =  "DP04_0046")

az_occupied2016 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2016,
  variables =  "DP04_0045")


#Get elderly people more than 65 years
az_elderly16 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0025',
  year= 2016)

#Get population with less than 5 years
az_child16 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2016)

#Get total population
az_total16 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2016)

#Get female led household percentage
az_femhou161 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2016)

az_femhou162 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2016)

family2016 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2016)

Table2016<-inner_join(x = az_femhou161, y = az_femhou162, by = "GEOID")
Table2016<-inner_join(x = Table2016, y = az_child16, by = "GEOID")
Table2016<-inner_join(x = Table2016, y = family2016, by = "GEOID")
Table2016<-inner_join(x = Table2016, y = az_total16, by = "GEOID")
write.csv(Table2016,"C:/Users/linab/Documents/Spring 2023/Thesis/table20162.csv", row.names = FALSE)

Join_table<-inner_join(x = Income2016, y = Population_table2016, by = "GEOID")
Join_table<-inner_join(x = az_ownership2016, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2016, y = az_ownership2016, by = "GEOID")
Join_table<-inner_join(x = az_elderly16, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total16, y = Join_table, by = "GEOID")
Population_table2016<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )

#Export population table
write.csv(Population_table2016,"C:/Users/linab/Documents/Spring 2023/Thesis/Population_table16.csv", row.names = FALSE)

################################################################################
########################YEAR 2015###############################################

##---Get data a the neighborhood level---##

Neighborhood2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2015
  
)

#Hispanic##
Hispanic2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2015
  
)

#White##
White2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2015
  
)

#Black##
Black2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2015
  
)

#American Indian##
AmericanIndian2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2015
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2015, y = Black2015, by = "GEOID")
Join_table<-inner_join(x = White2015, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2015, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2015, y = Join_table, by = "GEOID")
Population_table2015<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'S1901_C01_013',
  year= 2015
)

#Ownership 2015
az_ownership2015 <- get_acs(
  geography = "state",
  state = "AZ",
  year = 2015,
  variables =  "DP04_0046")

az_occupied2015 <- get_acs(
  geography = "state",
  state = "AZ",
  year = 2015,
  variables =  "DP04_0045")


#Get elderly people more than 65 years
az_elderly2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0025',
  year= 2015)

#Get population with less than 5 years
az_child15 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2015)

#Get total population
az_total2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2015)

#Get female led household percentage
az_femhou151 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2015)

az_femhou152 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2015)

family2015 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2015)

#Joined table
Table2015<-inner_join(x = az_femhou151, y = az_femhou152, by = "GEOID")
Table2015<-inner_join(x = Table2015, y = az_child15, by = "GEOID")
Table2015<-inner_join(x = Table2015, y = family2015, by = "GEOID")
Table2015<-inner_join(x = Table2015, y = az_total2015, by = "GEOID")
write.csv(Table2015,"C:/Users/linab/Documents/Spring 2023/Thesis/table20152.csv")

Join_table<-inner_join(x = Income2015, y = Population_table2015, by = "GEOID")
Join_table<-inner_join(x = az_ownership2015, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2015, y = az_ownership2015, by = "GEOID")
Join_table<-inner_join(x = az_elderly2015, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2015, y = Join_table, by = "GEOID")
Population_table2015<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )

#Export population table
write.csv(Population_table2015,"C:/Users/linab/Documents/Spring 2023/Thesis/Population_table15.csv")

################################################################################
########################YEAR 2014###############################################

##---Get data a the neighborhood level---##

Neighborhood2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2014
  
)

#Hispanic##
Hispanic2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2014
  
)

#White##
White2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2014
  
)

#Black##
Black2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2014
  
)

#American Indian##
AmericanIndian2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2014
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2014, y = Black2014, by = "GEOID")
Join_table<-inner_join(x = White2014, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2014, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2014, y = Join_table, by = "GEOID")
Population_table2014<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'S1901_C01_013',
  year= 2014
)

#Ownership 2014
az_ownership2014 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2014,
  variables =  "DP04_0045")

az_occupied2014 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2014,
  variables =  "DP04_0044")


#Get elderly people more than 65 years
az_elderly2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0021',
  year= 2014)

#Get population with less than 5 years
az_child14 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2014)

#Get total population
az_total2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2014)
#Get female led household percentage
az_femhou141 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2014)

az_femhou142 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2014)

family2014 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2014)

Table2014<-inner_join(x = az_femhou141, y = az_femhou142, by = "GEOID")
Table2014<-inner_join(x = Table2014, y = az_child14, by = "GEOID")
Table2014<-inner_join(x = Table2014, y = family2014, by = "GEOID")
Table2014<-inner_join(x = Table2014, y = az_total2014, by = "GEOID")
write.csv(Table2014,"C:/Users/linab/Documents/Spring 2023/Thesis/table2014.csv", row.names = FALSE)


Join_table<-inner_join(x = Income2014, y = Population_table2014, by = "GEOID")
Join_table<-inner_join(x = az_ownership2014, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2014, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly2014, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female2014, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2014, y = Join_table, by = "GEOID")
Population_table2014<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2014,"C:/Users/linab/Documents/Thesis/Thesis/Population_table20142.csv", row.names = FALSE)


################################################################################
########################YEAR 2013###############################################

##---Get data a the neighborhood level---##

Neighborhood2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2013
  
)

#Hispanic##
Hispanic2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2013
  
)

#White##
White2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2013
  
)

#Black##
Black2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2013
  
)

#American Indian##
AmericanIndian2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2013
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2013, y = Black2013, by = "GEOID")
Join_table<-inner_join(x = White2013, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2013, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2013, y = Join_table, by = "GEOID")
Population_table2013<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'S1901_C01_013',
  year= 2013
)

#Ownership 2013
az_ownership2013 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2013,
  variables =  "DP04_0045")

az_occupied2013 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2013,
  variables =  "DP04_0044")


#Get elderly people more than 65 years
az_elderly2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0021',
  year= 2013)

#Get population with less than 5 years
az_child13 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2013)

#Get total population
az_total2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2013)

#Get female led household percentage
az_femhou131 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2013)

az_femhou132 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2013)

family2013 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2013)

Table2013<-inner_join(x = az_femhou131, y = az_femhou132, by = "GEOID")
Table2013<-inner_join(x = Table2013, y = az_child13, by = "GEOID")
Table2013<-inner_join(x = Table2013, y = family2013, by = "GEOID")
Table2013<-inner_join(x = Table2013, y = az_total2013, by = "GEOID")
write.csv(Table2013,"C:/Users/linab/Documents/Spring 2023/Thesis/table2013.csv", row.names = FALSE)

Join_table<-inner_join(x = Income2013, y = Population_table2013, by = "GEOID")
Join_table<-inner_join(x = az_ownership2013, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2013, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly2013, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female2013, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2013, y = Join_table, by = "GEOID")
Population_table2013<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2013,"C:/Users/linab/Documents/Thesis/Thesis/Population_table20132.csv", row.names = FALSE)

################################################################################
########################YEAR 2012###############################################

##---Get data a the neighborhood level---##

Neighborhood2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2012
  
)

#Hispanic##
Hispanic2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2012
  
)

#White##
White2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2012
  
)

#Black##
Black2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2012
  
)

#American Indian##
AmericanIndian2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2012
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2012, y = Black2012, by = "GEOID")
Join_table<-inner_join(x = White2012, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2012, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2012, y = Join_table, by = "GEOID")
Population_table2012<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'S1901_C01_013',
  year= 2012
)

#Ownership 2012
az_ownership2012 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2012,
  variables =  "DP04_0045")

az_occupied2012 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2012,
  variables =  "DP04_0044")


#Get elderly people more than 65 years
az_elderly2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0021',
  year= 2012)

#Get population with less than 5 years
az_child12 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2012)

#Get total population
az_total2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2012)

#Get Female led household percentage
az_femhou121 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2012)

az_femhou122 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2012)

family2012 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2012)

Table2012<-inner_join(x = az_femhou121, y = az_femhou122, by = "GEOID")
Table2012<-inner_join(x = Table2012, y = az_child12, by = "GEOID")
Table2012<-inner_join(x = Table2012, y = family2012, by = "GEOID")
Table2012<-inner_join(x = Table2012, y = az_total2012, by = "GEOID")
write.csv(Table2012,"C:/Users/linab/Documents/Spring 2023/Thesis/table2012.csv", row.names = FALSE)

Join_table<-inner_join(x = Income2012, y = Population_table2012, by = "GEOID")
Join_table<-inner_join(x = az_ownership2012, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2012, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly2012, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female2012, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2012, y = Join_table, by = "GEOID")
Population_table2012<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2012,"C:/Users/linab/Documents/Thesis/Thesis/Population_table20122.csv", row.names = FALSE)

################################################################################
########################YEAR 2011###############################################

##---Get data a the neighborhood level---##

Neighborhood2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2011
  
)

#Hispanic##
Hispanic2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2011
  
)

#White##
White2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2011
  
)

#Black##
Black2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2011
  
)

#American Indian##
AmericanIndian2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2011
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2011, y = Black2011, by = "GEOID")
Join_table<-inner_join(x = White2011, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2011, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2011, y = Join_table, by = "GEOID")
Population_table2011<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'S1901_C01_013',
  year= 2011
)

#Ownership 2011
az_ownership2011 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2011,
  variables =  "DP04_0045")

az_occupied2011 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2011,
  variables =  "DP04_0044")


#Get elderly people more than 65 years
az_elderly2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0021',
  year= 2011)

#Get population with less than 5 years
az_child11 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2011)

#Get total population
az_total2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2011)

#Get female led household percentage
az_femhou111 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2011)

az_femhou112 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2011)

family2011 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2011)

Table2011<-inner_join(x = az_femhou111, y = az_femhou112, by = "GEOID")
Table2011<-inner_join(x = Table2011, y = az_child11, by = "GEOID")
Table2011<-inner_join(x = Table2011, y = family2011, by = "GEOID")
Table2011<-inner_join(x = Table2011, y = az_total2011, by = "GEOID")
write.csv(Table2011,"C:/Users/linab/Documents/Spring 2023/Thesis/table2011.csv", row.names = FALSE)

Join_table<-inner_join(x = Income2011, y = Population_table2011, by = "GEOID")
Join_table<-inner_join(x = az_ownership2011, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2011, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly2011, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female2011, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2011, y = Join_table, by = "GEOID")
Population_table2011<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2011,"C:/Users/linab/Documents/Thesis/Thesis/Population_table20112.csv", row.names = FALSE)

################################################################################
########################YEAR 2010###############################################

##---Get data a the neighborhood level---##

Neighborhood2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2010
  
)

#Hispanic##
Hispanic2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2010
  
)

#White##
White2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2010
  
)

#Black##
Black2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2010
  
)

#American Indian##
AmericanIndian2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2010
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2010, y = Black2010, by = "GEOID")
Join_table<-inner_join(x = White2010, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2010, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2010, y = Join_table, by = "GEOID")
Population_table2010<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'S1901_C01_013',
  year= 2010
)

#Ownership 2010
az_ownership2010 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2010,
  variables =  "DP04_0045")

az_occupied2010 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2010,
  variables =  "DP04_0044")


#Get elderly people more than 65 years
az_elderly2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0021',
  year= 2010)

#Get population with less than 5 years
az_child10 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2010)

#Get total population
az_total2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2010)

#Get female led houehold percentage
az_femhou101 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2010)

az_femhou102 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2010)

family2010 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2010)

Table2010<-inner_join(x = az_femhou101, y = az_femhou102, by = "GEOID")
Table2010<-inner_join(x = Table2010, y = az_child10, by = "GEOID")
Table2010<-inner_join(x = Table2010, y = family2010, by = "GEOID")
Table2010<-inner_join(x = Table2010, y = az_total2010, by = "GEOID")
write.csv(Table2010,"C:/Users/linab/Documents/Spring 2023/Thesis/table2010.csv", row.names = FALSE)


Join_table<-inner_join(x = Income2010, y = Population_table2010, by = "GEOID")
Join_table<-inner_join(x = az_ownership2010, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2010, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly2010, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female2010, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2010, y = Join_table, by = "GEOID")
Population_table2010<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2010,"C:/Users/linab/Documents/Thesis/Thesis/Population_table20102.csv", row.names = FALSE)

################################################################################
########################YEAR 2009###############################################

##---Get data a the neighborhood level---##

Neighborhood2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2009
  
)

#Hispanic##
Hispanic2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2009
  
)

#White##
White2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2009
  
)

#Black##
Black2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2009
  
)

#American Indian##
AmericanIndian2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2009
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2009, y = Black2009, by = "GEOID")
Join_table<-inner_join(x = White2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2009, y = Join_table, by = "GEOID")
Population_table2009<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable = 'DP03_0064',
  year= 2009
)

#Ownership 2009
az_ownership2009 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2009,
  variables =  "DP04_0045")

az_occupied2009 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2009,
  variables =  "DP04_0044")

#Get elderly people more than 65 years
az_elderly2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0021',
  year= 2009)

#Get population with less than 5 years
az_child09 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0005',
  year= 2009)

#Get total population
az_total2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2009)

#Get female led household percentage
az_femhou091 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_013'),
  year= 2009)

az_femhou092 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_037'),
  year= 2009)

family2009 <-get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= c('B25011_001'),
  year= 2009)

Table2009<-inner_join(x = az_femhou091, y = az_femhou092, by = "GEOID")
Table2009<-inner_join(x = Table2009, y = az_child09, by = "GEOID")
Table2009<-inner_join(x = Table2009, y = family2009, by = "GEOID")
Table2009<-inner_join(x = Table2009, y = az_total2009, by = "GEOID")
write.csv(Table2009,"C:/Users/linab/Documents/Spring 2023/Thesis/table2009.csv", row.names = FALSE)


Join_table<-inner_join(x = Income2009, y = Population_table2009, by = "GEOID")
Join_table<-inner_join(x = az_ownership2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2009, y = Join_table, by = "GEOID")
Population_table2009<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2009,"C:/Users/linab/Documents/Thesis/Thesis/Population_table20092.csv", row.names = FALSE)


##2. Upload the excel files with arsenic copper and lead information##

##Arsenic##
#Read arsenic file
arsenicmax2<-read_xlsx(here::here("ArsenicMax2.xlsx"))

##Copper##
#Read copper file
coppermax<-read_xlsx(here::here("CopperMax.xlsx"))

##Lead##
#Read lead file
leadmax<-read_xlsx(here::here("LeadMax.xlsx"))

##3. Create correlation matrix##

arsenic_number<-dplyr::select_if(arsenicmax2, is.numeric)
M<-cor(arsenic_number)

C_number<-dplyr::select_if(coppermax, is.numeric)
N<-cor(C_number)

L_number<-dplyr::select_if(leadmax, is.numeric)
L<-cor(L_number)

# Correlation test
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation arsenic
p.mat <- cor.mtest(arsenic_number)
head(p.mat[, 1:13])

#Correlogram plot Arsenic
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")

# matrix of the p-value of the correlation Copper
p.matc <- cor.mtest(C_number)
head(p.matlc[, 1:13])

#Correlogram plot
corrplot(N, type="upper", order="hclust", 
         p.mat = p.matc, sig.level = 0.01, insig = "blank")

# matrix of the p-value of the correlation Lead
p.matl <- cor.mtest(l_number)
head(p.matl[, 1:13])

#Correlogram plot
corrplot(L, type="upper", order="hclust", 
         p.mat = p.matlc, sig.level = 0.01, insig = "blank")

#Summarize data
summary(arsenic_number)
summary(C_number)
summary(L_number)



#################################################################################
#################MODELS##########################################################
#################################################################################

###################ARSENIC######################################################

#1. PANEL DATA MODEL CONCENTRATIONS ARSENIC
#Arsenic mean Concentrations Fixed effects (Complete Model)
pmarsenicfix <- plm(Arsenicmean ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                    + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA +Superfund + Miningsite, data = arsenicmax2, index = c("ID","year"), model="within")
summary(pmarsenicfix)
#Arsenic mean Concentrations Random effects (Complete Model)
pmarsenicrandom <- plm(Arsenicmean ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                       + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA +Superfund + Miningsite, data = arsenicmax2, index = c("ID","year"), model="random")
summary(pmarsenicrandom)
phtest(pmarsenicrandom,pmarsenicfix)

#Arsenic max Concentrations Fixed effects (Complete Model)
pmarsenicmaxfix <- plm(ArsenicMax ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                    + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA +Superfund + Miningsite, data = arsenicmax2, index = c("ID","year"), model="within")
summary(pmarsenicmaxfix)
#Arsenic max Concentrations Random effects (Complete Model)
pmarsenicmaxrandom <- plm(ArsenicMax ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                       + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA +Superfund + Miningsite, data = arsenicmax2, index = c("ID","year"), model="random")
summary(pmarsenicmaxrandom)
phtest(pmarsenicmaxrandom,pmarsenicmaxfix)

#Test for heteroscedasticity: Prescence of Heteroskedasticity
bptest(Arsenicmean ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
       + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA +Superfund + Miningsite, data = arsenicmax2, studentize = F)

#Test for autocorrelation: Presence of Autocorrelation
pdwtest(pmarsenicrandom, data = arsenicmax2, model="random")
pbgtest(pmarsenicrandom, data = arsenicmax2)


#Controling in the estimation of the coefficients for heteroskedasticity and autocorrelation
coeftest(pmarsenicrandom, vcovHC(pmarsenicrandom, method="arellano"))

GWPR.moran.test(pmarsenicfix, cws, bw, adaptive = FALSE, p = 2,
                kernel = "bisquare", longlat = FALSE, alternative = "greater")

###################Copper##############################################

#COMPLETE MODEL: Mean

#Fixed Effects

pmcopperfix <- plm(copperMean ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat+ d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                   + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA	+ Superfund	+ Miningsite, data = coppermax, index = c("ID","year"), model="within")
summary(pmcopperfix)

#Random Effects
pmcopperrandom <- plm(copperMean ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                       + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA	+ Superfund	+ Miningsite, data = coppermax, index = c("ID","year"), model="random")
summary(pmcopperrandom)


phtest(pmcopperrandom,pmcopperfix)

###COMPLETE MODEL: Max

#Fixed Effects
pmcoppermaxfix <- plm(copperMax ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat+ d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                   + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA	+ Superfund	+ Miningsite, data = coppermax, index = c("ID","year"), model="within")
summary(pmcoppermaxfix)

#Random Effects
pmcoppermaxrandom <- plm(copperMax ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                      + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA	+ Superfund	+ Miningsite, data = coppermax, index = c("ID","year"), model="random")
summary(pmcoppermaxrandom)


phtest(pmcoppermaxrandom,pmcoppermaxfix)

#Test for heteroscedasticity: Prescence of Heteroskedasticity
bptest(copperMean ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Low_Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
       + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA +Superfund + Miningsite, data = coppermax, studentize = F)

#Test for autocorrelation: Presence of Autocorrelation
pdwtest(pmcopperrandom, data = coppermax, model="random")
pbgtest(pmcopperrandom, data = coppermax)


#Controling in the estimation of the coefficients for heteroskedasticity and autocorrelation
coeftest(pmcopperrandom, vcovHC(pmcopperrandom, method="arellano"))


###Lead#################################################################
##PANEL DATA MODEL CONCENTRATIONS 

#COMPLETE MODEL: Mean
#Fixed Effects

pmleadfix <- plm(LeadMean ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat+ d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                   + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA	+ Superfund	+ Miningsite, data = leadmax, index = c("ID","year"), model="within")
summary(pmleadfix)

#Random Effects
pmleadrandom <- plm(LeadMean ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                      + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA	+ Superfund	+ Miningsite, data = leadmax, index = c("ID","year"), model="random")
summary(pmleadrandom)


phtest(pmleadrandom ,pmleadfix)

###COMPLETE MODEL: Max
#Fixed Effects
pmleadmaxfix <- plm(LeadMax ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat+ d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                      + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA	+ Superfund	+ Miningsite, data = leadmax, index = c("ID","year"), model="within")
summary(pmleadmaxfix)

#Random Effects
pmleadmaxrandom <- plm(LeadMax ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
                         + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA	+ Superfund	+ Miningsite, data = leadmax, index = c("ID","year"), model="random")
summary(pmleadmaxrandom)


phtest(pmleadmaxrandom,pmleadmaxfix)

#Test for heteroscedasticity: Prescence of Heteroskedasticity
bptest(LeadMean ~ PERCENT_HISPANIC + PERCENT_BLACK + PERCENT_AMERICANINDIAN + Income + PERCENT_Elderly + PERCENT_FEMALELED + PERCENT_CHILD + PERCENT_Ownership_rate + d_groundwater + Sizecat + d_APACHE	+ d_COCHISE	+ d_COCONINO + d_GILA	+ d_GRAHAM	+ d_GREENLEE	
       + d_LAPAZ	+ d_MOHAVE	+ d_NAVAJO	+ d_PIMA	+ d_PINAL	+ d_SANTACRUZ	+ d_YAVAPAI	+ d_YUMA +Superfund + Miningsite, data = leadmax, studentize = F)

#Test for autocorrelation: Presence of Autocorrelation
pdwtest(pmleadrandom, data = leadmax, model="random")
pbgtest(pmleadrandom, data = leadmax)


#Controling in the estimation of the coefficients for heteroskedasticity and autocorrelation
coeftest(pmleadrandom, vcovHC(pmleadrandom, method="arellano"))




#################################################################################
#################SPATIAL AUTOCORRELATION#########################################
#################################################################################

###################ARSENIC######################################################

Arsenicno2022<-dplyr::filter(arsenicmax2, year!=2022)
Arsenic2022<-dplyr::filter(arsenicmax2, year==2022)
Arsenic2021<-dplyr::filter(arsenicmax2, year==2021)
Arsenic2020<-dplyr::filter(arsenicmax2, year==2020)
Arsenic2019<-dplyr::filter(arsenicmax2, year==2019)
Arsenic2018<-dplyr::filter(arsenicmax2, year==2018)
Arsenic2017<-dplyr::filter(arsenicmax2, year==2017)
Arsenic2016<-dplyr::filter(arsenicmax2, year==2016)
Arsenic2015<-dplyr::filter(arsenicmax2, year==2015)
Arsenic2014<-dplyr::filter(arsenicmax2, year==2014)
Arsenic2013<-dplyr::filter(arsenicmax2, year==2013)
Arsenic2012<-dplyr::filter(arsenicmax2, year==2012)
Arsenic2011<-dplyr::filter(arsenicmax2, year==2011)
Arsenic2010<-dplyr::filter(arsenicmax2, year==2010)
Arsenic2009<-dplyr::filter(arsenicmax2, year==2009)


#Find Moran test
arsenic.dists2022 <- as.matrix(dist(cbind(Arsenic2022$Longitude, Arsenic2022$Latitude)))
arsmaxinv2022<-1/arsenic.dists2022
diag(arsmaxinv2022)<-0
MoranArs2022<-Moran.I(Arsenic2022$ArsenicMax,arsmaxinv2022)


arsenic.dists2021 <- as.matrix(dist(cbind(Arsenic2021$Longitude, Arsenic2021$Latitude)))
arsmaxinv2021<-1/arsenic.dists2021
diag(arsmaxinv2021)<-0
MoranArs2021<-Moran.I(Arsenic2021$ArsenicMax,arsmaxinv2021)

arsenic.dists2020 <- as.matrix(dist(cbind(Arsenic2020$Longitude, Arsenic2020$Latitude)))
arsmaxinv2020<-1/arsenic.dists2020
diag(arsmaxinv2020)<-0
MoranArs2020<-Moran.I(Arsenic2020$ArsenicMax,arsmaxinv2020)

arsenic.dists2019 <- as.matrix(dist(cbind(Arsenic2019$Longitude, Arsenic2019$Latitude)))
arsmaxinv2019<-1/arsenic.dists2019
diag(arsmaxinv2019)<-0
MoranArs2019<-Moran.I(Arsenic2019$ArsenicMax,arsmaxinv2019)

arsenic.dists2018 <- as.matrix(dist(cbind(Arsenic2018$Longitude, Arsenic2018$Latitude)))
arsmaxinv2018<-1/arsenic.dists2018
diag(arsmaxinv2018)<-0
MoranArs2018<-Moran.I(Arsenic2018$ArsenicMax,arsmaxinv2018)

arsenic.dists2017 <- as.matrix(dist(cbind(Arsenic2017$Longitude, Arsenic2017$Latitude)))
arsmaxinv2017<-1/arsenic.dists2017
diag(arsmaxinv2017)<-0
MoranArs2017<-Moran.I(Arsenic2017$ArsenicMax,arsmaxinv2017)

arsenic.dists2016 <- as.matrix(dist(cbind(Arsenic2016$Longitude, Arsenic2016$Latitude)))
arsmaxinv2016<-1/arsenic.dists2016
diag(arsmaxinv2016)<-0
MoranArs2016<-Moran.I(Arsenic2016$ArsenicMax,arsmaxinv2016)

arsenic.dists2015 <- as.matrix(dist(cbind(Arsenic2015$Longitude, Arsenic2015$Latitude)))
arsmaxinv2015<-1/arsenic.dists2015
diag(arsmaxinv2015)<-0
MoranArs2015<-Moran.I(Arsenic2015$ArsenicMax,arsmaxinv2015)

arsenic.dists2014 <- as.matrix(dist(cbind(Arsenic2014$Longitude, Arsenic2014$Latitude)))
arsmaxinv2014<-1/arsenic.dists2014
diag(arsmaxinv2014)<-0
MoranArs2014<-Moran.I(Arsenic2014$ArsenicMax,arsmaxinv2014)

arsenic.dists2013 <- as.matrix(dist(cbind(Arsenic2013$Longitude, Arsenic2013$Latitude)))
arsmaxinv2013<-1/arsenic.dists2013
diag(arsmaxinv2013)<-0
MoranArs2013<-Moran.I(Arsenic2013$ArsenicMax,arsmaxinv2013)

arsenic.dists2012 <- as.matrix(dist(cbind(Arsenic2012$Longitude, Arsenic2012$Latitude)))
arsmaxinv2012<-1/arsenic.dists2012
diag(arsmaxinv2012)<-0
MoranArs2012<-Moran.I(Arsenic2012$ArsenicMax,arsmaxinv2012)

arsenic.dists2011 <- as.matrix(dist(cbind(Arsenic2011$Longitude, Arsenic2011$Latitude)))
arsmaxinv2011<-1/arsenic.dists2011
diag(arsmaxinv2011)<-0
MoranArs2011<-Moran.I(Arsenic2011$ArsenicMax,arsmaxinv2011)

arsenic.dists2010 <- as.matrix(dist(cbind(Arsenic2010$Longitude, Arsenic2010$Latitude)))
arsmaxinv2010<-1/arsenic.dists2010
diag(arsmaxinv2010)<-0
MoranArs2010<-Moran.I(Arsenic2010$ArsenicMax,arsmaxinv2010)

arsenic.dists2009 <- as.matrix(dist(cbind(Arsenic2009$Longitude, Arsenic2009$Latitude)))
arsmaxinv2009<-1/arsenic.dists2009
diag(arsmaxinv2009)<-0
MoranArs2009<-Moran.I(Arsenic2009$ArsenicMax,arsmaxinv2009)
#Table Moran Test
a<-MoranArs2009$p.value
b<-MoranArs2010$p.value
c<-MoranArs2011$p.value
d<-MoranArs2012$p.value
e<-MoranArs2013$p.value
f<-MoranArs2014$p.value
g<-MoranArs2015$p.value
h<-MoranArs2016$p.value
i<-MoranArs2017$p.value
j<-MoranArs2018$p.value
k<-MoranArs2019$p.value
l<-MoranArs2020$p.value
m<-MoranArs2021$p.value
n<-MoranArs2022$p.value

dfar<-data.frame(year=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022), pvaluemoranar=c(a,b,c,d,e,f,g,h,i,j,k,l,m,n))

write_xlsx(dfar,"C:/Users/linab/Documents//moran_arsenic.xlsx")

#GWRI Panel Data Moran Test
centroid<-read_xlsx(here::here("centroids.xlsx"))
coords= data.frame(x=centroid$XCentroid, y=centroid$YCentroid)
plot(coords)

install.packages("sp")
install.packages("rgdal")
library(sp)
library(rgdal)

sp=SpatialPoints(coords)
spdf = SpatialPointsDataFrame(coords, arsenicmax)

cws <- readOGR(
  dsn = paste0("C:/Users/linab/Documents/Fall 2022/Thesis/", "CWS Service Area"),
  layer = "CWS_Service_Area")

PolygonsCWS<-class(cws)

plot(cws)
cws$ADEQ_ID

###################LEAD######################################################
#Read and filter lead file
leadmax<-read_xlsx(here::here("LeadMax.xlsx"))
leadno2022<-dplyr::filter(leadmax, YEAR!=2022)
lead2022<-dplyr::filter(leadmax, YEAR==2022)
lead2021<-dplyr::filter(leadmax, YEAR==2021)
lead2020<-dplyr::filter(leadmax, YEAR==2020)
lead2019<-dplyr::filter(leadmax, YEAR==2019)
lead2018<-dplyr::filter(leadmax, YEAR==2018)
lead2017<-dplyr::filter(leadmax, YEAR==2017)
lead2016<-dplyr::filter(leadmax, YEAR==2016)
lead2015<-dplyr::filter(leadmax, YEAR==2015)
lead2014<-dplyr::filter(leadmax, YEAR==2014)
lead2013<-dplyr::filter(leadmax, YEAR==2013)
lead2012<-dplyr::filter(leadmax, YEAR==2012)
lead2011<-dplyr::filter(leadmax, YEAR==2011)
lead2010<-dplyr::filter(leadmax, YEAR==2010)
lead2009<-dplyr::filter(leadmax, YEAR==2009)


#Find Moran test
lead.dists2022 <- as.matrix(dist(cbind(lead2022$Longitude, lead2022$Latitude)))
leadmaxinv2022<-1/lead.dists2022
diag(leadmaxinv2022)<-0
Moranlead2022<-Moran.I(lead2022$LeadMax,leadmaxinv2022)


lead.dists2021 <- as.matrix(dist(cbind(lead2021$Longitude, lead2021$Latitude)))
leadmaxinv2021<-1/lead.dists2021
diag(leadmaxinv2021)<-0
Moranlead2021<-Moran.I(lead2021$LeadMax,leadmaxinv2021)

lead.dists2020 <- as.matrix(dist(cbind(lead2020$Longitude, lead2020$Latitude)))
leadmaxinv2020<-1/lead.dists2020
diag(leadmaxinv2020)<-0
Moranlead2020<-Moran.I(lead2020$LeadMax,leadmaxinv2020)

lead.dists2019 <- as.matrix(dist(cbind(lead2019$Longitude, lead2019$Latitude)))
leadmaxinv2019<-1/lead.dists2019
diag(leadmaxinv2019)<-0
Moranlead2019<-Moran.I(lead2019$LeadMax,leadmaxinv2019)

lead.dists2018 <- as.matrix(dist(cbind(lead2018$Longitude, lead2018$Latitude)))
leadmaxinv2018<-1/lead.dists2018
diag(leadmaxinv2018)<-0
Moranlead2018<-Moran.I(lead2018$LeadMax,leadmaxinv2018)

lead.dists2017 <- as.matrix(dist(cbind(lead2017$Longitude, lead2017$Latitude)))
leadmaxinv2017<-1/lead.dists2017
diag(leadmaxinv2017)<-0
Moranlead2017<-Moran.I(lead2017$LeadMax,leadmaxinv2017)

lead.dists2016 <- as.matrix(dist(cbind(lead2016$Longitude, lead2016$Latitude)))
leadmaxinv2016<-1/lead.dists2016
diag(leadmaxinv2016)<-0
Moranlead2016<-Moran.I(lead2016$LeadMax,leadmaxinv2016)

lead.dists2015 <- as.matrix(dist(cbind(lead2015$Longitude, lead2015$Latitude)))
leadmaxinv2015<-1/lead.dists2015
diag(leadmaxinv2015)<-0
Moranlead2015<-Moran.I(lead2015$LeadMax,leadmaxinv2015)

lead.dists2014 <- as.matrix(dist(cbind(lead2014$Longitude, lead2014$Latitude)))
leadmaxinv2014<-1/lead.dists2014
diag(leadmaxinv2014)<-0
Moranlead2014<-Moran.I(lead2014$LeadMax,leadmaxinv2014)

lead.dists2013 <- as.matrix(dist(cbind(lead2013$Longitude, lead2013$Latitude)))
leadmaxinv2013<-1/lead.dists2013
diag(leadmaxinv2013)<-0
Moranlead2013<-Moran.I(lead2013$LeadMax,leadmaxinv2013)

lead.dists2012 <- as.matrix(dist(cbind(lead2012$Longitude, lead2012$Latitude)))
leadmaxinv2012<-1/lead.dists2012
diag(leadmaxinv2012)<-0
Moranlead2012<-Moran.I(lead2012$LeadMax,leadmaxinv2012)

lead.dists2011 <- as.matrix(dist(cbind(lead2011$Longitude, lead2011$Latitude)))
leadmaxinv2011<-1/lead.dists2011
diag(leadmaxinv2011)<-0
Moranlead2011<-Moran.I(lead2011$LeadMax,leadmaxinv2011)

lead.dists2010 <- as.matrix(dist(cbind(lead2010$Longitude, lead2010$Latitude)))
leadmaxinv2010<-1/lead.dists2010
diag(leadmaxinv2010)<-0
Moranlead2010<-Moran.I(lead2010$LeadMax,leadmaxinv2010)

lead.dists2009 <- as.matrix(dist(cbind(lead2009$Longitude, lead2009$Latitude)))
leadmaxinv2009<-1/lead.dists2009
diag(leadmaxinv2009)<-0
Moranlead2009<-Moran.I(lead2009$LeadMax,leadmaxinv2009)

a<-Moranlead2009$p.value
b<-Moranlead2010$p.value
c<-Moranlead2011$p.value
d<-Moranlead2012$p.value
e<-Moranlead2013$p.value
f<-Moranlead2014$p.value
g<-Moranlead2015$p.value
h<-Moranlead2016$p.value
i<-Moranlead2017$p.value
j<-Moranlead2018$p.value
k<-Moranlead2019$p.value
l<-Moranlead2020$p.value
m<-Moranlead2021$p.value
n<-Moranlead2022$p.value

dflead<-data.frame(year=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022), pvaluemoranar=c(a,b,c,d,e,f,g,h,i,j,k,l,m,n))

write_xlsx(dflead,"C:/Users/linab/Documents/moran_lead.xlsx")

###################COPPER######################################################
copno2022<-dplyr::filter(coppermax, Year!=2022)
cop2022<-dplyr::filter(coppermax, Year==2022)
cop2021<-dplyr::filter(coppermax, Year==2021)
cop2020<-dplyr::filter(coppermax, Year==2020)
cop2019<-dplyr::filter(coppermax, Year==2019)
cop2018<-dplyr::filter(coppermax, Year==2018)
cop2017<-dplyr::filter(coppermax, Year==2017)
cop2016<-dplyr::filter(coppermax, Year==2016)
cop2015<-dplyr::filter(coppermax, Year==2015)
cop2014<-dplyr::filter(coppermax, Year==2014)
cop2013<-dplyr::filter(coppermax, Year==2013)
cop2012<-dplyr::filter(coppermax, Year==2012)
cop2011<-dplyr::filter(coppermax, Year==2011)
cop2010<-dplyr::filter(coppermax, Year==2010)
cop2009<-dplyr::filter(coppermax, Year==2009)


#Find Moran test
cop.dists2022 <- as.matrix(dist(cbind(cop2022$Longitude, cop2022$Latitude)))
copmaxinv2022<-1/cop.dists2022
diag(copmaxinv2022)<-0
Morancop2022<-Moran.I(cop2022$Max,copmaxinv2022)


cop.dists2021 <- as.matrix(dist(cbind(cop2021$Longitude, cop2021$Latitude)))
copmaxinv2021<-1/cop.dists2021
diag(copmaxinv2021)<-0
Morancop2021<-Moran.I(cop2021$Max,copmaxinv2021)

cop.dists2020 <- as.matrix(dist(cbind(cop2020$Longitude, cop2020$Latitude)))
copmaxinv2020<-1/cop.dists2020
diag(copmaxinv2020)<-0
Morancop2020<-Moran.I(cop2020$Max,copmaxinv2020)

cop.dists2019 <- as.matrix(dist(cbind(cop2019$Longitude, cop2019$Latitude)))
copmaxinv2019<-1/cop.dists2019
diag(copmaxinv2019)<-0
Morancop2019<-Moran.I(cop2019$Max,copmaxinv2019)

cop.dists2018 <- as.matrix(dist(cbind(cop2018$Longitude, cop2018$Latitude)))
copmaxinv2018<-1/cop.dists2018
diag(copmaxinv2018)<-0
Morancop2018<-Moran.I(cop2018$Max,copmaxinv2018)

cop.dists2017 <- as.matrix(dist(cbind(cop2017$Longitude, cop2017$Latitude)))
copmaxinv2017<-1/cop.dists2017
diag(copmaxinv2017)<-0
Morancop2017<-Moran.I(cop2017$Max,copmaxinv2017)

cop.dists2016 <- as.matrix(dist(cbind(cop2016$Longitude, cop2016$Latitude)))
copmaxinv2016<-1/cop.dists2016
diag(copmaxinv2016)<-0
Morancop2016<-Moran.I(cop2016$Max,copmaxinv2016)

cop.dists2015 <- as.matrix(dist(cbind(cop2015$Longitude, cop2015$Latitude)))
copmaxinv2015<-1/cop.dists2015
diag(copmaxinv2015)<-0
Morancop2015<-Moran.I(cop2015$Max,copmaxinv2015)

cop.dists2014 <- as.matrix(dist(cbind(cop2014$Longitude, cop2014$Latitude)))
copmaxinv2014<-1/cop.dists2014
diag(copmaxinv2014)<-0
Morancop2014<-Moran.I(cop2014$Max,copmaxinv2014)

cop.dists2013 <- as.matrix(dist(cbind(cop2013$Longitude, cop2013$Latitude)))
copmaxinv2013<-1/cop.dists2013
diag(copmaxinv2013)<-0
Morancop2013<-Moran.I(cop2013$Max,copmaxinv2013)

cop.dists2012 <- as.matrix(dist(cbind(cop2012$Longitude, cop2012$Latitude)))
copmaxinv2012<-1/cop.dists2012
diag(copmaxinv2012)<-0
Morancop2012<-Moran.I(cop2012$Max,copmaxinv2012)

cop.dists2011 <- as.matrix(dist(cbind(cop2011$Longitude, cop2011$Latitude)))
copmaxinv2011<-1/cop.dists2011
diag(copmaxinv2011)<-0
Morancop2011<-Moran.I(cop2011$Max,copmaxinv2011)

cop.dists2010 <- as.matrix(dist(cbind(cop2010$Longitude, cop2010$Latitude)))
copmaxinv2010<-1/cop.dists2010
diag(copmaxinv2010)<-0
Morancop2010<-Moran.I(cop2010$Max,copmaxinv2010)

cop.dists2009 <- as.matrix(dist(cbind(cop2009$Longitude, cop2009$Latitude)))
copmaxinv2009<-1/cop.dists2009
diag(copmaxinv2009)<-0
Morancop2009<-Moran.I(cop2009$Max,copmaxinv2009)

a<-Morancop2009$p.value
b<-Morancop2010$p.value
c<-Morancop2011$p.value
d<-Morancop2012$p.value
e<-Morancop2013$p.value
f<-Morancop2014$p.value
g<-Morancop2015$p.value
h<-Morancop2016$p.value
i<-Morancop2017$p.value
j<-Morancop2018$p.value
k<-Morancop2019$p.value
l<-Morancop2020$p.value
m<-Morancop2021$p.value
n<-Morancop2022$p.value

dfcop<-data.frame(year=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022), pvaluemoranar=c(a,b,c,d,e,f,g,h,i,j,k,l,m,n))

write_xlsx(dfcop,"C:/Users/linab/Documents/moran_cop.xlsx")

###################Graphics#####################


#Graphic ground water vs arsenic
ggplot(Arsemax, aes(x=d_groundwater, y=Arsenicmean)) +
  geom_point(size=2)
#Graphic size category vs arsenic
ggplot(Arsemax, aes(x=Sizecat, y=Arsenicmean)) +
  geom_point(size=2)

#Grafic hispanic vs arsenic
ggplot(Arsemax, aes(x=PERCENT_HISPANIC, y=ArsenicMax)) +
  geom_point(size=2)
ggplot(Arsemax, aes(x=PERCENT_HISPANIC, y=Arsenicmean)) +
  geom_point(size=2)
#Grafic white vs arsenic
ggplot(Arsemax, aes(x=PERCENT_WHITE, y=ArsenicMax)) +
  geom_point(size=2)
ggplot(Arsemax, aes(x=PERCENT_WHITE, y=Arsenicmean)) +
  geom_point(size=2)
#Grafic americanindian vs arsenic
ggplot(Arsemax, aes(x=PERCENT_AMERICANINDIAN, y=Arsenicmean)) +
  geom_point(size=2)

