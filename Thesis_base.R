#Tittle: Chemical Contamination Arizona
# Author: Lina Benitez
# Initial Date: November 01 2022


##1. Download data from the Census 2020##

##---Install packages--##
install.packages("tidycensus")
install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("here")

##---api key--##

library(tidycensus)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
census_api_key("9cad7f1490792ca12565fe027f4f5f146ac6a799")

##---Get data a the neighborhood level---##

Neighborhood2010 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P1_001N',
  year= 2010
  
)

#Hispanic##
Hispanic2020 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'P2_002N',
  year= 2020
  
)

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

#Get ownership rate from ACS
#DP04_0046=Owner-Occupied households
#DP04_0045=Occupied housing units
year<-c("2015","2016")
for(x in year) {
  
  az_ownership2020 <- get_acs(
    geography = "tract",
    state = "AZ",
    year = 2020,
    variables =  "DP04_0046")
  
  az_occupied2020 <- get_acs(
    geography = "tract",
    state = "AZ",
    year = 2020,
    variables =  "DP04_0045")
  
}

#Create Table
Join_tableownership<-inner_join(x = az_ownership, y = az_occupied, by = "GEOID")
Ownership_table2020<- subset(Join_tableownership, select = -c(NAME.y,variable.x, variable.y) )

#Export population table
write.csv(Ownership_table2020,"C:/Users/linab/Documents/Thesis/Thesis/Ownership_2020.csv", row.names = FALSE)

#Get elderly people more than 65 years
az_elderly20 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0024',
  year= 2020)

#Get female population
az_female20 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0027',
  year= 2020)

#Get total population
az_total20 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2020)

#Create Table
Join_tableelderlysex<-inner_join(x = az_elderly20, y = az_female20, by = "GEOID")
Join_tableelderlysex<-inner_join(x = Join_tableelderlysex, y = az_total20, by = "GEOID")


#Export population table
write.csv(Join_tableelderlysex,"C:/Users/linab/Documents/Thesis/Thesis/Elderlysex_2020.csv", row.names = FALSE)

################################################################################
########################YEAR 2021###############################################

##---Get data a the neighborhood level---##

Neighborhood2021 <- get_acs(
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

#Get female population
az_female21 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0027',
  year= 2021)

#Get total population
az_total21 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2021)


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

#Get female population
az_female19 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0027',
  year= 2019)

#Get total population
az_total19 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2019)


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
  variable= 'DP05_0024',
  year= 2016)

#Get female population
az_female16 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0027',
  year= 2016)

#Get total population
az_total16 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2016)


Join_table<-inner_join(x = Income2016, y = Population_table2016, by = "GEOID")
Join_table<-inner_join(x = az_ownership2016, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2016, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly16, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female16, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total16, y = Join_table, by = "GEOID")
Population_table2016<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2016,"C:/Users/linab/Documents/Thesis/Thesis/Population_table2016.csv", row.names = FALSE)

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
  geography = "tract",
  state = "AZ",
  year = 2015,
  variables =  "DP04_0046")

az_occupied2015 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2015,
  variables =  "DP04_0045")


#Get elderly people more than 65 years
az_elderly2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0024',
  year= 2015)

#Get female population
az_female2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0027',
  year= 2015)

#Get total population
az_total2015 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2015)


Join_table<-inner_join(x = Income2015, y = Population_table2015, by = "GEOID")
Join_table<-inner_join(x = az_ownership2015, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2015, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly2015, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female2015, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2015, y = Join_table, by = "GEOID")
Population_table2015<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2015,"C:/Users/linab/Documents/Thesis/Thesis/Population_table2015.csv", row.names = FALSE)

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

#Get female population
az_female2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0003',
  year= 2014)

#Get total population
az_total2014 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2014)


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

#Get female population
az_female2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0003',
  year= 2013)

#Get total population
az_total2013 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2013)


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

#Get female population
az_female2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0003',
  year= 2012)

#Get total population
az_total2012 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2012)


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

#Get female population
az_female2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0003',
  year= 2011)

#Get total population
az_total2011 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2011)


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

#Get female population
az_female2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0003',
  year= 2010)

#Get total population
az_total2010 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2010)


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

#Get female population
az_female2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0003',
  year= 2009)

#Get total population
az_total2009 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2009)


Join_table<-inner_join(x = Income2009, y = Population_table2009, by = "GEOID")
Join_table<-inner_join(x = az_ownership2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female2009, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2009, y = Join_table, by = "GEOID")
Population_table2009<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2009,"C:/Users/linab/Documents/Thesis/Thesis/Population_table20092.csv", row.names = FALSE)

################################################################################
########################YEAR 2008###############################################

##---Get data a the neighborhood level---##

Neighborhood2008 <- get_decennial(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_001',
  year= 2010
  
)

#Hispanic##
Hispanic2008 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_012',
  year= 2008
  
)

#White##
White2008 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_003',
  year= 2008
  
)

#Black##
Black2008 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_004',
  year= 2008
  
)

#American Indian##
AmericanIndian2008 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'B03002_005',
  year= 2008
  
)

#Create table with all the total populations
Join_table<-inner_join(x = AmericanIndian2008, y = Black2008, by = "GEOID")
Join_table<-inner_join(x = White2008, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Hispanic2008, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = Neighborhood2008, y = Join_table, by = "GEOID")
Population_table2008<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y) )


#Income
Income2008 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable = 'DP03_0064',
  year= 2008
)

#Ownership 2008
az_ownership2008 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2008,
  variables =  "DP04_0045")

az_occupied2008 <- get_acs(
  geography = "tract",
  state = "AZ",
  year = 2008,
  variables =  "DP04_0044")

#Get elderly people more than 65 years
az_elderly2008 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0021',
  year= 2008)

#Get female population
az_female2008 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0003',
  year= 2008)

#Get total population
az_total2008 <- get_acs(
  geography = 'tract', 
  state= 'AZ',
  variable= 'DP05_0001',
  year= 2008)


Join_table<-inner_join(x = Income2008, y = Population_table2008, by = "GEOID")
Join_table<-inner_join(x = az_ownership2008, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_occupied2008, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_elderly2008, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_female2008, y = Join_table, by = "GEOID")
Join_table<-inner_join(x = az_total2008, y = Join_table, by = "GEOID")
Population_table2008<- subset(Join_table, select = -c(NAME.x,NAME.y,NAME.x.x,NAME.y.y, NAME.x.x.x, NAME.y.y.y) )

#Export population table
write.csv(Population_table2008,"C:/Users/linab/Documents/Thesis/Thesis/Population_table2008.csv", row.names = FALSE)

##2. Upload the excel files with the violations information##

install.packages("readxl")
library(readxl)
install.packages("corrplot")
library(corrplot)
##Arsenic##
Arsenic_file<-read_xlsx(here::here("ARSENIC0922.xlsx"))



##  Lead and copper##
LC_file<-read_xlsx("C:/Users/linab/Documents/Fall 2022/Thesis/Thesis/LC0921.xlsx")

##2. Create correlation matrix##

arsenic_number<-dplyr::select_if(Arsenic_file, is.numeric)
M<-cor(arsenic_number)

LC_number<-dplyr::select_if(LC_file, is.numeric)
N<-cor(LC_number)

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

#Correlogram plot
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")

# matrix of the p-value of the correlation L&C
p.matlc <- cor.mtest(LC_number)
head(p.matlc[, 1:13])

#Correlogram plot
corrplot(N, type="upper", order="hclust", 
         p.mat = p.matlc, sig.level = 0.01, insig = "blank")

#Summarize data
summary(arsenic_number)

sapply(arsenic_number, sd)


#################################################################################
#################MODELS##########################################################
#################################################################################


install.packages("pglm")
install.packages("texreg")
library(pglm)
library(texreg)


##Run logit model

data('arsenic_file', package = 'pglm')
anb <- pglm(ARSENIC ~ d_groundwater + d_medium + d_large + PERCENT_HISPANIC + PERCENT_WHITE + PERCENT_AMERICANINDIAN + Income + PERCENT_Elderly + PERCENT_Female + PERCENT_Ownership_rate, data=Arsenic_file, family = binomial('logit'),
            model = "pooling",index=c("CWS", "YEAR"),  method = "bfgs", print.level = 3, R = 5)
anblogit <- glm(ARSENIC ~ d_groundwater + d_medium + d_large + PERCENT_HISPANIC + PERCENT_WHITE + PERCENT_AMERICANINDIAN + Income + PERCENT_Elderly + PERCENT_Female + PERCENT_Ownership_rate, data=Arsenic_file, family = binomial('logit'))

summary(anb)


