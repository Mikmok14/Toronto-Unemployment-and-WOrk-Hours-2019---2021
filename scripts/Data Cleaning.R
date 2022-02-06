#### Premable ####
# Purpose: Clean data downloaded from https://open.toronto.ca/dataset/toronto-s-dashboard-key-indicators/
#Author: Marco Chau
#Date: 16 January 2022
#Contact: marco.chau@mail.utoronto.ca

#### Workspace setup ####
#Use R Projects
library(tidyverse)
library(dplyr)
library(opendatatoronto)
library(lubridate)
library(ggplot2)
library(knitr)
library(kableExtra)

### Read in raw data ###
raw_ecodat <- readr::read_csv("inputs/data/Toronto Key Metrics.csv"
                              )

### Isolating employment data ###
clean_ecodat <-
  raw_ecodat|>
  filter(measure_name == "Unemployment Rate (Toronto Residents)" | measure_name == "Average Actual Hours at Main Job (worked in reference week)")|>
  filter(year %in%(2019:2021))|>
  select(measure_name, measure_value, year, period_number_in_year)|>
  rename(
    "Year" = year,
    "Month" = period_number_in_year
  )
clean_ecodat$Date <- as.Date(paste(clean_ecodat$Year, clean_ecodat$Month, 01), "%Y %m %d")


clean_ecodat$Month[clean_ecodat$Month == "1"] <- "January"
clean_ecodat$Month[clean_ecodat$Month == "2"] <- "February"
clean_ecodat$Month[clean_ecodat$Month == "3"] <- "March"
clean_ecodat$Month[clean_ecodat$Month == "4"] <- "April"
clean_ecodat$Month[clean_ecodat$Month == "5"] <- "May"
clean_ecodat$Month[clean_ecodat$Month == "6"] <- "June"
clean_ecodat$Month[clean_ecodat$Month == "7"] <- "July"
clean_ecodat$Month[clean_ecodat$Month == "8"] <- "August"
clean_ecodat$Month[clean_ecodat$Month == "9"] <- "September"
clean_ecodat$Month[clean_ecodat$Month == "10"] <- "October"
clean_ecodat$Month[clean_ecodat$Month == "11"] <- "November"
clean_ecodat$Month[clean_ecodat$Month == "12"] <- "December"

### Isolating Unemployment Data ###
employ_dat<-
  clean_ecodat|>
  filter(measure_name == "Unemployment Rate (Toronto Residents)")|>
  select(measure_value, Year, Month, Date)|>
  rename(Unemployment_Rate = measure_value)
employ_dat$Unemployment_Rate = employ_dat$Unemployment_Rate*100
employ_dat<- employ_dat[c("Year", "Month", "Date","Unemployment_Rate")]

###Isolating average hours worked###
work_dat <-
  clean_ecodat|>
  filter(measure_name == "Average Actual Hours at Main Job (worked in reference week)")|>
  select(measure_value, Year, Month)|>
  rename("Average_Work_Hours" = measure_value)

### Merging unemployment rate dataframe and weekly hours worked dataframes ###
employ_work_dat <- employ_dat[,c("Unemployment_Rate", "Year", "Month", "Date")]
employ_work_dat["Average_Work_Hours"] = work_dat["Average_Work_Hours"]
employ_work_dat <- employ_work_dat[,c(2, 3, 4, 1, 5)]

### Calculating rate of change and adding it to dataframe ###
employ_work_dat <- 
  employ_work_dat |>
  mutate(Unemployment_ROC = 100*(Unemployment_Rate - lag(Unemployment_Rate))/lag(Unemployment_Rate))
employ_work_dat$Unemployment_ROC <- round(employ_work_dat$Unemployment_ROC, digit = 2)

employ_work_dat<-
  employ_work_dat|>
  mutate(Workhours_ROC = 100*(Average_Work_Hours - lag(Average_Work_Hours))/lag(Average_Work_Hours))
employ_work_dat$Workhours_ROC <- round(employ_work_dat$Workhours_ROC, digit = 2)

### Saving data to directory for reproducibility ###
save(employ_work_dat, file = "employ_work_dat.rda")