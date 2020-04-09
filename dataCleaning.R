# Copyright 2020 Alexander Mohn
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

### Libraries ###
library(dplyr)
library(tidyr)
library(stringr)

### Functions ###
cleanDataPostMar22 <- function(covidData) {
  # Load in state FIPS codes
  stateFIPS <- read.csv("Data/StateFIPS.csv", colClasses = c("FIPS" = "character"))
  
  # Initial Cleaning
  covidData <- covidData %>%
    # Merge FIPS columns
    mutate(FIPS = as.character(FIPS), ï..FIPS = as.character(ï..FIPS)) %>%
    unite(FIPS, ï..FIPS, FIPS, na.rm=TRUE) %>%
    # Make FIPS code include all 5 digits
    mutate(FIPS = as.character(FIPS)) %>%
    mutate(FIPS = ifelse(str_length(FIPS) != 5, paste("0", FIPS, sep = ""), FIPS)) %>%
    # Separate FIPS into state and county
    mutate(County_FIPS = substr(FIPS, 3, 5)) %>%
    mutate(State_FIPS = substr(FIPS, 1, 2)) %>%
    select(-FIPS) %>%
    # Fix state FIPS codes
    merge(stateFIPS, by.x = "Province_State", by.y = "State") %>%
    mutate(State_FIPS = ifelse(State_FIPS != FIPS, FIPS, State_FIPS)) %>%
    select(-FIPS) %>%
    # Set unknown counties to NA
    mutate(County_FIPS = ifelse(County_FIPS == "", NA, County_FIPS)) %>%
    # Get dates
    select(-Last_Update) %>%
    mutate(Date = substr(Filename, nchar(Filename) - 13, nchar(Filename) - 4)) %>%
    select(-Filename) %>%
    mutate(Date = as.Date(Date, "%m-%d-%y"))
  
  # Filter out non-US
  covidData <- covidData %>% filter(Country_Region == "US") %>%
    # Rename Admin2 to County, Lat to Latitude, Long_ to Longitude
    rename(County = "Admin2", Latitude = "Lat", Longitude = "Long_") %>%
    # Drop Active column
    select(-Active) %>%
    # Filter out cruise ships
    filter(Province_State != "Diamond Princess") %>%
    filter(Province_State != "Grand Princess") %>%
    # Filter out recovered stats (which only are available for whole country)
    filter(Province_State != "Recovered") %>%
    select(-Recovered) %>%
    # Filter out Wuhan Evacuee
    filter(Province_State != "Wuhan Evacuee") %>%
    # Filter out territories (Census API has no info on them)
    filter(County != "")
  
  # Fix missing FIPS codes
  missing <- covidData %>% filter(is.na(County_FIPS)) %>%
    mutate(County_FIPS = mapply(getFIPS, County, as.numeric(State_FIPS))) %>%
    mutate(County_FIPS = as.character(County_FIPS)) %>%
    mutate(County_FIPS = ifelse(str_length(County_FIPS) < 3,
                                ifelse(str_length(County_FIPS) == 2,
                                       paste("0", County_FIPS, sep = ""),
                                       paste("00", County_FIPS, sep = "")),
                                County_FIPS))
  covidData <- covidData %>% filter(!is.na(County_FIPS)) %>%
    bind_rows(missing)
  
  return(covidData)
}

# Find missing county FIPS codes
getFIPS <- (function() {
  lookupTbl <- read.csv("Data/all-geocodes-v2017.csv", encoding = "UTF-8") %>%
    filter(X.U.FEFF.Summary.Level == 50)
  return (function(name, stFIPS) {
    # Select all counties in state
    inState <- lookupTbl[lookupTbl$State.Code..FIPS. == stFIPS,]
    names <- inState$Area.Name..including.legal.statistical.area.description.
    # Fuzzy string match county name to state counties
    idx <- agrep(name, names)
    # Get county FIPS code(s)
    result <- inState$County.Code..FIPS.[idx]
    if(length(result) > 1) {
      result <- result[1]
    } else if(length(result) == 0) {
      result <- NA
    }
    return(result)
  })
})()

# Get list of all dates
getDates <- function(covidData) {
  dateList <- covidData %>% select(Date) %>% unique()
    mutate(Date = as.character.Date(Date, "%m-%d-%Y"))
  return(dateList)
}

# Get list of all counties
getAllCounties <- function(covidData) {
  result <- covidData %>% group_by(County_FIPS, State_FIPS) %>%
    select(County, Province_State, County_FIPS, State_FIPS) %>%
    unique() %>% ungroup() %>% filter(!is.na(County_FIPS))
  return(result)
}