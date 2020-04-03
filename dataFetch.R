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
library(httr)
library(jsonlite)
library(stringr)
library(readr)

### Functions ###
# List available data
listAvailable <- function() {
  url <- paste("https://api.github.com/repos/CSSEGISandData/COVID-19/contents/",
               "csse_covid_19_data/csse_covid_19_daily_reports", sep = "")
  request <- GET(url)
  json <- content(request, as = "text")
  validate(json)
  json <- fromJSON(json)
  return(json)
}

# Fetch based on pattern
fetch <- function(json, pattern) {
  fileList <- json %>% filter(str_detect(name, pattern))
  covidData <- lapply(fileList$download_url, read.csv) %>% bind_rows()
  return(covidData)
}

# Fetch all data from 3/22/20 forward
initialFetch <- function() {
  availableData <- listAvailable()
  pattern <- paste("(03-2[2-9]-2020.csv)|",
                   "(03-3[0-1]-2020.csv)|",
                   "(0[4-9]-[0-3][0-9]-2020.csv)|",
                   "(1[0-2]-[0-3][0-9]-2020.csv)|",
                   "([0-1][0-9]-[0-3][0-9]-202[1-9].csv)", sep = "")
  covidData <- fetch(availableData, pattern)
  return(covidData)
}

# Fetch applicable census data
# Requires a census API key stored as a single line in "Data/.censusAPIKey"
getCensusData <- (function() {
  key <- read_file("Data/.censusAPIKey")
  return(function(countyFIPS, stateFIPS) {
    url <- paste("https://api.census.gov/data/2019/pep/",
                 "population?get=DENSITY,POP&",
                 "for=county:",
                 paste(countyFIPS, collapse = ","),
                 "&in=state:",
                 stateFIPS,
                 "&key=",
                 key,
                sep = "")
    request <- GET(url)
    json <- content(request, as ="text")
    validate(json)
    json <- fromJSON(json)
    json <- as.data.frame(json)
    colnames(json) <- as.character(unlist(json[1,]))
    json <- tail(json, -1)
    return(json)
  })
})()

# Get density and population for each county
getPopDensity <- function(countyList) {
  popInfo <- countyList %>% filter(!is.na(County_FIPS)) %>%
    group_by(State_FIPS) %>%
    do(getCensusData(.$County_FIPS, max(.$State_FIPS))) %>%
    ungroup() %>% select(-State_FIPS) %>%
    rename(State_FIPS = "state",
           County_FIPS = "county")
  countyList <- countyList %>% merge(popInfo)
  return(countyList)
}