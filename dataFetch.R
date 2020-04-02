### Libraries ###
library(dplyr)
library(httr)
library(jsonlite)
library(stringr)

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