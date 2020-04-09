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

### Project Functions ###
source("./dataFetch.R", encoding = "UTF-8")
source("./dataCleaning.R", encoding = "UTF-8")

### Functions ###
# Cleaning pipeline
processData <- function(covidData) {
  covidData <- covidData %>% cleanDataPostMar22()
  counties <- getAllCounties(covidData) %>% getPopDensity()
  covidData <- covidData %>% merge(counties)
  return(covidData)
}

# Fetch new data and merge with old
getNewData <- function(covidData) {
  dateList <- getDates(covidData)
  newData <- getNewPostMar22(dateList) %>% processData()
  covidData <- covidData %>% bind_rows(newData)
  return(covidData)
}

### Data Flow ###
covidData <- initialFetch() %>% processData()