### Libraries ###
library(dplyr)
library(tidyr)
library(stringr)

### Functions ###
cleanDataPostMar22 <- function(covidData) {
  # Initial Cleaning
  covidData <- covidData %>%
    # Merge FIPS columns
    mutate(FIPS = as.character(FIPS), ï..FIPS = as.character(ï..FIPS)) %>%
    unite(FIPS, ï..FIPS, FIPS, na.rm=TRUE) %>%
    # Separate out date and time
    separate(Last_Update, into=c("Date", "Time"), sep=" ")
  
  # Fix dates in month/day/year format
  # Matches dates of form [m]m/[d]d/[yy]yy, where year is in 2020s
  pattern = "[0-1]?[0-9]/[0-3]?[0-9]/(20)?2[0-9]"
  monthDayYear <- covidData %>% filter(str_detect(Date, pattern)) %>%
    # Separate date into month, day, and year
    separate(Date, into = c("Month", "Day", "Year"), sep = "/") %>%
    # Force year to 4 digits, month to 2 digits, and day to 2 digits
    mutate(Year = ifelse(str_length(Year) == 2, paste("20", Year, sep = ""), Year)) %>%
    mutate(Month = ifelse(str_length(Month) == 1, paste("0", Month, sep = ""), Month)) %>%
    mutate(Day = ifelse(str_length(Day) == 1, paste("0", Day, sep = ""), Day)) %>%
    # Convert back to year-month-day format
    unite(Date, Year, Month, Day, sep = "-")
  
  # Merge back into original data and convert date column to date
  covidData <- covidData %>% filter(!str_detect(Date, pattern)) %>%
    bind_rows(monthDayYear) %>%
    mutate(Date = as.Date(Date))
  
  # Filter out non-US
  covidData <- covidData %>% filter(Country_Region == "US") %>%
    # Rename Admin2 to County, Lat to Latitude, Long_ to Longitude
    rename(County = "Admin2", Latitude = "Lat", Longitude = "Long_") %>%
    # Drop Active column and Time column
    select(-Active, -Time) %>%
    # Filter out cruise ships
    filter(Province_State != "Diamond Princess") %>%
    filter(Province_State != "Grand Princess") %>%
    # Filter out recovered stats (which only are available for whole country)
    filter(Province_State != "Recovered") %>%
    select(-Recovered) %>%
    # Filter out Wuhan Evacuee
    filter(Province_State != "Wuhan Evacuee")
  
  return(covidData)
}