# data exploration in R
library(dplyr)
library(readr)
library(stringr)
setwd("/Users/mrnemo/Tilburg Uni/dPrep(R)/Region_Mobility_Report_CSVs")
mobility <- read_csv("2020_NL_Region_Mobility_Report.csv")
# loading and inspecting the data

# Exercise 1
head(mobility)
summary(mobility$residential_percent_change_from_baseline)
table(mobility$country_region)
table(mobility$sub_region_1)

# exercise 2
filtered_data <- mobility %>% filter(is.na(sub_region_1) & is.na(sub_region_2))
summary(filtered_data)
View(filtered_data)

# exercise 3
summary(mobility$retail_and_recreation_percent_change_from_baseline)

# data cleaning and transformatiob
## exercise 4 - dropping columns
delete_df <- c('country_region_code', 'metro_area', 'iso_3166_2_code', 'census_fips_code')
mobility <- mobility %>% select(-delete_df)

## exercise 5 - renaming columns
updated_mobility = mobility %>% rename(retail_and_recreation = retail_and_recreation_percent_change_from_baseline, province = sub_region_1, city = sub_region_2) %>% rename_with(~str_remove(., '_percent_change_from_baseline'))
View(updated_mobility)

## exercise 6 - convert into date
class(mobility$date)
mobility$date <- as.Date(mobility$date)

min(mobility$date)
max(mobility$date)

## exercise 7 - adding a new column to updated_mobility
columns <- c('retail_and_recreation', 'grocery_and_pharmacy', 'parks', 'transit_stations', 'workplaces', 'residential') # create a list of all the variables that needs to be included
updated_mobility <- updated_mobility %>% mutate(avg_mobility2 = rowMeans(select(., all_of(columns)), na.rm =TRUE)) #use rowMeans() to calculate the mean of all the variables defined in the list above. The select() takes all the columns. 
# q: can you explain what happend in the code above?
# a: The mutate() function adds new columns to the dataframe. The rowMeans() function calculates the mean of the columns defined in the select() function. The all_of() function is used to select all the columns defined in the list. The na.rm = TRUE argument is used to remove any missing values from the calculation.

## exercise 8 - selecting a subdata set
subset_country <- updated_mobility %>% filter(is.na(city) & is.na(province))
# q; explain what happend here with the filter() and is.na() functions in the code above
# a: The filter() function is used to select rows from the dataframe. The is.na() function is used to check for missing values. In this case, the filter() function is used to select rows where both the city and province columns are missing.
subset_province <- updated_mobility %>% filter(is.na(city) & !is.na(province)) # to find country and province, filter on NA cities and !is.na provinces since you want a subset with provinces, country will be disregarded because it's already 'linked' to the province
subset_city <- updated_mobility %>% filter(!is.na(city)) # you want to filter it only on city, which !is.na, because you will get all the cities (which are not NA)

# exercise 9 - write a function
inspect_data <- function(df){
  cat('Generating some descriptive statistics...\n\n')
  cat('Data: ')
  cat(deparse(substitute(df)))
  cat('\n\n')
  
  cat('Summary statistics\n')
  print(summary(df))
  cat('\n\n')
  
  cat('Number of columns: ')
  cat(ncol(df))
  cat('\n\n')
  
  cat('Number of observations: ')
  cat(nrow(df))
  cat('\n\n')
  
  cat('Range of the data:\n')
  summary(df$date)
  cat('\n\n')
  
}

inspect_data(subset_country)
inspect_data(subset_province)
inspect_data(subset_city)

## exercise 10 - make the code snippet work
# Code snippet
missings <- function(df, cols) {
  sapply(cols, function(x) length(which(is.na(df[, x])))/nrow(df))
}

columns <- c('retail_and_recreation', 'grocery_and_pharmacy', 'parks', 'transit_stations', 'workplaces', 'residential')

# Another way to define columns is as follows using the 'grep' function:
# columns <- grep('retail|grocery|park|transit|workplace|residential', colnames(mobility),value=T)

missings(subset_country, columns)

result <- lapply(list(subset_country, subset_province, subset_city), missings, cols = columns)
# q: explain the code in line 91
# a: The lapply() function is used to apply the missings() function to each of the dataframes in the list. The missings() function calculates the proportion of missing values for each of the columns defined in the columns list. The result is a list of vectors, where each vector contains the proportion of missing values for each of the columns in the columns list.

# we can also nicely "bind" them together
bind_rows(result)

data.frame(dataset = c('country','province','city'), bind_rows(result))
# adding this line as a test
