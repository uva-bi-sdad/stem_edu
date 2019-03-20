#data quality for lat longs of openjobs
library(ggplot2)
library(ggmap)
library(lubridate)
library(data.table)
library(dplyr)
library(stringr)

data <- fread('./data/stem_edu/working/allOpenjobsParsed.csv')
summary(data)

### BEGIN MISSINGNESS VALIDATION
apply(data, 2, function(x) {sum(is.na(x))})

apply(data, 2, function(x) {sum(x == "")})

#begin checking for consistent data types
apply(data, 2, function(x) {length(unique(x))})

#strings in the numeric fields
typeof(data$datePosted)
