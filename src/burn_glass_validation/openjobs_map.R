# This script will visualize placement of openjobs in virginia
library(ggplot2)
library(ggmap)
library(lubridate)
library(data.table)
library(dplyr)
data <- fread('./data/stem_edu/working/allOpenjobsParsed.csv')
summary(data)

#take 2016 dates
data_2016 <- data[as_date(data$datePosted) >= as_date('2016-01-01'),]
data_2016 <- data_2016[as_date(data_2016$datePosted) < as_date('2016-01-31'),]

#plotting lat and long with virginia
plot(data_2016$jobLocation_geo_longitude, data_2016$jobLocation_geo_latitude)

#ggmap background
api <- 'AIzaSyCqJUvDKU5BNB14AdIJFTvd8GTtNtW8MMg'
register_google(api)

va_map <- get_map(source = 'google', location = 'Virginia', crop = FALSE, zoom = 7)
va_map <- ggmap(va_map)
va_map

va_map + geom_point(data= data_2016, aes(x=data_2016$jobLocation_geo_longitude, y= data_2016$jobLocation_geo_latitude),
                    color = 'red')
#warning message prints that 8287 rows contain missing values
sum(is.na(data_2016$jobLocation_geo_latitude))
sum(is.na(data_2016$jobLocation_geo_longitude))
8287/8480

sum(is.na(data$jobLocation_geo_latitude))/nrow(data)
sum(is.na(data$jobLocation_geo_longitude))/nrow(data)
