# generating ggmap plots for 2016 of open jobs data
# This script will visualize placement of openjobs in virginia
library(ggplot2)
library(ggmap)
library(lubridate)
library(data.table)
library(dplyr)
data <- fread('./data/stem_edu/working/allOpenjobsParsed.csv')
summary(data)

#take 2016 dates
data_feb2016 <- data[as_date(data$datePosted) >= as_date('2016-02-01'),]
data_feb2016 <- data_feb2016[as_date(data_feb2016$datePosted) < as_date('2016-02-28'),]

#plotting lat and long with virginia
plot(data_feb2016$jobLocation_geo_longitude, data_2016$jobLocation_geo_latitude)
#still 96% missing
sum(is.na(data_feb2016$jobLocation_geo_latitude)) / nrow(data_feb2016)

#let's try march: looks good as we get 60k observations
mar2016 <- data[as_date(data$datePosted) >= as_date('2016-03-01'),]
mar2016 <- mar2016[as_date(mar2016$datePosted) < as_date('2016-03-31'),]
#checking NAs
sum(is.na(mar2016$jobLocation_geo_latitude)) / nrow(mar2016)
#two percent missing

plot(mar2016$jobLocation_geo_longitude, mar2016$jobLocation_geo_latitude)

#ggmapping for march
api <- 'AIzaSyCqJUvDKU5BNB14AdIJFTvd8GTtNtW8MMg'
register_google(api)

va_map <- get_map(source = 'google', location = 'Virginia', crop = T, zoom = 7, maptype = 'hybrid')
va_map <- ggmap(va_map)
va_map

va_map + geom_point(data= mar2016, aes(x=mar2016$jobLocation_geo_longitude, y= mar2016$jobLocation_geo_latitude),
                    color = 'red') + ggtitle('March 2016 Openjobs Locations')

