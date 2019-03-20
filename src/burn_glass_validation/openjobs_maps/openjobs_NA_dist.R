#completeness for each month and map for each month
library(ggplot2)
library(ggmap)
library(lubridate)
library(data.table)
library(dplyr)
data <- fread('./data/stem_edu/working/allOpenjobsParsed.csv')
summary(data)

#take 2016 dates
data_2016 <- data[as_date(data$datePosted) >= as_date('2016-01-01'),]
data_2016 <- data_2016[as_date(data_2016$datePosted) < as_date('2016-12-31'),]

#get month
data_2016$month <- lubridate::month(as_date(data_2016$datePosted))

#check na distribution in
na_month <- aggregate(jobLocation_geo_latitude ~ month, data=data_2016, function(x) {sum(is.na(x))}, na.action = na.pass)
#to get percent divide by how many observations in that month
na_month$jobLocation_geo_latitude <- na_month$jobLocation_geo_latitude / count(data_2016,month)$n
na_month

#take 2017 dates
data_2017 <- data[as_date(data$datePosted) >= as_date('2017-01-01'),]
data_2017 <- data_2017[as_date(data_2017$datePosted) < as_date('2017-12-31'),]
#get month 2017
data_2017$month <- lubridate::month(as_date(data_2017$datePosted))
#check na distribution in month
na_month_2 <- aggregate(jobLocation_geo_latitude ~ month, data=data_2017, function(x) {sum(is.na(x))}, na.action = na.pass)
#to get percent divide by how many observations in that month
na_month_2$jobLocation_geo_latitude <- na_month_2$jobLocation_geo_latitude / count(data_2017,month)$n
na_month_2
