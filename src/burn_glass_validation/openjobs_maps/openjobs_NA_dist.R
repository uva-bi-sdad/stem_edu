#completeness for each month
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

#### Date Validity ####
# How many rows have values that don't make sense?
tab <- data.frame(table(data$datePosted))
barplot(height = tab$Freq, names.arg = tab$Var1)
#note that 3/17/2016, 09/09/2016, and 4/22/2016 have unusually high counts
#let's look at the distribution for those months
march <- data[as_date(data$datePosted) >= as_date('2016-03-01'),]
march <- march[as_date(march$datePosted) < as_date('2016-03-31'),]
m_tab <- data.frame(table(march$datePosted))
barplot(height = m_tab$Freq, names.arg = m_tab$Var1)

april <- data[as_date(data$datePosted) >= as_date('2016-04-01'),]
april <- april[as_date(april$datePosted) < as_date('2016-04-30'),]
a_tab <- data.frame(table(april$datePosted))
barplot(height = a_tab$Freq, names.arg = a_tab$Var1)

sep <- data[as_date(data$datePosted) >= as_date('2016-09-01'),]
sep <- sep[as_date(sep$datePosted) < as_date('2016-09-30'),]
s_tab <- data.frame(table(sep$datePosted))
barplot(height = s_tab$Freq, names.arg = s_tab$Var1)

# to conclude this section, any date analysis must be wary of these months

### Other Validity ####
#make sure unique identifiers are unique
#length of each identifier is supposed to be 32, so check that against sum of all character length
len <- length(unique(data$rawdata_id))
32*len == (sum(as.numeric(lapply(data$rawdata_id, nchar))))
#identifiers look good

### consistency (duplicates) ####
# check for duplicate rows by dropping unique identifier
data_dup <- data[,-1]
data_dup <- data_dup[duplicated(data_dup)]
nrow(data_dup)
# we see that there are 13555 duplicates overall
cleaned_data <- data %>% distinct(jobLocation_geo_latitude, jobLocation_geo_longitude, normalizedTitle_onetCode,
                                  normalizedTitle_onetName, datePosted, responsibilities, experienceRequirements,
                                  jobDescription, hiringOrg, .keep_all = TRUE)
#check to see the right amount of rows removed
nrow(data) - nrow(cleaned_data)
