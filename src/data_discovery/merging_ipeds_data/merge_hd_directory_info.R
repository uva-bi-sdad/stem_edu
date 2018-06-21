#########################################################
###          IPEDS Data - Code to Merge hd            ###
###  Institution Characteristics & Directory Info     ###
###                   Emily Sheen                     ###
#########################################################


library("dplyr")

library(readr)

# Import Data files = CSVs
hd2012 <- read_csv("data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2012.csv")
hd2013 <- read_csv("data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2013.csv")
hd2014 <- read_csv("data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2014.csv")
hd2015 <- read_csv("data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2015.csv")
hd2016 <- read_csv("data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2016.csv")
hd2017 <- read_csv("data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2017.csv")

# Add cols for year
hd2012$YEAR <- rep(2012, length(hd2012$UNITID))
hd2013$YEAR <- rep(2013, length(hd2013$UNITID))
hd2014$YEAR <- rep(2014, length(hd2014$UNITID))
hd2015$YEAR <- rep(2015, length(hd2015$UNITID))
hd2016$YEAR <- rep(2016, length(hd2016$UNITID))
hd2017$YEAR <- rep(2017, length(hd2017$UNITID))


# Join all 6 tables
temp <- full_join(hd2012, hd2013)
temp <- full_join(temp, hd2014)


#Notice that Lat and Long are character vectors in hd2015, convert to numeric
hd2015$LATITUDE <- as.numeric(hd2015$LATITUDE)
hd2015$LONGITUD <- as.numeric(hd2015$LONGITUD)

temp <- full_join(temp, hd2015)
temp <- full_join(temp, hd2016)

#Notice that Lat and Long are character vectors in hd2015, convert to numeric
hd2017$LATITUDE <- as.numeric(hd2017$LATITUDE)
hd2017$LONGITUD <- as.numeric(hd2017$LONGITUD)
temp <- full_join(temp, hd2017)

#assign new table of all years as hd2012_2017
hd2012_2017 <- temp
rm(temp) #removes from environment

#Notice we have the correct number of observations
totRows <- nrow(hd2012) + nrow(hd2013) +
  nrow(hd2014) + nrow(hd2015) + nrow(hd2016) + nrow(hd2017)

colnames(hd2012_2017)  #notice more columns from mismatching variables
levels(as.factor(hd2012_2017$YEAR))


