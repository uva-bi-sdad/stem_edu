#########################################################
###        IPEDS Data - Code to Merge ef_a            ###
###     Fall Enrollment by Race, ethnicity, gender    ###
###                   Emily Sheen                     ###
#########################################################

library("dplyr")

library(readr)

# Import Data files = CSVs
efa2012 <- read_csv("data/stem_edu/original/IPEDS_Data/4_Fall_Enrollment_Race_Ethn_Gender_Level/ef2012a_rv.csv")
efa2013 <- read_csv("data/stem_edu/original/IPEDS_Data/4_Fall_Enrollment_Race_Ethn_Gender_Level/ef2013a_rv.csv")
efa2014 <- read_csv("data/stem_edu/original/IPEDS_Data/4_Fall_Enrollment_Race_Ethn_Gender_Level/ef2014a_rv.csv")
efa2015 <- read_csv("data/stem_edu/original/IPEDS_Data/4_Fall_Enrollment_Race_Ethn_Gender_Level/ef2015a_rv.csv")
efa2016 <- read_csv("data/stem_edu/original/IPEDS_Data/4_Fall_Enrollment_Race_Ethn_Gender_Level/ef2016a.csv")
#Note 2017 is not available yet

#Notice all the column names in this set are same, unlike hd data
which(colnames(efa2012) != colnames(efa2013))

# Add cols for year
efa2012$YEAR <- rep(2012, length(efa2012$UNITID))
efa2013$YEAR <- rep(2013, length(efa2013$UNITID))
efa2014$YEAR <- rep(2014, length(efa2014$UNITID))
efa2015$YEAR <- rep(2015, length(efa2015$UNITID))
efa2016$YEAR <- rep(2016, length(efa2016$UNITID))

# Join all 5 tables
temp <- full_join(efa2012, efa2013)
temp <- full_join(temp, efa2014)
temp <- full_join(temp, efa2015)
temp <- full_join(temp, efa2016)

#assign new table of all years as efa2012_2016
efa2012_2016 <- temp
rm(temp) #removes temp from environment

#Notice we have the correct number of observations
totRows <- nrow(efa2012) + nrow(efa2013) + nrow(efa2014) +
  nrow(efa2015) + nrow(efa2016)

colnames(efa2012_2016)  #same as for indiv. tables
levels(as.factor(efa2012_2016$YEAR))  #all the years are in new dataframe


