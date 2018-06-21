#########################################################
###       IPEDS Data - Code to Merge eta_dist         ###
###      Enrollment Data and Distance Learning        ###
###                   Emily Sheen                     ###
#########################################################


library("dplyr")

library(readr)

# Import Data files = CSVs
efaDist2012 <- read_csv("data/stem_edu/original/IPEDS_Data/6_Fall_Enrollment_Dist_Educ_Levels/ef2012a_dist_rv.csv")
efaDist2013 <- read_csv("data/stem_edu/original/IPEDS_Data/6_Fall_Enrollment_Dist_Educ_Levels/ef2013a_dist_rv.csv")
efaDist2014 <- read_csv("data/stem_edu/original/IPEDS_Data/6_Fall_Enrollment_Dist_Educ_Levels/ef2014a_dist_rv.csv")
efaDist2015 <- read_csv("data/stem_edu/original/IPEDS_Data/6_Fall_Enrollment_Dist_Educ_Levels/ef2015a_dist_rv.csv")
efaDist2016 <- read_csv("data/stem_edu/original/IPEDS_Data/6_Fall_Enrollment_Dist_Educ_Levels/ef2016a_dist.csv")

# Add cols for year
efaDist2012$YEAR <- rep(2012, length(efaDist2012$UNITID))
efaDist2013$YEAR <- rep(2013, length(efaDist2013$UNITID))
efaDist2014$YEAR <- rep(2014, length(efaDist2014$UNITID))
efaDist2015$YEAR <- rep(2015, length(efaDist2015$UNITID))
efaDist2016$YEAR <- rep(2016, length(efaDist2016$UNITID))

which(colnames(efaDist2012) != colnames(efaDist2013))

# Join all 6 tables, noting all colnames match
temp <- full_join(efaDist2012, efaDist2013)
temp <- full_join(temp, efaDist2014)
temp <- full_join(temp, efaDist2015)
temp <- full_join(temp, efaDist2016)

#assign new table of all years as efaDist2012_2016
efaDist2012_2016 <- temp
rm(temp) #removes from environment

#Notice we have the correct number of observations
totRows <- nrow(efaDist2012) + nrow(efaDist2013) +
  nrow(efaDist2014) + nrow(efaDist2015) + nrow(efaDist2016)

colnames(efaDist2012_2016)  #column names pulled correctly
levels(as.factor(efaDist2012_2016$YEAR)) #joined all years of data
