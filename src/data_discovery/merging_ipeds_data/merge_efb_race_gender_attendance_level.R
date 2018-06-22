#########################################################################
###              Merging IPEDS EF data                                ###
###    Fall Enrollment- Age category, gender, attendance and level    ###
###                 Raghav Sawhney                                    ###
#########################################################################

library("dplyr")

library(readr)

# Import Data files = CSVs

ef2012b <- read_csv("data/stem_edu/original/IPEDS_Data/5_Fall_Enrollment_Age_Gender_Attendance/ef2012b.csv")
ef2013b <- read_csv("data/stem_edu/original/IPEDS_Data/5_Fall_Enrollment_Age_Gender_Attendance/ef2013b.csv")
ef2014b <- read_csv("data/stem_edu/original/IPEDS_Data/5_Fall_Enrollment_Age_Gender_Attendance/ef2014b.csv")
ef2015b <- read_csv("data/stem_edu/original/IPEDS_Data/5_Fall_Enrollment_Age_Gender_Attendance/ef2015b.csv")
ef2016b <- read_csv("data/stem_edu/original/IPEDS_Data/5_Fall_Enrollment_Age_Gender_Attendance/ef2016b.csv")



# Add cols for year
ef2012b$YEAR <- rep(2012, length(ef2012b$UNITID))
ef2013b$YEAR <- rep(2013, length(ef2013b$UNITID))
ef2014b$YEAR <- rep(2014, length(ef2014b$UNITID))
ef2015b$YEAR <- rep(2015, length(ef2015b$UNITID))
ef2016b$YEAR <- rep(2016, length(ef2016b$UNITID))



# Join all 6 tables
temp <- full_join(ef2012b, ef2013b)
temp <- full_join(temp, ef2014b)




temp <- full_join(temp, ef2015b)
temp <- full_join(temp, ef2016b)




#assign new table of all years as ef2012_2016b
ef2012_2016b <- temp
rm(temp) #removes from environment

#Notice we have the correct number of observations
totRows <- nrow(ef2012b) + nrow(ef2013b) +
  nrow(ef2014b) + nrow(ef2015b) + nrow(ef2016b)

colnames(ef2012_2016b)  #notice more columns from mismatching variables
levels(as.factor(ef2012_2016b$YEAR))

#Write new CSV to the working folder area
write.csv(x = ef2012_2016b,
          file = "data/stem_edu/working/DSPG18/IPEDS/ef2012_2016b.csv",
          row.names = FALSE)
head(ef2012_2016b)
