###########################################################
###              Merging IPEDS IC data                  ###
###    Institutional Characteristics & Edu Services     ###
###                 Raghav Sawhney                      ###
###########################################################

library("dplyr")

library(readr)

# Import Data files = CSVs
ic2012 <- read_csv("data/stem_edu/original/IPEDS_Data/2_Institutional_Char_Education_Services/ic2012.csv")
ic2013 <- read_csv("data/stem_edu/original/IPEDS_Data/2_Institutional_Char_Education_Services/ic2013.csv")
ic2014 <- read_csv("data/stem_edu/original/IPEDS_Data/2_Institutional_Char_Education_Services/ic2014.csv")
ic2015 <- read_csv("data/stem_edu/original/IPEDS_Data/2_Institutional_Char_Education_Services/ic2015.csv")
ic2016 <- read_csv("data/stem_edu/original/IPEDS_Data/2_Institutional_Char_Education_Services/ic2016.csv")
ic2017 <- read_csv("data/stem_edu/original/IPEDS_Data/2_Institutional_Char_Education_Services/ic2017.csv")

# Add cols for year
ic2012$YEAR <- rep(2012, length(ic2012$UNITID))
ic2013$YEAR <- rep(2013, length(ic2013$UNITID))
ic2014$YEAR <- rep(2014, length(ic2014$UNITID))
ic2015$YEAR <- rep(2015, length(ic2015$UNITID))
ic2016$YEAR <- rep(2016, length(ic2016$UNITID))
ic2017$YEAR <- rep(2017, length(ic2017$UNITID))


# Join all 6 tables
temp <- full_join(ic2012, ic2013)
temp <- full_join(temp, ic2014)




temp <- full_join(temp, ic2015)
temp <- full_join(temp, ic2016)


temp <- full_join(temp, ic2017)

#assign new table of all years as hd2012_2017
ic2012_2017 <- temp
rm(temp) #removes from environment

#Notice we have the correct number of observations
totRows <- nrow(ic2012) + nrow(ic2013) +
  nrow(ic2014) + nrow(ic2015) + nrow(ic2016) + nrow(ic2017)

colnames(ic2012_2017)  #notice more columns from mismatching variables
levels(as.factor(ic2012_2017$YEAR))

#Write new CSV to the working folder area
write.csv(x = ic2012_2017,
          file = "data/stem_edu/working/DSPG18/IPEDS/ic2012_2017.csv",
          row.names = FALSE)
head(ic2012_2017)
