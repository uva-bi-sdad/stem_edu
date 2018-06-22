##############################################################
###              Merging IPEDS data                        ###
###       Degrees and Certifacations Conferred             ###
###                 Raghav Sawhney                         ###
##############################################################

library("dplyr")

library(readr)

# Import Data files = CSVs

c2012_a_rv <- read_csv("data/stem_edu/original/IPEDS_Data/7_Degree_Certif_Completion/c2012_a_rv.csv")
c2013_a_rv <- read_csv("data/stem_edu/original/IPEDS_Data/7_Degree_Certif_Completion/c2013_a_rv.csv")
c2014_a_rv <- read_csv("data/stem_edu/original/IPEDS_Data/7_Degree_Certif_Completion/c2014_a_rv.csv")
c2015_a_rv <- read_csv("data/stem_edu/original/IPEDS_Data/7_Degree_Certif_Completion/c2015_a_rv.csv")
c2016_a <- read_csv("data/stem_edu/original/IPEDS_Data/7_Degree_Certif_Completion/c2016_a.csv")
c2017_a <- read_csv("data/stem_edu/original/IPEDS_Data/7_Degree_Certif_Completion/c2017_a.csv")




# Add cols for year
c2012_a_rv$YEAR <- rep(2012, length(c2012_a_rv$UNITID))
c2013_a_rv$YEAR <- rep(2013, length(c2013_a_rv$UNITID))
c2014_a_rv$YEAR <- rep(2014, length(c2014_a_rv$UNITID))
c2015_a_rv$YEAR <- rep(2015, length(c2015_a_rv$UNITID))
c2016_a$YEAR <- rep(2016, length(c2016_a$UNITID))
c2017_a$YEAR <- rep(2017, length(c2017_a$UNITID))



# Join all 6 tables
temp <- full_join(c2012_a_rv, c2013_a_rv)
temp <- full_join(temp, c2014_a_rv)



temp <- full_join(temp, c2015_a_rv)
temp <- full_join(temp, c2016_a)

temp <- full_join(temp, c2017_a)


#assign new table of all years as ef2012_2016b
c2012_2017_a <- temp
rm(temp) #removes from environment

#Notice we have the correct number of observations
totRows <- nrow(c2012_a_rv) + nrow(c2013_a_rv) +
  nrow(c2014_a_rv) + nrow(c2015_a_rv) + nrow(c2016_a) + nrow(c2017_a)

colnames(c2012_2017_a)  #notice more columns from mismatching variables
levels(as.factor(c2012_2017_a$YEAR))

#Write new CSV to the working folder area
write.csv(x = c2012_2017_a,
          file = "data/stem_edu/working/DSPG18/IPEDS/c2012_2017_a.csv",
          row.names = FALSE)
head(c2012_2017_a)
