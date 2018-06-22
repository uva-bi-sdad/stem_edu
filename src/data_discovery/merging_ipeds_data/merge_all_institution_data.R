#########################################################
###  IPEDS Data - Code to Merge All institution data  ###
###  Fall Enrollment & Institutional characteristics  ###
###      Unique observations are UNITID + YEAR        ###
###                   Emily Sheen                     ###
#########################################################


library("dplyr")
library(readr)

# Import Data files = CSVs
hd2012_2017 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/hd2012_2017.csv")
ic2012_2017 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/ic2012_2017.csv")
efa2012_2016 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/efa2012_2016.csv")
efb2012_2016 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/efb2012_2016.csv")
efaDist2012_2016 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/efaDist2012_2016.csv")

