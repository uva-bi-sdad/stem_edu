#########################################################
###          IPEDS Data - Exploratory Code            ###
###     2008 Cohort IPEDS Outcome Measures Data       ###
###                   Emily Sheen                     ###
#########################################################

#Import data
library(readr)
om2016 <- read_csv("data/stem_edu/original/IPEDS_Data/8_Student_Persistance_Success/om2016.csv")
View(om2016)

colnames(om2016)
