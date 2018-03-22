library(data.table)
library(dplyr)
cbsaCountyCross = fread("./data/stem_edu/original/County-CBSA Crosswalk/cbsatocountycrosswalk2017.csv")

setkey(cbsaCountyCross, fipscounty, cbsa)

cbsaCountyCross[,.N, by = fipscounty]
dim(cbsaCountyCross)
length(unique(cbsaCountyCross$fipscounty))
filter(cbsaCountyCross, fipscounty == '06037')

# Conclusion: use fipscounty. 3271 fipscounties, 410 CBSAs. To compare to Datawork we aggregate JOLTS data to CBSAs.

# Data aggregations needed: for JOLTS, Datawork, and Openjobs, a table with CBSA/County/Month-Yr/#jobs/#stem jobs
metros = read.csv("./data/stem_edu/original/JOLTS/2017.q1-q2.by_area/2017.q1-q2 US000 U.S. TOTAL.csv")
tmp =  read.csv("./data/stem_edu/original/JOLTS/2017.q1-q2.by_area/2017.q1-q2 01001 Autauga County, Alabama.csv")
