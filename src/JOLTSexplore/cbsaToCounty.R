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
