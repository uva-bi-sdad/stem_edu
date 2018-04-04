library(data.table)
library(dplyr)
library(lubridate)

# Load open jobs data, remove long string columns for ease of use.

openJobs = fread("./data/stem_edu/working/allOpenjobsParsed.csv")
colnames(openJobs)[2:3] = c("tlats", 'tlons')

# Load in the block codes atc for all the lat/longs from above

geocodedJobs = fread("./data/stem_edu/working/allOpenjobsGeocoded.csv")

# Merge the two

mergedJobsWithDesc = openJobs[geocodedJobs, on = c("tlats", "tlons")]

geomergedJobs = mergedJobsWithDesc[,-c("responsibilities", "experienceRequirements", "jobDescription")]
rm(openJobs, geocodedJobs)

geomergedJobs$month = month(date(geomergedJobs$datePosted))
geomergedJobs$year = year(date(geomergedJobs$datePosted))

# Make classification columns for each onet code according to the crosswalk produced by Bianica

cw = fread("./data/stem_edu/original/occupation_cw.csv", header = T, stringsAsFactors = F)[,-1]

cw$nsf_stem <- recode(cw$nsf_se, 'Non-S&E and Other' = "Non STEM",'S&E Occupations' = "STEM", 'S&E-Related Occupations' = "STEM Related")
cw$rothwell_stem <- recode(cw$rothwell_classification, "Other" = "Non STEM",'High-STEM' = "STEM Related", "Super-STEM" = "STEM")
cw$soc_stem2 <- recode(cw$soc_stem, "Non-STEM" = "Non STEM", "STEM-related" = "STEM Related", "STEM" = "STEM")

crosswalk = select(cw, onet_soc_code, onet_soc_title, soc_stem2, nsf_stem, rothwell_stem)
rm(cw)

# Merge with crosswalk data then clean up some columns

jobsAndCrosswalk = geomergedJobs[crosswalk, on = c("normalizedTitle_onetCode" = "onet_soc_code"), nomatch = 0]
jobsAndCrosswalk = jobsAndCrosswalk[,-c("tlats", "tlons", 'block_fips', 'onet_soc_title')]

# Add CBSA Data

cbsaCtyXwalk = fread("./data/stem_edu/original/County-CBSA Crosswalk/cbsatocountycrosswalk2017.csv")
cbsaCtyXwalk$fipscounty = as.numeric(cbsaCtyXwalk$fipscounty)

jobsStemcwCBSA = jobsAndCrosswalk[cbsaCtyXwalk[,c("fipscounty", "cbsa")], on = c("county_fips" = 'fipscounty'), nomatch = 0]

# Conclusion: use fipscounty. 3271 fipscounties, 410 CBSAs. To compare to Datawork we aggregate JOLTS data to CBSAs.

# Data aggregations needed: for JOLTS, Datawork, and Openjobs, a table with CBSA/County/Month-Yr/#jobs/#stem jobs

jobsByCounty = na.omit(jobsStemcwCBSA[, .(nJobs = .N,
                     nSocStem = sum(soc_stem2 == "STEM"),
                     nNsfStem = sum(nsf_stem == "STEM"),
                     nRothStem = sum(rothwell_stem == "STEM")),
                 by = .(year, month, county_fips)])

jobsByCbsa = na.omit(jobsStemcwCBSA[, .(nJobs = .N,
                                  nSocStem = sum(soc_stem2 == "STEM"),
                                  nNsfStem = sum(nsf_stem == "STEM"),
                                  nRothStem = sum(rothwell_stem == "STEM")),
                              by = .(year, month, cbsa)])

fwrite(jobsByCounty, "./data/stem_edu/working/openDataJobsCounty.csv")
fwrite(jobsByCbsa, "./data/stem_edu/working/openDataJobsCbsa.csv")


#
# To compare to DAW, we need to aggregate the OJ data by month and by CBSA in Virginia
#

jobsDescCbsa = mergedJobsWithDesc[cbsaCtyXwalk[,c("fipscounty", "cbsa")], on = c("county_fips" = 'fipscounty'), nomatch = 0][,c("tlats", "tlons", "normalizedTitle_onetCode","normalizedTitle_onetName", "datePosted", "responsibilities", "experienceRequirements", "jobDescription", "hiringOrg", "cbsa")]
jobsDescCbsa$datePosted = as_date(jobsDescCbsa$datePosted)

# Missing CBSAs are characte(0)'s instead of NAs. This removes them.
# Keeping CBSAs with more than 100 records, since there was a big gap between a CBSA with 90 and a CBSA with 2780.g
jobsDescCbsa = jobsDescCbsa[nchar(cbsa) > 1]
cbsasToKeep = jobsDescCbsa[, .N > 100, by = "cbsa"][V1 == TRUE, cbsa]
jobsDescCbsa = jobsDescCbsa[cbsa %in% cbsasToKeep]

# Create a column for quarter

jobsDescCbsa$quarter = sprintf("%s Q%s",year(jobsDescCbsa$datePosted), quarter(jobsDescCbsa$datePosted))

# Determine if 2 postings are the same by comparing their descriptions. First remove all exact job desc matches
# For ass-covering purposes, also check matches along the other columns with &

rows = jobsDescCbsa[,!duplicated(jobDescription), by = c("quarter", "cbsa")]$V1
r1 = jobsDescCbsa[,unique(jobDescription, by = "normalizedTitle_onetCode"), by = c("quarter", "cbsa")]
sum(rows)
dim(r1)





