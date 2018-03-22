library(data.table)
library(dplyr)
library(lubridate)

# Load open jobs data, remove long string columns for ease of use.

openJobs = fread("./data/stem_edu/working/allOpenjobsParsed.csv")[,-c("responsibilities", "experienceRequirements", "jobDescription")]
colnames(openJobs)[2:3] = c("tlats", 'tlons')

# Load in the block codes atc for all the lat/longs from above

geocodedJobs = fread("./data/stem_edu/working/allOpenjobsGeocoded.csv")

# Merge the two

geomergedJobs = openJobs[geocodedJobs, on = c("tlats", "tlons")]
rm(openJobs, geocodedJobs)

geomergedJobs$month = month(date(geomergedJobs$datePosted))
geomergedJobs$year = year(date(geomergedJobs$datePosted))

# Make classification columns for each onet code according to the crosswalk produced by Bianica

cw = fread("./data/stem_edu/original/occupation_cw.csv", header = T, stringsAsFactors = F)[,-1]
cw$nsf_stem <- ifelse(cw$nsf_se!="Non-S&E and Other","STEM","Non-STEM")
cw$rothwell_stem <- ifelse(cw$rothwell_classification!="Other","STEM","Non-STEM")
cw$soc_stem2 <- ifelse(cw$soc_stem!="Non-STEM","STEM","Non-STEM")

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




