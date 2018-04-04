library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(maptools)

openJobs = fread("./data/stem_edu/working/allOpenjobsParsed.csv")
colnames(openJobs)[2:3] = c("tlats", 'tlons')
geocodedJobs = fread("./data/stem_edu/working/allOpenjobsGeocoded.csv")
mergedJobsWithDesc = openJobs[geocodedJobs, on = c("tlats", "tlons")]
cbsaCtyXwalk = fread("./data/stem_edu/original/County-CBSA Crosswalk/cbsatocountycrosswalk2017.csv")
cbsaCtyXwalk$fipscounty = as.numeric(cbsaCtyXwalk$fipscounty)

#
# To compare to DAW, we need to aggregate the OJ data by month and by CBSA in Virginia
#

jobsDescCbsa = mergedJobsWithDesc[cbsaCtyXwalk[,c("fipscounty", "cbsa")], on = c("county_fips" = 'fipscounty'), nomatch = 0][,c("tlats", "tlons", "normalizedTitle_onetCode","normalizedTitle_onetName", "datePosted", "responsibilities", "experienceRequirements", "jobDescription", "hiringOrg", "cbsa")]
jobsDescCbsa$datePosted = as_date(jobsDescCbsa$datePosted)

# Missing CBSAs are characte(0)'s instead of NAs. This removes them.
jobsDescCbsa = jobsDescCbsa[nchar(cbsa) > 1]
# Keeping CBSAs with more than 100 records, since there was a big gap between a CBSA with 90 and a CBSA with 2780.
cbsasToKeep = jobsDescCbsa[, .N > 100, by = "cbsa"][V1 == TRUE, cbsa]
jobsDescCbsa = jobsDescCbsa[cbsa %in% cbsasToKeep]
rm(cbsasToKeep)

# Create a column for quarter

jobsDescCbsa$quarter = sprintf("%sQ%s",year(jobsDescCbsa$datePosted), quarter(jobsDescCbsa$datePosted))

# Determine if 2 postings are the same by comparing their descriptions. First remove all exact job desc matches
# For ass-covering purposes, also check matches along the other columns with

rows = jobsDescCbsa[,!duplicated(jobDescription), by = c("quarter", "cbsa", "normalizedTitle_onetCode")]$V1
jobsDedup = jobsDescCbsa[rows]
jobsDedupShort = jobsDedup[,-c('responsibilities', 'experienceRequirements','jobDescription'), with = FALSE]

# Make classification columns for each onet code according to the crosswalk produced by Bianica

cw = fread("./data/stem_edu/original/occupation_cw.csv", header = T, stringsAsFactors = F)[,-1]

cw$nsf_stem <- recode(cw$nsf_se, 'Non-S&E and Other' = "Non STEM",'S&E Occupations' = "STEM", 'S&E-Related Occupations' = "STEM Related")
cw$rothwell_stem <- recode(cw$rothwell_classification, "Other" = "Non STEM",'High-STEM' = "STEM Related", "Super-STEM" = "STEM")
cw$soc_stem2 <- recode(cw$soc_stem, "Non-STEM" = "Non STEM", "STEM-related" = "STEM Related", "STEM" = "STEM")

crosswalk = select(cw, onet_soc_code, onet_soc_title, soc_stem2, nsf_stem, rothwell_stem)
rm(cw)

jobsAndCrosswalk = jobsDedupShort[crosswalk, on = c("normalizedTitle_onetCode" = "onet_soc_code"), nomatch = 0][,-c("tlats", "tlons", 'onet_soc_title', 'hiringOrg')]

jobsByCbsaOJ = na.omit(jobsAndCrosswalk[, .(nJobs = .N,
                                        nSocStem = sum(soc_stem2 == "STEM"),
                                        nNsfStem = sum(nsf_stem == "STEM"),
                                        nRothStem = sum(rothwell_stem == "STEM"),
                                        nSocStemRel = sum(soc_stem2 == "STEM Related"),
                                        nNsfStemRel = sum(nsf_stem == "STEM Related"),
                                        nRothStemRel = sum(rothwell_stem == "STEM Related")),
                                    by = .(quarter, cbsa)])

fwrite(jobsByCbsaOJ, "./data/stem_edu/working/openJobsDedupByCountyAndQuarter.csv")

## Some metro areas have sub-cbsa numbers for subsets. Alex-Arl-DC is one of those. It appears as 47894 in the OJ data but as 47900 in DAW. I'm changing it to 47900 in OJ for consistency
# See https://www.census.gov/population/estimates/metro-city/0312msa.txt
jobsByCbsaOJ$cbsa[jobsByCbsaOJ$cbsa == '47894'] = '47900'


jobsByCbsaDAW = fread("./data/stem_edu/working/dataAtWork_summary.csv")[,-'V1', with = FALSE]
# Filter this by virginia then aggregate to cbsa, we nee to match with a year's lag use DAW for Q1 2016.
#Do virgina vs everything using JOLTS

### COMPARE PROPORTIONS OF THE VARIOUS STEM CATEGORIES FROM DAW 2016Q1 TO OJ 2017Q1
# Write a function to compare oj to daw for a time

ojQuarters = c("2017Q1")
dawQuarters = c("2016Q1")
cbsas = unique(jobsByCbsaOJ$cbsa)

compareOJandDAW = function(cbsas, ojQuarters, dawQuarters){
  ojJobs = jobsByCbsaOJ[quarter %in% ojQuarters][cbsa %in% cbsas]
  dawJobs = jobsByCbsaDAW[yearQtr %in% dawQuarters][cbsa_fips %in% cbsas]
  dawJobs = dawJobs[,.(cbsa_name = cbsa_name[1],
                       quarter = yearQtr[1],
                       nJobsDAW = sum(total_job_ads),
                       nSocStemDAW = sum(STEM),
                       nSocStemRelDAW = sum(`STEM-related`),
                       nNsfStemDAW = sum(`S&E Occupations`),
                       nNsfStemRelDAW = sum(`S&E-Related Occupations`),
                       nRothStemDAW = sum(`Super-STEM`),
                       nRothStemRelDAW = sum(`High-STEM`)),
                    by = cbsa_fips]
  colnames(ojJobs) = c("quarter","cbsa_fips", "nJobsOJ", "nSocStemOJ", "nNsfStemOJ", "nRothStemOJ", "nSocStemRelOJ", "nNsfStemRelOJ", "nRothStemRelOJ")
  return(list(ojJobs = ojJobs, dawJobs = dawJobs))
}

dat = compareOJandDAW(cbsas, ojQuarters, dawQuarters)

ojJobs = arrange(dat[[1]], cbsa_fips)
dawJobs = arrange(dat[[2]], cbsa_fips)

# Fitness for use comparison: ratio of stem to stem related for each of the stemminess measures.

stemRatiosComparison = data.table(cbsa_fips = dawJobs$cbsa_fips,
                                  nsfOjRatio = ojJobs$nNsfStemOJ/ojJobs$nNsfStemRelOJ,
                                  socOjRatio = ojJobs$nSocStemOJ/ojJobs$nSocStemRelOJ,
                                  rothOjRatio = ojJobs$nRothStemOJ/ojJobs$nRothStemRelOJ,
                                  nsfDawRatio = dawJobs$nNsfStemDAW/dawJobs$nNsfStemRelDAW,
                                  socDawRatio = dawJobs$nSocStemDAW/dawJobs$nSocStemRelDAW,
                                  rothDawRatio = dawJobs$nRothStemDAW/dawJobs$nRothStemRelDAW)

# Shape file
cbsaShapeFile = readOGR(dsn  = "./data/stem_edu/original/shapefiles/cb_2016_us_cbsa_500k/cb_2016_us_cbsa_500k.shp")


