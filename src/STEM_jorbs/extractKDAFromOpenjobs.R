library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)
library(jsonlite)

data.dir = "./data/stem_edu/original/"

# Load in the file containing the locations and onet codes of all job postings, as well as the crosswalk

parsedJobs = readRDS(paste0(data.dir, "parsed_jobs_df.RDS"))[,-1]
crosswalkFull = read.csv(paste0(data.dir, "occupation_cw.csv"), header = T, stringsAsFactors = F)[,-1]

# This function pulls out the correct info in nested list form.

extractKSAforSOC = function(socCode, parsedJobs){
  nmtPostings = parsedJobs[parsedJobs$normalizedTitle_onetCode == socCode, "raw_data"]
  dat = lapply(nmtPostings, fromJSON)
  list(
    skills = lapply(dat, "[[", "skills"),
    quals = lapply(dat, "[[", "qualifications"),
    eduReq = lapply(dat, "[[", "educationRequirements"),
    expReq = lapply(dat, "[[", "experienceRequirements")
  )
}

# Now we can find SOC codes from the crosswalk and extract whatever we want
# For now we're looking at one profession: nuclear medicine tech. let's find their postings

nmtSocCode = crosswalkFull[grep("Nuclear Medicine Technologists", crosswalkFull$onet_soc_title, ignore.case = T), 1]
extractKSAforSOC(nmtSocCode, parsedJobs)

execSocCode = crosswalkFull[grep("Chief Executives", crosswalkFull$onet_soc_title, ignore.case = T), 1]
execDat = extractKSAforSOC(execSocCode, parsedJobs)
