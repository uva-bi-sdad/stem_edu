library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

profileMissingness = function(dataTable){
  ncol = ncol(dataTable)
  propMissing = sapply(1:ncol, function(x){
    col = dataTable[,x, with = FALSE]
    out = max(mean(is.na(col)), mean(col == -999, na.rm = T), mean(nchar(col) == 0, na.rm = T), mean(col == 'na', na.rm = T))
    return(out)
  })
  lenUnique = sapply(1:ncol, function(x){
    col = dataTable[,x, with = FALSE]
    out = uniqueN(col)
    return(out)
  })
  out = data.table(dataTable = deparse(substitute(dataTable)), columnNames = colnames(dataTable), propMissing = propMissing, lenUnique = lenUnique)
  return(out)
}

# Load data
## Devika got this far
test <- utils::unzip("./data/stem_edu/original/Burning Glass Data/Certs/2017/Certs_2017-12.zip", list = FALSE, overwrite = FALSE, exdir = "./data/stem_edu/original/Burning Glass Data/Certs")
test <- read.delim(test)
head(test)

data.dir = "./data/stem_edu/original/Burning Glass Data/"
folderNames = c("Certs/", "CIP/", "Main/", "Skill/", "Degree/", "Major/")
month = as.character(c(2007, 2010:2017))

datasetAds = lapply(1:9, function(x) {
  paths = paste0(data.dir, folderNames, month[x], "/")
  out = utils::unzip(paths[x], list = FALSE, overwrite = FALSE) # I can't get this to work
} )
# Gonna take a break
datasetsAds = lapply(2:6, function(x) {
  out = fread(list.files(data.dir, full.names = TRUE)[x])
  #colnames(out)[1] = "BGTResid"
  return(out)
})
certsAds = datasetsAds[[1]]
cips = datasetsAds[[2]]
main = datasetsAds[[3]]
skillsAds = datasetsAds[[4]]
stdmajors = datasetsAds[[5]]

profileMissingness(certsAds)
profileMissingness(cips)
profileMissingness(main)
profileMissingness(skillsAds)
profileMissingness(stdmajors)


# Skills: one row per skill in a posting, has hierarchical clustering, as well as 3 indiscators for specialized, baseline, and software

length(unique(skillsAds$Skill))
length(unique(skillsAds$SkillCluster))
length(unique(skillsAds$SkillClusterFamily))


# STDMajor: majors mentioned in the job posting

length(unique(stdmajors$STDMajor))

# CIPs: same rows as stdmajor, but has cip code for the mentioned major

length(unique(cips$CIP))

# certs: certifications

table(certsAds$Certification)
length(unique(certsAds$Certification))


# Main
# Posting date, onet code, names and places in BG taxonomy, NAICS codes, lat/long, degree info, salary range
