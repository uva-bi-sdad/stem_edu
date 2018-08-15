library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

# Missingness profiling function

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

data.dir = "./data/stem_edu/original/BurningGlassData/sampleResumeData"
datasetNames = c("candidateInfo", "certs", "employer", "jobs", "school", "skills")

datasets = lapply(1:6, function(x) {
  out = fread(list.files(data.dir, full.names = TRUE)[x])
  colnames(out)[1] = "BGTResid"
  return(out)
})
candidateInfo = datasets[[1]]
certs = datasets[[2]]
employer = datasets[[3]]
jobs = datasets[[4]]
school = datasets[[5]]
skills = datasets[[6]]

profileMissingness(candidateInfo)
profileMissingness(certs)
profileMissingness(employer)
profileMissingness(jobs)
profileMissingness(school)
profileMissingness(skills)


# Plot a job history for a person
# Count the number of job histories which fail to parse due to weird encoding
# Is there a date associated with when the resumes are uploaded to the DB, or when they're acquired? Or last updated?

uniqueIDs = unique(jobs$BGTResid)
index = 2
activeId = uniqueIDs[index]
filter(jobs, BGTResid == activeId)

jobTimes = jobs %>%
  filter(BGTResid == activeId) %>%
  transmute(jobISOStartDate = as_date(jobISOStartDate), jobISOEndDate = as_date(jobISOEndDate)) %>%
  arrange(jobISOStartDate)
jobTimes = cbind(y = nrow(jobTimes):1, jobTimes)

ggplot(data = jobTimes) + geom_point(aes(y = y, x = jobISOStartDate), pch = 1, col = 'blue') +
  geom_point(aes(y = y, x = jobISOEndDate), pch = 2, col = 'red')


# Count unique skills and skill clusters

length(unique(skills$skillName))
length(unique(skills$skillCLusterName))

# School: what proportion have CIP codes
mean(nchar(school$MajorCipCode) == 0)
mean(nchar(school$major) == 0)

# Employer, same number of rows as the jobs data. Contains employer specific information
# Why are NAICS codes missing when they are?

mean(is.na(employer$inferredNaics))

# Certifications


#candidaidate Info
# Location and counts of things, basically
hist(candidateInfo$NoOfJobs)



