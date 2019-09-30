library(tidyverse)
library(data.table)
library(inspectdf)
library(dplyr)

b_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/blacksburg_jobs.csv")
str(b_job)
head(b_job)
#Select these because they were listed for data profiling
b_job <- select(b_job, bgtjobid, jobdate, occfam, occfamname, employer, city,state, county, fipsstate, fipscounty, fips, lat, lon, onet, onetname, bgtocc, edu, degree, exp, jobhours)

###creating table to hold profiling results:
b_job_prof <- data.table(variable = colnames(b_job), completeness = numeric(length = ncol(b_job)),
                         validity = numeric(length = ncol(b_job)), uniqueness = numeric(length = ncol(b_job)))

compcount <- function(x){
  (length(x) - sum(is.na(x)))/length(x)
}

b_job_prof$completeness <- apply(b_job, MARGIN = 2, compcount)

###VALUE VALIDITY
####checking to make sure all job ids are the same length:
unique(nchar(b_job$bgtjobid))
b_job %>% group_by(nchar(bgtjobid)) %>% summarise(count = n())
#found that almost all are 21 char long, but smaller numbers are 16-20 characters long. However, those
#shorter IDs otherwise resemble the 21 character IDs, and the sample ID in the data dictionary is only 4
#characters long, so going to consider them good.
b_job %>% filter(nchar(bgtjobid)==16)
b_job_prof[b_job_prof$variable == "bgtjobid","validity"] <- 1

#Validity for jobdate: all have the appropriate number of characters
sum(nchar(b_job$jobdate) != 10)
unique(nchar(b_job$jobdate))

b_job$jobdate <- as.Date(b_job$jobdate, "%Y-%m-%d")

min(b_job$jobdate)
max(b_job$jobdate)

b_job_prof[b_job_prof$variable == "jobdate","validity"] <- 1

###Validity for occfam
#Should always be a 2-digit number

sum(nchar(b_job$occfam) !=2, na.rm = TRUE)
b_job_prof[b_job_prof$variable == "occfam","validity"] <- 1*b_job_prof[b_job_prof$variable == "occfam","completeness"]

###validity occfamname
#occfamname should all be characters
str(b_job$occfamname)
#looking for outliers in character count of occfamname
b_job %>% group_by(nchar(occfamname)) %>% summarise(count = n())
#occfamname should not be less than or equal to one
sum(nchar(b_job$occfamname) <= 1, na.rm = TRUE)
b_job_prof[b_job_prof$variable == "occfamname","validity"] <- 1*b_job_prof[b_job_prof$variable == "occfamname","completeness"]
