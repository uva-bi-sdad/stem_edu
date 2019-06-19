library(tidyverse)
library(data.table)
library(inspectdf)

r_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/rich_jobs.csv")
str(r_job)
head(r_job)
#Select these because they were listed for data profiling
r_job <- select(r_job, bgtjobid, jobdate, occfam, occfamname, employer, city,state, county, fipsstate, fipscounty, fips, lat, lon)

r_job_cat <- r_job %>% inspect_cat()
r_job_cat$levels$city
r_job_cat$levels$fipsstate


r_job_cat %>% show_plot()

r_job %>% group_by(lon) %>% summarise(count = n()) %>% arrange(desc(count))

###creating table to hold profiling results:
r_job_prof <- data.table(variable = colnames(r_job), completeness = numeric(length = ncol(r_job)),
                         validity = numeric(length = ncol(r_job)), uniqueness = numeric(length = ncol(r_job)))

###COMPLETENESS

compcount <- function(x){
  (length(x) - sum(is.na(x)))/length(x)
}

r_job_prof$completeness <- apply(r_job, MARGIN = 2, compcount)

#validating compcount function:

(nrow(r_job)-sum(is.na(r_job$employer)))/nrow(r_job)


###VALUE VALIDITY
####checking to make sure all job ids are the same length:
unique(nchar(r_job$bgtjobid))
r_job %>% group_by(nchar(bgtjobid)) %>% summarise(count = n())
#found that almost all are 21 char long, but smaller numbers are 16-20 characters long. However, those
#shorter IDs otherwise resemble the 21 character IDs, and the sample ID in the data dictionary is only 4
#characters long, so going to consider them good.
r_job %>% filter(nchar(bgtjobid)==16)
r_job_prof[r_job_prof$variable == "bgtjobid","validity"] <- 1

#Validity for jobdate: all have the appropriate number of characters
sum(nchar(r_job$jobdate) != 10)
unique(nchar(r_job$jobdate))

r_job$jobdate <- as.Date(r_job$jobdate, "%Y-%m-%d")

min(r_job$jobdate)
max(r_job$jobdate)

r_job_prof[r_job_prof$variable == "jobdate","validity"] <- 1

###Validity for occfam
#Should always be a 2-digit number

sum(nchar(r_job$occfam) !=2, na.rm = TRUE)
r_job_prof[r_job_prof$variable == "occfam","validity"] <- 1*r_job_prof[r_job_prof$variable == "occfam","completeness"]
