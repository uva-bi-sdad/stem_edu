library(tidyverse)
library(data.table)
library(inspectdf)

b_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/blacksburg_jobs.csv")
str(b_job)
head(b_job)

b_job <- select(b_job, bgtjobid, jobdate, occfam, occfamname, employer, city,
                state, county, fipsstate, fipscounty, fips, lat, lon)

b_job_cat <- b_job %>% inspect_cat()
b_job_cat$levels$city
b_job_cat$levels$fipsstate

b_job_cat %>% show_plot()

b_job %>% group_by(lon) %>% summarise(count = n()) %>% arrange(desc(count))

###Could not find any other missing value indicator other than NA

###creating table to hold profiling results:
b_job_prof <- data.table(variable = colnames(b_job), completeness = numeric(length = ncol(b_job)),
                         validity = numeric(length = ncol(b_job)), uniqueness = numeric(length = ncol(b_job)))

###getting completeness metric:

compcount <- function(x){
  (length(x) - sum(is.na(x)))/length(x)
}

 b_job_prof$completeness <- apply(b_job, MARGIN = 2, compcount)
#validating compcount function:
 (nrow(b_job)-sum(is.na(b_job$employer)))/nrow(b_job)

 str(b_job)

 ###Value validity
 #checking to make sure all are the same length:
 unique(nchar(b_job$bgtjobid))
 b_job %>% group_by(nchar(bgtjobid)) %>% summarise(count = n())
 #found that almost all are 21 char long, but smaller numbers are 17-20 characters long. However, those
 #shorter IDs otherwise resemble the 21 character IDs, and the sample ID in the data dictionary is only 4
 #characters long, so going to consider them good.
 b_job %>% filter(nchar(bgtjobid)==17)
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
 sum(nchar(b_job$occfam) != 2, na.rm = TRUE)
 b_job_prof[b_job_prof$variable == "occfam","validity"] <- 1*b_job_prof[b_job_prof$variable == "occfam","completeness"]

