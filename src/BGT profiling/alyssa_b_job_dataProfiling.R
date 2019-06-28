library(tidyverse)
library(data.table)
library(inspectdf)

b_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/blacksburg_jobs.csv")
str(b_job)
head(b_job)

b_job <- select(b_job, bgtjobid, jobdate, occfam, occfamname, onet, onetname, bgtocc, employer, city,
                state, county, fipsstate, fipscounty, fips, lat, lon, edu, degree, maxdegree, exp, jobhours)

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

 ###Validity for occfamname
 #Should always be text, lengths look reasonable (very short is NA, very long is genuinely long occupation family names)
str(b_job)
hist(nchar(b_job$occfamname))
b_job[nchar(b_job$occfamname) > 50, "occfamname"]
b_job_prof[b_job_prof$variable == "occfamname","validity"] <- 1*b_job_prof[b_job_prof$variable == "occfamname","completeness"]


#validity for O-NET: these should all be 8-digit codes (but they're stored as 10-character strings due to dashes)

unique(nchar(b_job$onet))
str(b_job)
b_job_prof[b_job_prof$variable == "onet","validity"] <- 1*b_job_prof[b_job_prof$variable == "onet","completeness"]

#validity for O-NET name: should be character strings, very short and very long ones look ok
hist(nchar(b_job$onetname))
unique(b_job[nchar(b_job$onetname) <= 10 | nchar(b_job$onetname) >= 90, "onetname"])
b_job_prof[b_job_prof$variable == "onetname","validity"] <- 1*b_job_prof[b_job_prof$variable == "onetname","completeness"]

#validity for burning glass occupation code, all  same length integers
table(nchar(b_job$bgtocc))
b_job_prof[b_job_prof$variable == "bgtocc","validity"] <- 1*b_job_prof[b_job_prof$variable == "bgtocc","completeness"]


 ##Validity for employer
 #Not sure what to look for here, beyond that they're all characters?
#Very long and very short employer names all check out:
unique(nchar(b_job$employer))
long_employer <- b_job[which(nchar(b_job$employer)>=50),"employer"]
unique(long_employer)
unique(b_job[which(nchar(b_job$employer)<=5),"employer"])
b_job_prof[b_job_prof$variable == "employer","validity"] <- 1*b_job_prof[b_job_prof$variable == "employer","completeness"]

#validity for edu, all values look ok
unique(b_job$edu)
b_job_prof[b_job_prof$variable == "edu","validity"] <- 1*b_job_prof[b_job_prof$variable == "edu","completeness"]

#validity for edu, all look ok but have some extra spaces at the end:
unique(b_job$degree)
b_job_prof[b_job_prof$variable == "degree","validity"] <- 1*b_job_prof[b_job_prof$variable == "degree","completeness"]

#validity for maxdegree, all look ok but have some extra spaces at the end:
unique(b_job$maxdegree)
b_job_prof[b_job_prof$variable == "maxdegree","validity"] <- 1*b_job_prof[b_job_prof$variable == "maxdegree","completeness"]

#validity for exp, all are reasonable values for experience requirements
hist(b_job$exp)
b_job_prof[b_job_prof$variable == "exp","validity"] <- 1*b_job_prof[b_job_prof$variable == "exp","completeness"]

#validity for jobhours, all values look ok although they have extra spaces at the end
unique(b_job$jobhours)
b_job_prof[b_job_prof$variable == "jobhours","validity"] <- 1*b_job_prof[b_job_prof$variable == "jobhours","completeness"]

##Validity for city
#looking at all the city names
unique(b_job$city)
b_job_prof[b_job_prof$variable == "city","validity"] <- 1*b_job_prof[b_job_prof$variable == "city","completeness"]

##Validity for state
#should just all be Virginia, and it is
unique(b_job$state)
b_job_prof[b_job_prof$variable == "state","validity"] <- 1*b_job_prof[b_job_prof$variable == "state","completeness"]

##Validity for county
#looking at values, all of which look fine
unique(b_job$county)
b_job_prof[b_job_prof$variable == "county","validity"] <- 1*b_job_prof[b_job_prof$variable == "county","completeness"]

##Validity for fipsstate
#should all be 51, VA's fips code, and they are
unique(b_job$fipsstate)
b_job_prof[b_job_prof$variable == "fipsstate","validity"] <- 1*b_job_prof[b_job_prof$variable == "fipsstate","completeness"]

######STOPPING HERE FOR THE DAY, SAVING RESULTS TO PICK UP TOMORROW######
write.csv(b_job_prof, "data/stem_edu/working/Team_SA_job_skills_filter/b_job_alyssa-draft.csv")

#####picking up again--reading in old results

b_job_prof <- read.csv("data/stem_edu/working/Team_SA_job_skills_filter/b_job_alyssa-draft.csv")

##FIPScounty: these should be 3 digits, some are 2 but if a 0 is added in front they become county FIPS near Blacksburg
unique(b_job$fipscounty)
b_job_prof[b_job_prof$variable == "fipscounty","validity"] <- 1*b_job_prof[b_job_prof$variable == "fipscounty","completeness"]

##FIPS: looked at unique values, they match the combination of state+county codes above
unique(b_job$fipscounty)
b_job_prof[b_job_prof$variable == "fips","validity"] <- 1*b_job_prof[b_job_prof$variable == "fips","completeness"]

##lat and lon
##all numbers, all of appropriate lengths, mins and maxes pretty close to each other
unique(nchar(b_job$lat))
unique(nchar(b_job$lon))
summary(b_job)
b_job_prof[b_job_prof$variable == "lat","validity"] <- 1*b_job_prof[b_job_prof$variable == "lat","completeness"]
b_job_prof[b_job_prof$variable == "lon","validity"] <- 1*b_job_prof[b_job_prof$variable == "lon","completeness"]

#######UNIQUENESS######

uniqueval <- function(x){
  sum(!is.na(unique(x)))
}

b_job_prof$uniqueness <- apply(b_job, MARGIN = 2, uniqueval)

#saving out:
write.csv(b_job_prof, "data/stem_edu/working/Team_SA_job_skills_filter/b_job_alyssa.csv")
