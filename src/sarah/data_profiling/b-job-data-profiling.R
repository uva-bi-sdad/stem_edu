library(tidyverse)
library(data.table)
library(inspectdf)
library(dplyr)

b_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/blacksburg_jobs.csv")
b_job <- select(b_job, bgtjobid, jobdate, occfam, occfamname, employer, city,state, county, fipsstate, fipscounty, fips, lat, lon, onet, onetname, bgtocc, edu, degree, exp, jobhours)


b_job_prof <- data.table(variable = colnames(b_job), completeness = numeric(length = ncol(b_job)),
                         validity = numeric(length = ncol(b_job)), uniqueness = numeric(length = ncol(b_job)))

###COMPLETENESS

compcount <- function(x){
  (length(x) - sum(is.na(x)))/length(x)
}

b_job_prof$completeness <- apply(b_job, MARGIN = 2, compcount)

#####UNIQUENESS

uniquecount <- function(x){
  length(unique(x))
}

b_job_prof$uniqueness <- apply(b_job, MARGIN = 2, uniquecount)

###VALUE VALIDITY
####checking to make sure all job ids are the same length:
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

###VALIDITY EMPLOYER
#Characters
str(b_job$employer)
#looking for outliers in character count of employer
b_job %>% group_by(nchar(employer)) %>% summarise(count = n())
#employer should not be less than or equal to one
sum(nchar(b_job$employer) <= 1, na.rm = TRUE)

b_job_prof[b_job_prof$variable == "employer","validity"] <- 1*b_job_prof[b_job_prof$variable == "employer","completeness"]


###VALIDITY city
#structure should be char
str(b_job$city)
#looking for outliers in character count of employer
b_job %>% group_by(nchar(city)) %>% summarise(count = n())

b_job_prof[b_job_prof$variable == "city","validity"] <- 1*b_job_prof[b_job_prof$variable == "city","completeness"]
###Validity State
#structure should be char
str(b_job$state)
#State should be Virginia
b_job$state == "Virginia" #This returns FAlSE because there are lots of unnecessary spaces
unique(nchar(b_job$state)) # this returns 30 because there are lots of unnecessary spaces
#removing unnecessary spacing
b_job$state <- str_replace_all(string=b_job$state, pattern=" ", repl="")
b_job$state == "Virginia" #now this returns TRUE

#STATE IS SUPPOSED TO BE TWO CHAR
b_job_prof[b_job_prof$variable == "state","validity"] <- 1*b_job_prof[b_job_prof$variable == "state","completeness"]

###VALIDITY county
#looked for outliers in count of county
b_job %>% group_by(county) %>% summarise(count = n())
b_job_prof[b_job_prof$variable == "county","validity"] <- 1*b_job_prof[b_job_prof$variable == "county","completeness"]

###Validity FIPSSTATE
#should be two characters
nchar(b_job$fipsstate)
str(b_job$fipsstate) #is integer, should be a character
b_job_prof[b_job_prof$variable == "fipsstate","validity"] <- 1*b_job_prof[b_job_prof$variable == "fipsstate","completeness"]

###Validity FIPSCOUNTY
#should be three characters
nchar(b_job$fipscounty) #some returned one or two
str(b_job$fipscounty) # is integer, should be character
b_job %>% group_by(nchar(fipscounty)) %>% summarise(count = n()) #they are just missing zeros

b_job_prof[b_job_prof$variable == "fipscounty","validity"] <- 1*b_job_prof[b_job_prof$variable == "fipscounty","completeness"]

###Validity FIPS
#should be five characters
nchar(b_job$fips)
str(b_job$fips) # is integers, should be character
unique(b_job$fips) #should match to unique fipscounty

b_job_prof[b_job_prof$variable == "fips","validity"] <- 1*b_job_prof[b_job_prof$variable == "fips","completeness"]

###Validity lat
#should be a number
str(b_job$lat)
# check to see if lat is in a similar range
sum(b_job$lat < 30)
sum(b_job$lat > 42)

b_job_prof[b_job_prof$variable == "lat","validity"] <- 1*b_job_prof[b_job_prof$variable == "lat","completeness"]

###Validity lon
#should be a number
str(b_job$lon)
# check to see if lon is in a similar range
sum(b_job$lon < -82)
sum(b_job$lon > -72)
b_job_prof[b_job_prof$variable == "lon","validity"] <- 1*b_job_prof[b_job_prof$variable == "lon","completeness"]

###ONET
str(b_job$onet) #character
b_job %>% group_by(nchar(onet)) %>% summarise(count = n()) #checking for uniform character count
b_job_prof[b_job_prof$variable == "onet","validity"] <- 1*b_job_prof[b_job_prof$variable == "onet","completeness"]

###ONET name
str(b_job$onetname) #character
length(unique(b_job$onetname)) #should be equal to onet variable
b_job_prof[b_job_prof$variable == "onetname","validity"] <- 1*b_job_prof[b_job_prof$variable == "onetname","completeness"]

###BGTOCC
str(b_job$bgtocc) #character
b_job %>% group_by(nchar(bgtocc)) %>% summarise(count = n()) #checking for uniform character count
b_job_prof[b_job_prof$variable == "bgtocc","validity"] <- 1*b_job_prof[b_job_prof$variable == "bgtocc","completeness"]

###EDU
str(b_job$edu)#should be num, IS INTEGER
unique(b_job$edu) #there are only six unique values including
b_job_prof[b_job_prof$variable == "edu","validity"] <- 1*b_job_prof[b_job_prof$variable == "edu","completeness"]

###DEGREE
str(b_job$degree)#character
unique(b_job$degree)
b_job_prof[b_job_prof$variable == "degree","validity"] <- 1*b_job_prof[b_job_prof$variable == "degree","completeness"]

###EXP
str(b_job$exp) #num
unique(b_job$exp) #some of the decimal numbers do not make a ton of sense and may be due to rounding error
b_job_prof[b_job_prof$variable == "exp","validity"] <- 1*b_job_prof[b_job_prof$variable == "exp","completeness"]

###jobhhours
str(b_job$jobhours) #character
unique(b_job$jobhours)
b_job_prof[b_job_prof$variable == "jobhours","validity"] <- 1*b_job_prof[b_job_prof$variable == "jobhours","completeness"]

write.csv(b_job_prof, "~/stem_edu/src/sarah/b_job_prof.csv")

###SKILLS
b_skill <- fread("data/stem_edu/working/Team_SA_job_skills_filter/blacksburg_skills.csv")
str(b_skill)
head(b_skill)
b_skill
b_skill <- b_skill %>% select(skill, skillcluster, skillclusterfamily)

b_skill <- b_skill%>%
  mutate(skill = replace(skill, skill == "na", NA))
b_skill <- b_skill%>%
  mutate(skillcluster = replace(skillcluster, skillcluster == "na", NA))
b_skill <- b_skill%>%
  mutate(skillclusterfamily = replace(skillclusterfamily, skillclusterfamily == "na", NA))

b_skill %>%
  filter(nchar(skill)==2) #checking that na is removed

###creating table to hold profiling results:
b_skill_prof <- data.table(variable = colnames(b_skill), completeness = numeric(length = ncol(b_skill)),
                           validity = numeric(length = ncol(b_skill)), uniqueness = numeric(length = ncol(b_skill)))

#####Completeness

compcount <- function(x){
  (length(x) - sum(is.na(x)))/length(x)
}

b_skill_prof$completeness <- apply(b_skill, MARGIN = 2, compcount)
#double checking completeness function
sum(is.na(b_skill$skill))
sum(is.na(b_skill$skillcluster))
sum(is.na(b_skill$skillclusterfamily))

uniquecount <- function(x){
  length(unique(x))
}

b_skill_prof$uniqueness <- apply(b_skill, MARGIN = 2, uniquecount)


#####VALUE VALIDITY
#Skill
str(b_skill$skill) #character
tail(b_skill %>% group_by(nchar(skill)) %>% summarise(count = n()))
b_skill_prof[b_skill_prof$variable == "skill","validity"] <- 1*b_skill_prof[b_skill_prof$variable == "skill","completeness"]

#skill cluster
str(b_skill$skillcluster) #character
head(b_skill %>% group_by(nchar(skillcluster)) %>% summarise(count = n()))
tail(b_skill %>% group_by(nchar(skillcluster)) %>% summarise(count = n()))
b_skill_prof[b_skill_prof$variable == "skillcluster","validity"] <- 1*b_skill_prof[b_skill_prof$variable == "skillcluster","completeness"]

#skill cluster family
str(b_skill$skillclusterfamily) #character
head(b_skill %>% group_by(nchar(skillclusterfamily)) %>% summarise(count = n()))
tail(b_skill %>% group_by(nchar(skillclusterfamily)) %>% summarise(count = n()))
b_skill_prof[b_skill_prof$variable == "skillclusterfamily","validity"] <- 1*b_skill_prof[b_skill_prof$variable == "skillclusterfamily","completeness"]


write.csv(b_skill_prof, "~/stem_edu/src/sarah/b_skill_prof.csv")

blacksburg_prof <- b_job_prof %>%
  bind_rows(b_skill_prof)
write.csv(blacksburg_prof, "~/stem_edu/src/sarah/blacksburg_prof.csv")
