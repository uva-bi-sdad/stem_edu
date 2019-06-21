library(tidyverse)
library(data.table)
library(inspectdf)

r_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/rich_jobs.csv")
str(r_job)
head(r_job)
#Select these because they were listed for data profiling
r_job <- select(r_job, bgtjobid, jobdate, occfam, occfamname, employer, city,state, county, fipsstate, fipscounty, fips, lat, lon, onet, onetname, bgtocc, edu, degree, exp, jobhours)

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

###validity occfamname
#occfamname should all be characters
str(r_job$occfamname)
#looking for outliers in character count of occfamname
r_job %>% group_by(nchar(occfamname)) %>% summarise(count = n())
#occfamname should not be less than or equal to one
sum(nchar(r_job$occfamname) <= 1, na.rm = TRUE)
r_job_prof[r_job_prof$variable == "occfamname","validity"] <- 1*r_job_prof[r_job_prof$variable == "occfamname","completeness"]

###VALIDITY EMPLOYER
#Characters
str(r_job$employer)
#looking for outliers in character count of employer
r_job %>% group_by(nchar(employer)) %>% summarise(count = n())
#employer should not be less than or equal to one
sum(nchar(r_job$employer) <= 1, na.rm = TRUE)

r_job_prof[r_job_prof$variable == "employer","validity"] <- 1*r_job_prof[r_job_prof$variable == "employer","completeness"]


###VALIDITY city
#structure should be char
str(r_job$city)
#looking for outliers in character count of employer
r_job %>% group_by(nchar(city)) %>% summarise(count = n())

r_job_prof[r_job_prof$variable == "city","validity"] <- 1*r_job_prof[r_job_prof$variable == "city","completeness"]
###Validity State
#structure should be char
str(r_job$state)
#State should be Virginia
r_job$state == "Virginia" #This returns FAlSE because there are lots of unnecessary spaces
unique(nchar(r_job$state)) # this returns 30 because there are lots of unnecessary spaces
#removing unnecessary spacing
r_job$state <- str_replace_all(string=r_job$state, pattern=" ", repl="")
r_job$state == "Virginia" #now this returns TRUE

#STATE IS SUPPOSED TO BE TWO CHAR
r_job_prof[r_job_prof$variable == "state","validity"] <- 1*r_job_prof[r_job_prof$variable == "state","completeness"]

###VALIDITY county
#looked for outliers in count of county
r_job %>% group_by(county) %>% summarise(count = n())
r_job_prof[r_job_prof$variable == "county","validity"] <- 1*r_job_prof[r_job_prof$variable == "county","completeness"]

###Validity FIPSSTATE
#should be two characters
nchar(r_job$fipsstate)
str(r_job$fipsstate) #is integer, should be a character
r_job_prof[r_job_prof$variable == "fipsstate","validity"] <- 1*r_job_prof[r_job_prof$variable == "fipsstate","completeness"]

###Validity FIPSCOUNTY
#should be three characters
nchar(r_job$fipscounty) #some returned one or two
str(r_job$fipscounty) # is integer, should be character
r_job %>% group_by(nchar(fipscounty)) %>% summarise(count = n()) #they are just missing zeros

r_job_prof[r_job_prof$variable == "fipscounty","validity"] <- 1*r_job_prof[r_job_prof$variable == "fipscounty","completeness"]

###Validity FIPS
#should be five characters
nchar(r_job$fips)
str(r_job$fips) # is integers, should be character
unique(r_job$fips) #should match to unique fipscounty

r_job_prof[r_job_prof$variable == "fips","validity"] <- 1*r_job_prof[r_job_prof$variable == "fips","completeness"]

###Validity lat
#should be a number
str(r_job$lat)
# check to see if lat is in a similar range
sum(r_job$lat < 32)
sum(r_job$lat > 42)

r_job_prof[r_job_prof$variable == "lat","validity"] <- 1*r_job_prof[r_job_prof$variable == "lat","completeness"]

###Validity lon
#should be a number
str(r_job$lon)
# check to see if lon is in a similar range
sum(r_job$lon < -82)
sum(r_job$lon > -72)
r_job_prof[r_job_prof$variable == "lon","validity"] <- 1*r_job_prof[r_job_prof$variable == "lon","completeness"]

###ONET
str(r_job$onet) #character
r_job %>% group_by(nchar(onet)) %>% summarise(count = n()) #checking for uniform character count
r_job_prof[r_job_prof$variable == "onet","validity"] <- 1*r_job_prof[r_job_prof$variable == "onet","completeness"]

###ONET name
str(r_job$onetname) #character
length(unique(r_job$onetname)) #should be equal to onet variable
r_job_prof[r_job_prof$variable == "onetname","validity"] <- 1*r_job_prof[r_job_prof$variable == "onetname","completeness"]

###BGTOCC
str(r_job$bgtocc) #character
r_job %>% group_by(nchar(bgtocc)) %>% summarise(count = n()) #checking for uniform character count
r_job_prof[r_job_prof$variable == "bgtocc","validity"] <- 1*r_job_prof[r_job_prof$variable == "bgtocc","completeness"]

###EDU
str(r_job$edu)#should be num, IS INTEGER
unique(r_job$edu) #there are only six unique values including
r_job_prof[r_job_prof$variable == "edu","validity"] <- 1*r_job_prof[r_job_prof$variable == "edu","completeness"]

###DEGREE
str(r_job$degree)#character
unique(r_job$degree)
r_job_prof[r_job_prof$variable == "degree","validity"] <- 1*r_job_prof[r_job_prof$variable == "degree","completeness"]

###EXP
str(r_job$exp) #num
unique(r_job$exp) #some of the decimal numbers do not make a ton of sense and may be due to rounding error
r_job_prof[r_job_prof$variable == "exp","validity"] <- 1*r_job_prof[r_job_prof$variable == "exp","completeness"]

###jobhhours
str(r_job$jobhours) #character
unique(r_job$jobhours)
r_job_prof[r_job_prof$variable == "jobhours","validity"] <- 1*r_job_prof[r_job_prof$variable == "jobhours","completeness"]

#####UNIQUENESS

uniquecount <- function(x){
  length(unique(x))
}

r_job_prof$uniqueness <- apply(r_job, MARGIN = 2, uniquecount)
