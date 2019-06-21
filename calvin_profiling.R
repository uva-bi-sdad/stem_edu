# Playing with the BGT data
# Calvin Isch
# 2019/06/13

library(DataExplorer)
library(devtools)
library(dplyr)
library(tidyverse)
library(data.table)
install_github("alastairrushworth/inspectdf")

# Variables that we care about for data profiling
all_vars <-c("bgtjobid","jobdate","occfam","occfamname","employer","city","state","county", "fipsstate", "fipscounty","fips","lat","lon","bestfitmsaname")
profile_vars <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_main_2017_51.RDS')
skills <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_skills_2017_51.RDS')
certs <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_certs_2017_51.RDS')
cip  <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_cip_2017_51.RDS')
degrees  <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_degrees_2017_51.RDS')
major  <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_major_2017_51.RDS')
profile_vars2 <- profile_vars %>%
  select("bgtjobid","jobdate","occfam","occfamname","employer","city","state","county", "fipsstate", "fipscounty","fips","lat","lon","bestfitmsaname")

# replacing -999 with NA
tempSals <- which(degrees$salary == -999)
degrees[tempSals,"salary"] <- NA
# Filtering
hasSalary <- filter(degrees,is.na(salary) == FALSE)
cor(hasSalary$degreelevel,hasSalary$salary)

# Check salaries
salaries <- skills$salary
s <- table(salaries)
s
# 6118437 of the salaries are missing from skills

# Sample commands that can be run from DataExplorer
plot_str(profile_vars2)
plot_missing(profile_vars2)
plot_histogram(profile_vars2)
plot_correlation(profile_vars2, type = 'continuous','Review.Date')
plot_bar(profile_vars2)

# ==================================================#

# Returns just the counties that we care about for our initial data-profiling.
#na.omit() is needed or no?
blacksburgMSA <- profile_vars2[profile_vars2$bestfitmsaname == "Blacksburg-Christiansburg-Radford, VA", ]
richmondMSA <- profile_vars2[profile_vars2$bestfitmsaname == "Richmond, VA", ]

# selects the skills for people from blacksburg and richmond
richid<- richmondMSA$bgtjobid
richskills<- skills[bgtjobid %chin% richid]
richskills <- richskills %>%
  select(skill,skillcluster,skillclusterfamily,bgtjobid)

# replace the "na" with an actual NA
richskills[richskills == "na"] <- NA
blacksid<- blacksburgMSA$bgtjobid
blackbskills<- skills[bgtjobid %chin% blacksid]
blackbskills<- blackbskills %>%
  select(skill,skillcluster,skillclusterfamily,bgtjobid)
blackbskills[blackbskills == "na"] <- NA


plot_missing(blackbskills)
plot_bar(blackbskills)

# =============================================================#
# Outputs the completeness and the uniqueness of Blacksburg and Richmond
is.na(blackbskills)
l <- count(blacksburgMSA)
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(blacksburgMSA)) {
  comp <- l - sum(is.na (blacksburgMSA[[i]]))
  uni<-length(unique(blacksburgMSA[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}
l2 <- count(richmondMSA)
print("Completeness for Richmond: ")
for (i in names(richmondMSA)) {
  comp <- l2 - sum(is.na (richmondMSA[[i]]))
  uni<-length(unique(richmondMSA[[i]]))
  print(paste(i, comp, round(comp / l2 * 100,2),"Still figuring out validity", uni))
}

# same things with skills:
l3 <- count(blackbskills)
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(blackbskills)) {
  comp <- l3 - sum(is.na (blackbskills[[i]]))
  uni<-length(unique(blackbskills[[i]]))
  print(paste(i, comp,round(comp / l3 * 100,2),"Still figuring out validity",uni))
}
l4 <- count(richskills)
print("Completeness for Richmond: ")
for (i in names(richskills)) {
  comp <- l4 - sum(is.na (richskills[[i]]))
  uni<-length(unique(richskills[[i]]))
  print(paste(i, comp, round(comp / l4 * 100,2),"Still figuring out validity", uni))
}


#=========================================================#
# Checks expected values in richmond
check_numbers <- function(mx,mn,coln,msa){
  good_thing = 0
  na_thing =0
  bad_thing = 0
  l = 99997
  if (msa[2,14] == "Blacksburg-Christiansburg-Radford, VA") {
    l = 11232
  }
  for (i in 1:l) {
    if (is.na(msa[i,coln])) {
      na_thing = na_thing + 1
      next
    } else if (msa[i,coln] < mx && msa[i,coln] > mn) {
      good_thing = good_thing + 1
      next
    } else {
      bad_thing = bad_thing +1
      next
    }
  }
  return(paste(msa[2,14],colnames(msa[coln]),"Num good: ",good_thing, "Num na:",na_thing,"Bad Thing:",bad_thing))
}

check_str <- function(mxChar,mnChar,coln,msa){
  good_thing = 0
  na_thing =0
  bad_thing = 0
  l = 99997
  if (msa[2,14] == "Blacksburg-Christiansburg-Radford, VA") {
    l = 11232
  }
  for (i in 1:l) {
    if (is.na(msa[i,coln])) {
      na_thing = na_thing + 1
      next
    } else if (nchar(msa[i,coln]) <= mxChar && nchar(msa[i,coln]) >= mnChar) {
      good_thing = good_thing + 1
      next
    } else {
      bad_thing = bad_thing +1
      next
    }
  }
  return(paste(msa[2,14],colnames(msa[coln]),"Num good: ",good_thing, "Num na:",na_thing,"Bad Thing:",bad_thing))
}


# ============================================================#
# checks that each job id is 11 characters
print(check_str(11,11,1,blacksburgMSA))
print(check_str(11,11,1,richmondMSA))
# checks that each date is 5 characters
print(check_str(5,5,2,blacksburgMSA))
print(check_str(5,5,2,richmondMSA))
# checks that each occfam name is a str between 2 and 50 chars
print(check_str(60,2,4,blacksburgMSA))
print(check_str(60,2,4,richmondMSA))
# checks that each employer is a str between 2 and 50 chars
print(check_str(130,1,5,blacksburgMSA))
print(check_str(130,1,5,richmondMSA))
# checks that each city is a str between 2 and 50 chars
print(check_str(30,1,6,blacksburgMSA))
print(check_str(30,1,6,richmondMSA))
# checks that each state is a str between 2 and 50 chars
print(check_str(30,1,7,blacksburgMSA))
print(check_str(30,1,7,richmondMSA))
# checks that each county is a str between 2 and 50 chars
print(check_str(30,1,8,blacksburgMSA))
print(check_str(30,1,8,richmondMSA))
# job ide between 38 and 39 billion
print(check_numbers(39000000000,38000000000,1,blacksburgMSA))
print(check_numbers(39000000000,38000000000,1,richmondMSA))
# check both occfam: between 55 and 10, col3
print(check_numbers(56,10,3,blacksburgMSA))
print(check_numbers(56,10,3,richmondMSA))
# check both fipsstate: expect 51, col9
print(check_numbers(52,50,9,blacksburgMSA))
print(check_numbers(52,50,9,richmondMSA))
# check fips county col 10
# blacksburg Floyd low = 63, radford high 750
print(check_numbers(751,62,10,blacksburgMSA))
#richmond Amelia low = 007,Richmond city = 760 (low and high)
print(check_numbers(761,6,10,richmondMSA))
# blacksburg Floyd low = 51063, radford high 51750
print(check_numbers(51751,51062,11,blacksburgMSA))
#richmond Amelia low = 51007,Richmond city = 51760 (low and high)
print(check_numbers(51761,51006,11,richmondMSA))
# the highest point of the state is close to 39 lat, the low is close to 35
print(check_numbers(39,35,12,blacksburgMSA))
print(check_numbers(39,35,12,richmondMSA))
# the highest point of the state is close to 82.4 lat, the low is close to 35
print(check_numbers(-76,-82.4,13,blacksburgMSA))
print(check_numbers(-76,-82.4,13,richmondMSA))

#check skills
count(blackbskills,skill)
count(blackbskills,skillcluster)
count(blackbskills,skillclusterfamily)
count(richskills,skill)
count(richskills,skillcluster)
count(richskills,skillclusterfamily)




# ===============================================================#
# Now I begin to look at the resume data:
library(R.utils)
# I've already unzipped 449-452
gunzip("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/449_job_info.csv.gz")
gunzip("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/449_certification_info.csv.gz")
gunzip("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/449_education_info.csv.gz")
gunzip("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/449_personal_info.csv.gz")
gunzip("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/449_skill_info.csv.gz")

currentSeries = "449"
jobs <- read_csv(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_job_info.csv",sep=""))
certificates <- read_csv(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_certification_info.csv",sep=""))
education <- read_csv(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_education_info.csv",sep=""))
personal <- read_csv(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_personal_info.csv", sep =""))
skil <- read_csv(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_skill_info.csv",sep = ""))

View(jobs)
View(certificates)
View(education)
View(personal)
View(skil)

