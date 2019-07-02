#This is my profiling script STW:



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
all_vars <-c("bgtjobid","jobdate","occfam","occfamname","employer","city","state",
             "county", "fipsstate", "fipscounty","fips","lat","lon","bestfitmsaname",
             "onet","onetname","bgtocc","edu","maxedu","degree","exp", "jobhours")
profile_vars <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_main_2017_51.RDS')
skills <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_skills_2017_51.RDS')
certs <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_certs_2017_51.RDS')
cip  <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_cip_2017_51.RDS')
degrees  <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_degrees_2017_51.RDS')
major  <- readRDS('../stem_edu/data/stem_edu/working/burning_glass/ads_major_2017_51.RDS')
profile_vars2 <- profile_vars %>%
  select(all_vars)

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

# Check onet  is 10 characters
print(check_str(11,9,15,blacksburgMSA))
print(check_str(11,9,15,richmondMSA))
# Check onet name is between 2 and 50 characters
print(check_str(150,2,16,blacksburgMSA))
print(check_str(150,1,16,richmondMSA))
# Check bgtocc is 10 characters
print(check_str(11,9,17,blacksburgMSA))
print(check_str(11,9,17,richmondMSA))
# Check degree between 2 and 50
print(check_str(50,2,20,blacksburgMSA))
print(check_str(50,2,20,richmondMSA))
# Check the jobhours
print(check_str(50,2,22,blacksburgMSA))
print(check_str(50,2,22,richmondMSA))

# low education is 0, high is no more than 30
print(check_numbers(30,0,18,blacksburgMSA))
print(check_numbers(30,0,18,richmondMSA))
# low max education is 0, high is no more than 30
print(check_numbers(30,0,19,blacksburgMSA))
print(check_numbers(30,0,19,richmondMSA))
# low max education is 0, high is no more than 30
print(check_numbers(30,0,21,blacksburgMSA))
print(check_numbers(30,0,21,richmondMSA))


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
library(lubridate)

# I've already unzipped 449-452

currentSeries = "420"
gzip(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_job_info.csv",sep=""))
gzip(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_certification_info.csv",sep=""))
gzip(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_education_info.csv",sep=""))
gzip(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_personal_info.csv",sep=""))
gzip(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_skill_info.csv",sep=""))


overlap <- jobs$BGTResId %in% jobs2$BGTResId
overlap1 <- jobs2$BGTResId %in% jobs3$BGTResId
length(overlap1[overlap1 == TRUE])

View(jobs)

# Unzip and zip back up to only keep memory strain in working memory.
jobs <- read_csv(gunzip(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_job_info.csv.gz",sep="")))
gzip(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_job_info.csv",sep=""))

#Unzip and read all three files into a nested tibble
zipfiles <- list.files(path = "./data/original/DNA-Extract/Aggregated-Data") %>%
  as_tibble() %>%
  rename(data_zip_path = value) %>%
  mutate(
    data_zip_path = str_c("./data/original/DNA-Extract/Aggregated-Data/", data_zip_path),
    input_files = map(.x = data_zip_path, ~gzfile(.x) %>% fromJSON())
  )
#Unnest and clean unnecessary variables (i.e. almost all missing)
dna.dt <- zipfiles %>%
  unnest() %>%
  as.data.table() %>%
  dt_select(-c(data_zip_path, section)) #Section does not appear in the code book (DNA Snapshot)

# Selects only people's who's last job was in 2017
as.Date(jobs$jobISOEndDate, "%Y/%m/%d")
peeps2017 <- jobs %>%
  select(BGTResId, jobISOEndDate) %>%
  filter(as.Date(jobs$jobISOEndDate, "%Y/%m/%d") >= as.Date("2017-01-01"))
peeps2018 <- jobs %>%
  select(BGTResId, jobISOEndDate) %>%
  filter(as.Date(jobs$jobISOEndDate, "%Y/%m/%d") >= as.Date("2018-01-01"))

subset(peeps2017,!(BGTResId %in% peeps2018$BGTResId))




#+++++++++++++++++++++++++++++++++ READING IN RESUMES +++++++++++++++++++++++++++++++++++#

library(stringr)
# Read's the zipped file without unzipping them
job <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_job_info.csv.gz",sep=""))

# Create for loop 454 files, keep only relevant resumes each step of the way and combine to one table
bringTogether <- function(fileType) {
  peep <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/1","_",fileType,".csv.gz",sep=""), data.table = TRUE)
  for (i in (seq(2:5)+1)){
    peep<- merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",fileType,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
  }
  peep$msa <- sub("^(\\d{5}).*$", "\\1", peep$msa)
  either_resumes <- peep[is.na(str_extract(peep$msa, "40060")) | is.na(str_extract(peep$msa, "13980")),]
  # peep$rich_blacksburg <- ifelse((is.na(str_extract(peep$msa, "40060"))|is.na(str_extract(peep$msa, "13980"))), TRUE, FALSE)
  # if (is.na(str_extract(peep$msa, "40060"))) {
  #   peep$msa <- "40060"
  # } else if (is.na(str_extract(peep$msa, "13980"))) {
  #   peep$msa <- "13980"
  # } else {
  #   peep$msa <- peep$msa
  # }
  either_resumes <- peep[peep$rich_blacksburg,]
  eitherresid <- either_resumes$BGTResId
  peep <- peep[BGTResId %chin% eitherresid]
  for (j in (seq(1:89))) {
    x <- j * 5 + 1
    peep1 <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",x,"_",fileType,".csv.gz",sep=""), data.table = TRUE)
    for (i in (seq(2:5)+x)){
      peep1<- merge(peep1,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",fileType,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
    }
    peep1$msa <- sub("^(\\d{5}).*$", "\\1", peep1$msa)
    either_resumes <- peep1[peep1$msa == "40060" | peep1$msa == "13980",]
    eitherresid <- either_resumes$BGTResId
    peep1 <- peep1[BGTResId %chin% eitherresid]
    peep<- merge(peep,peep1,all=TRUE, by= names(peep))
  }
  for (i in (seq(1:4)+450)){
    peep <-merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",fileType,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
  }
  peep$msa <- sub("^(\\d{5}).*$", "\\1", peep$msa)
  either_resumes <- peep[peep$msa == "40060" | peep$msa == "13980",]
  eitherresid <- either_resumes$BGTResId
  peep <- peep[BGTResId %chin% eitherresid]
  return(peep)
}

# Get's all of the resume's personal info's together, takes ~5 mins to run
all_resumes <- bringTogether("personal_info")
#either_resumes <- all_resumes[is.na(str_extract(all_resumes$msa, "40060")) | is.na(str_extract(all_resumes$msa, "13980")),]

richmond_resumes <- all_resumes[all_resumes$msa == "40060",]
blacksburg_resumes <- all_resumes[all_resumes$msa == "13980",]
richresid <- richmond_resumes$BGTResId
blacksburgresid <- blacksburg_resumes$BGTResId

# Combines the other files into one based off of the people from our MSAs
combine_other <- function(msa, filet) {
  peep <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/1","_",filet,".csv.gz",sep=""), data.table = TRUE)
  for (i in (seq(2:5)+1)){
    peep<- merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",filet,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
  }
  peep <- peep[BGTResId %chin% msa]
  for (j in (seq(1:89))) {
    print(paste(as.character((j*100)/89), "%"))
    x <- j * 5 + 1
    peep1 <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",x,"_",filet,".csv.gz",sep=""), data.table = TRUE)
    for (i in (seq(2:5)+x)){
      peep1<- merge(peep1,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",filet,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
    }
    peep1 <- peep1[BGTResId %chin% msa]
    peep<- merge(peep,peep1,all=TRUE, by= names(peep))
  }
  for (i in (seq(1:4)+450)){
    peep <-merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",filet,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
  }
  peep <- peep[BGTResId %chin% msa]
  return(peep)
}

all_edu <- bringTogether("education_info")
all_job <- bringTogether("job_info")
all_skill <- bringTogether("skill_info")

# Gets all of the certificates that come from our MSAs, takes ~3 minutes to run
richmond_certs <- combine_other(richresid, "certification_info")
blacksburg_certs <- combine_other(blacksburgresid, "certification_info")

# Gets all of the education that come from our MSAs, takes ~20 minutes to run
richmond_edu <- combine_other(richresid, "education_info")
blacksburg_edu <- combine_other(blacksburgresid, "education_info")

# Gets all of the job history that come from our MSAs, takes ~ minutes to run
richmond_job <- combine_other(richresid, "job_info")
blacksburg_job <- combine_other(blacksburgresid, "job_info")


all_resumes <- read.table("../stem_edu/all_resumes.txt", sep="\t")
blacksburg_resumes <- all_resumes[all_resumes$msa == "13980",]
blacksburgresid <- blacksburg_resumes$BGTResId


# Gets all of the skills that come from our MSAs, takes ~ minutes to run
richmond_skill <- combine_other(richresid, "skill_info")
blacksburg_skill <- combine_other(blacksburgresid, "skill_info")

write.table(blacksburg_skill, "../stem_edu/blacksburg_skills.txt", sep="\t")

# Each step of the way get only virginia peeps and add them to a data table

# Continue until done



