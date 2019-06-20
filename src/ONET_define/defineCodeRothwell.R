
library(tidyverse)
library(data.table)
library(readxl)
library(reshape2)

setwd("/home/af5ug/stem_edu")

knowledge <- read_excel("data/stem_edu/original/ONET_survey/Knowledge.xlsx")
education <- read_excel("data/stem_edu/original/ONET_survey/Education, Training, and Experience.xlsx")
titles <- read_excel("data/stem_edu/original/ONET_survey/Occupation Data.xlsx")
edu_cat <- read_excel("data/stem_edu/original/ONET_survey/Education, Training, and Experience Categories.xlsx")

###Trimming down knowledge data to what we actually need

colnames(knowledge) <- c("soc_code","title","element_id","element_name","scale_id",
                      "scale_name","data_value","n","se","lower_ci","upper_ci",
                      "recommend_suppress", "not_relevant", "date",
                      "domain_source")

knowledge <- filter(knowledge, scale_name == "Level", element_name %in%
                      c("Biology", "Building and Construction", "Chemistry",
                        "Computers and Electronics", "Design", "Economics and Accounting",
                        "Engineering and Technology", "Food Production", "Mathematics",
                        "Mechanical", "Medicine and Dentistry", "Physics",
                        "Production and Processing", "Telecommunications"), data_value >= 4.5)

##O*NET codes for occupations that require high levels of technical knowledge

know_occ <- unique(knowledge$soc_code)

####Filtering down to the education data we actually want

colnames(education) <- c("soc_code","title","element_id","element_name","scale_id",
                         "scale_name", "category", "data_value", "n","se","lower_ci",
                         "upper_ci", "recommend_suppress", "date", "domain_source")
#education %>% filter(soc_code == "11-1011.00", scale_id == "RL") %>% group_by(soc_code) %>% summarise(sum = sum(data_value))

education <- filter(education, scale_id == "RL")

edu_cast <- dcast(education, formula = soc_code ~ category, value.var = "data_value")

edu_cast$sub_bach <- 0

for(i in 1:nrow(edu_cast)){
  edu_cast[i,"sub_bach"] <- sum(edu_cast[i,2:6])
}

edu_occ <- unique(filter(edu_cast, sub_bach >= 50)$soc_code)

###Which occupation codes match both the education and knowledge criteria?

both_occ <- edu_occ[which(edu_occ %in% know_occ)]

###What are the actual job titles and information for these occupations?

colnames(titles) <- c("soc_code","title","description")

stw <- titles[which(titles$soc_code %in% both_occ),]
head(stw)

#checking up on some of the STW definitions
#artists: which of their skills are related here?
knowledge[which(knowledge$soc_code == "27-1013.00"),]
#education of accountants:
edu_cast[edu_cast$soc_code == "13-2011.01",]

###Finding how many jobs in Richmond and Blacksburg match this definition

b_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/blacksburg_jobs.csv")
r_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/rich_jobs.csv")

b_job_stw <- b_job[onet %chin% stw$soc_code]
r_job_stw <- r_job[onet %chin% stw$soc_code]

##some basic information about each

#number of STW positions
nrow(b_job_stw)
nrow(r_job_stw)


#STW positions as percentage of all positions
nrow(b_job_stw)/nrow(b_job)
nrow(r_job_stw)/nrow(r_job)

#number of unique STW occupations
length(unique(b_job_stw$onet))
length(unique(r_job_stw$onet))

#STW occupations as percentage of all occupations
length(unique(b_job_stw$onet))/length(unique(b_job$onet))
length(unique(r_job_stw$onet))/length(unique(r_job$onet))
