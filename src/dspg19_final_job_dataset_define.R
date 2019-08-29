####CREATING FINAL JOB AD DATA FILES FOR ANALYSIS####

library(data.table)
library(tidyverse)
library(lubridate)

###Files needed: all main job ads and job ad skills in Virginia for 2016 and 2017, classification of
###skills as hard and soft, classificatoins of ONET codes as STW/non-STW


###load in all jobs and skills in 2016 and 2017

loc <- file.path("data/stem_edu/working/burning_glass_ad_combine_16_17")
job_main <- fread(file.path(loc, "combine_16_17_job_ad_main.csv"))
job_skill <- fread(file.path(loc, "combine_16_17_job_ad_skill.csv"))
job_main <- job_main[,2:54]
job_skill <- job_skill[,2:10]

#cleaning up -999 as NA in "edu" in main job file
job_main %>% group_by(edu) %>% summarise(count = n())
job_main[job_main$edu == -999, "edu"] <- NA
job_main %>% group_by(edu) %>% summarise(count = n())


###load in list of STW occupations
loc <- file.path("src/ONET_define")
stw_occs <- fread(file.path(loc, "Rothwell_STW_list.csv"))

###get down to just STW jobs
stw_job_main <- job_main[onet %chin% stw_occs$onet]
stw_job_main$bgtjobid <- as.character(stw_job_main$bgtjobid)

#how many of these jobs do not require a bachelor's degree or above?
stw_job_main %>% group_by(edu) %>% summarise(count = n())
nrow(filter(stw_job_main, edu <= 14 | is.na(edu) == TRUE))

##splitting to Richmond and Blacksburg
r_stw_job_main <- filter(stw_job_main, msaname == "Richmond, VA")
b_stw_job_main <- filter(stw_job_main, msaname == "Blacksburg-Christiansburg-Radford, VA")


###determining top 5 jobs and associated skills in each place
top5job <- function(adList){
  assocBelow <- adList %>% filter(edu <= 14 | is.na(edu) == TRUE, jobhours != "parttime", internship == 0)
  jobDesc <- assocBelow %>% group_by(onet, onetname) %>% summarise(count = n()) %>% arrange(desc(count))
  top5 <- jobDesc[1:5,]
  print(top5)

  jobFilter <- filter(assocBelow, onetname %in% top5$onetname)
  jobFilter$bgtjobid <- as.character(jobFilter$bgtjobid)

  jobFilter
}

r_top5_main <- top5job(r_stw_job_main)
b_top5_main <- top5job(b_stw_job_main)

###Getting skills data for STW jobs in Richmond and Blacksburg

##both hard and soft skills:
loc <- file.path("data/stem_edu/working/hard_soft_skills")
hard_soft_skills <- fread(file.path(loc, "stw_hard-soft-skill.csv"))

findAllSkill <- function(jobList, skillList, skillClassList){
  skillList$bgtjobid <- as.character(skillList$bgtjobid)
  jobList$bgtjobid <- as.character(jobList$bgtjobid)
  skillClass <- select(skillClassList, skill, hard_soft)

  skillFilter <- filter(skillList, bgtjobid %in% jobList$bgtjobid)
  skillFilter$bgtjobid <- as.character(skillFilter$bgtjobid)

  skillFilter <- left_join(skillFilter, jobList[,c("bgtjobid", "onet", "onetname")], by = "bgtjobid")

  skillFilter <- inner_join(skillFilter, skillClass)

  skillFilter
}

r_top5_job_all_skill <- findAllSkill(r_top5_main, job_skill, hard_soft_skills)
b_top5_job_all_skill <- findAllSkill(b_top5_main, job_skill, hard_soft_skills)

##finding just hard skills
hard_skills <- hard_soft_skills[hard_soft == "hard"]

findHardSkill <- function(jobList, skillList, hardSkillList){
  skillList$bgtjobid <- as.character(skillList$bgtjobid)
  jobList$bgtjobid <- as.character(jobList$bgtjobid)

  skillFilter <- filter(skillList, bgtjobid %in% jobList$bgtjobid, skill %in% hardSkillList$skill)
  skillFilter$bgtjobid <- as.character(skillFilter$bgtjobid)

  skillFilter <- left_join(skillFilter, jobList[,c("bgtjobid", "onet", "onetname")], by = "bgtjobid")

  skillFilter
}

r_top5_job_hard_skill <- findHardSkill(r_top5_main, job_skill, hard_skills)
b_top5_job_hard_skill <- findHardSkill(b_top5_main, job_skill, hard_skills)


###Sanity checks
#should only return top 5 job types:
r_top5_job_all_skill %>% group_by(onetname) %>% summarise(count = n()) %>% arrange(desc(count))
b_top5_job_all_skill %>% group_by(onetname) %>% summarise(count = n()) %>% arrange(desc(count))

#should include some soft skills:
r_top5_job_all_skill %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))
b_top5_job_all_skill %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))

#should NOT include soft skills, and should match numbers of above:
r_top5_job_hard_skill %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))
b_top5_job_hard_skill %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))

###Writing out data sets:

loc <- file.path("data/stem_edu/working/burning_glass_ad_combine_16_17")
write.csv(r_top5_main, file.path(loc, "richmond_top5_stw_jobs_main.csv"))
write.csv(b_top5_main, file.path(loc, "blacksburg_top5_stw_jobs_main.csv"))
write.csv(r_top5_job_all_skill, file.path(loc, "richmond_top5_stw_jobs_all_skills.csv"))
write.csv(b_top5_job_all_skill, file.path(loc, "blacksburg_top5_stw_jobs_all_skills.csv"))
write.csv(r_top5_job_hard_skill, file.path(loc, "richmond_top5_stw_jobs_hard_skills.csv"))
write.csv(b_top5_job_hard_skill, file.path(loc, "blacksburg_top5_stw_jobs_hard_skills.csv"))
