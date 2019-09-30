###Finding most common STW occupations among ads that do NOT require a bachelor's degree,
###separately in Richmond and Blacksburg

library(tidyverse)
library(data.table)

##reading in data

loc <- file.path("data/stem_edu/working/Team_SA_job_skills_filter")
b_stw <- fread(file.path(loc, "rothwell_blacksburg_stw_job.csv"))
r_stw <- fread(file.path(loc, "rothwell_richmond_stw_job.csv"))

loc2 <- file.path("data/stem_edu/working/burning_glass")
bgt_skills <- readRDS(file.path(loc2, "ads_skills_2017_51.RDS"))

loc3 <- file.path("data/stem_edu/working/skills_by_occupation")
hard_skills <- fread(file.path(loc3, "workingHardSkills.csv"))

#removing irrelevant and problematic V1 columns
r_stw <- r_stw[,3:56]
b_stw <- b_stw[,3:56]

top5job <- function(adList){
  assocBelow <- adList %>% filter(edu <= 14 | is.na(edu) == TRUE, jobhours != "parttime")
  jobDesc <- assocBelow %>% group_by(onet, onetname) %>% summarise(count = n()) %>% arrange(desc(count))
  top5 <- jobDesc[1:5,]
  print(top5)

  jobFilter <- filter(assocBelow, onetname %in% top5$onetname)
  jobFilter$bgtjobid <- as.character(jobFilter$bgtjobid)

  jobFilter
}

r_top5_main <- top5job(r_stw)
b_top5_main <- top5job(b_stw)

findSkill <- function(jobList, skillList, hardSkillList){
  skillList$bgtjobid <- as.character(skillList$bgtjobid)
  jobList$bgtjobid <- as.character(jobList$bgtjobid)

  skillFilter <- filter(skillList, bgtjobid %in% jobList$bgtjobid, skill %in% hardSkillList$skill)
  skillFilter$bgtjobid <- as.character(skillFilter$bgtjobid)

  skillFilter <- left_join(skillFilter, jobList[,c("bgtjobid", "onet", "onetname")], by = "bgtjobid")

  skillFilter
}

r_top5_skill <- findSkill(r_top5_main, bgt_skills, hard_skills)
b_top5_skill <- findSkill(b_top5_main, bgt_skills, hard_skills)

###Basic information about skills and skill clusters required for these occupations

skillDescribe <- function(file){
  ad_cluster_summary <- file %>% group_by(bgtjobid, onet, onetname, skillcluster, jobdate) %>%
    summarise(skillcount = n())
  ad_summary <- ad_cluster_summary %>% group_by(bgtjobid, onet, onetname, jobdate) %>%
    summarise(skillcount = sum(skillcount), clustercount = n())
  occ_summary <- ad_summary %>% group_by(onet, onetname) %>% summarise(ad_count = n(),
     avg_skill = mean(skillcount), min_skill = min(skillcount), first_quart_skill = quantile(skillcount,
     probs = .25), median_skill = median(skillcount), third_quart_skill = quantile(skillcount, probs = .75),
     ax_skill = max(skillcount), avg_cluster = mean(clustercount), min_cluster = min(clustercount),
     first_quart_cluster = quantile(clustercount, probs = .25), median_cluster = median(clustercount),
     third_quart_cluster = quantile(clustercount, probs = .75), max_cluster = max(clustercount))
  occ_summary
}

r_occ_describe <- skillDescribe(r_top5_skill)
b_occ_describe <- skillDescribe(b_top5_skill)

##writing out skills and job ads
loc4 <- file.path("data/stem_edu/working/skills_by_occupation/top5_rb_separate")
write.csv(r_top5_main, file.path(loc4, "r_top5-stw-jobmain.csv"))
write.csv(b_top5_main, file.path(loc4, "b_top5-stw-jobmain.csv"))
write.csv(r_top5_skill, file.path(loc4, "r_top5-stw-jobskill.csv"))
write.csv(b_top5_skill, file.path(loc4, "b_top5-stw-jobskill.csv"))
