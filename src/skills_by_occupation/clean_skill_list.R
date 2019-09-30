library(data.table)
library(tidyverse)

loc <- "data/stem_edu/working/burning_glass_ad_combine_16_17"

r_top5_ad_skill <- fread(file.path(loc, "richmond_top5_stw_jobs_all_skills.csv"))
b_top5_ad_skill <- fread(file.path(loc, "blacksburg_top5_stw_jobs_all_skills.csv"))
r_top5_ad_main <- fread(file.path(loc, "richmond_top5_stw_jobs_main.csv"))
b_top5_ad_main <- fread(file.path(loc, "blacksburg_top5_stw_jobs_main.csv"))

alpha_skill <- function(ad_file, skill_file, jobname, threshhold = .005){
  #get list of ad IDs for the desired job
  ad <- unique(filter(ad_file, onetname == jobname)$bgtjobid)
  #find the skills that have those job IDs
  skill <- filter(skill_file, bgtjobid %in% ad)
  #determine the threshhold for skills of interest
  thresh_num <- length(ad)*threshhold

  #make alphabetical skill list
  skill %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(count = n()) %>%
    filter(count > thresh_num) %>% arrange(skill)
}

nurse_blacksburg_top_skill <- alpha_skill(ad_file = b_top5_ad_main, skill_file = b_top5_ad_skill, jobname = "Critical Care Nurses")
nurse_richmond_top_skill <- alpha_skill(ad_file = r_top5_ad_main, skill_file = r_top5_ad_skill, jobname = "Critical Care Nurses")
nurse_top_skill <- rbind(nurse_blacksburg_top_skill, nurse_richmond_top_skill) %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = sum(count)) %>% arrange(skill)

maintenance_blacksburg_top_skill <- alpha_skill(ad_file = b_top5_ad_main, skill_file = b_top5_ad_skill, jobname = "Maintenance and Repair Workers, General")
maintenance_richmond_top_skill <- alpha_skill(ad_file = r_top5_ad_main, skill_file = r_top5_ad_skill, jobname = "Maintenance and Repair Workers, General")
maintenance_top_skill <- rbind(maintenance_blacksburg_top_skill, maintenance_richmond_top_skill) %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = sum(count)) %>% arrange(skill)

computer_blacksburg_top_skill <- alpha_skill(ad_file = b_top5_ad_main, skill_file = b_top5_ad_skill, jobname = "Computer User Support Specialists")
computer_richmond_top_skill <- alpha_skill(ad_file = r_top5_ad_main, skill_file = r_top5_ad_skill, jobname = "Computer User Support Specialists")
computer_top_skill <- rbind(computer_blacksburg_top_skill, computer_richmond_top_skill) %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = sum(count)) %>% arrange(skill)

##duplicate skills for nurses: Critical Care/Critical Care Nursing, Neonatal Intensive Care/Neonatal Intensive Care Unit (NICU),
##Surgery/Surgical Services

##duplicate skills for maintenance workers: Environmental Laws and Regulations/	Environmental Regulations

##duplicate skills for computer support specialists: Computer Installation and Setup/PC Installation, Preventive Maintenance/Predictive / Preventative Maintenance

##generally, remove skills that are na/na/na

dupe_list <- as.data.frame(rbind(c("Critical Care", "Critical Care Nursing"), c("Neonatal Intensive Care Unit",
                "Neonatal Intensive Care Unit (NICU)"), c("Surgery", "Surgical Services"), c("Environmental Laws and Regulations",
                "Environmental Regulations"), c("PC Installation", "Computer Installation and Setup"), c("Preventive Maintenance", "Predictive / Preventative Maintenance")))

skill_clean <- function(dupe_list, skill_file){
  for(i in 1:nrow(dupe_list)){
    skill_file[skill_file$skill == dupe_list[i,1],"skill"] <- dupe_list[i,2]
  }
  skill_file <- filter(skill_file, skill != "na")
  skill_file
}

blacksburg_clean <- skill_clean(dupe_list = dupe_list, skill_file = b_top5_ad_skill)
blacksburg_clean_summary <- blacksburg_clean %>% group_by(skill) %>% summarise(count = n())
blacksburg_summary <- b_top5_ad_skill%>% group_by(skill) %>% summarise(count = n())
#looking at skills that are in the original file but not the clean file:
unique(b_top5_ad_skill$skill[which(unique(b_top5_ad_skill$skill) %in% unique(blacksburg_clean$skill) == FALSE)])

richmond_clean <- skill_clean(dupe_list = dupe_list, skill_file = r_top5_ad_skill)

write.csv(richmond_clean, file.path(loc, "richmond_top5_stw_job_all_skills_clean.csv"))
write.csv(blacksburg_clean, file.path(loc, "blacksburg_top5_stw_job_all_skills_clean.csv"))
