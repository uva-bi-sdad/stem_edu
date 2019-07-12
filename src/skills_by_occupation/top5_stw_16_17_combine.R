###Now that we have 16 and 17 data, finding ALL most common STW occupations among ads that do NOT require
###a bachelor's degree, separately in Richmond and Blacksburg

library(tidyverse)
library(data.table)

###load in all jobs and skills in 2016 and 2017

loc <- file.path("data/stem_edu/working/burning_glass_ad_combine_16_17")
job_main <- fread(file.path(loc, "combine_16_17_job_ad_main.csv"))
job_skill <- fread(file.path(loc, "combine_16_17_job_ad_skill.csv"))
job_main <- job_main[,2:54]
job_skill <- job_skill[,2:10]

###load in list of STW occupations
loc <- file.path("src/ONET_define")
stw_occs <- fread(file.path(loc, "Rothwell_STW_list.csv"))

###get down to just STW jobs
stw_job_main <- job_main[onet %in% stw_occs$onet]
stw_job_main$bgtjobid <- as.character(stw_job_main$bgtjobid)

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

##determining which hard skills go with jobs in these places

#this is temporary hard skill list--REPLACE ONCE WE HAVE FINISHED FORMALLY CREATING HARD SKILL LIST
loc3 <- file.path("data/stem_edu/working/skills_by_occupation")
hard_skills <- fread(file.path(loc3, "workingHardSkills.csv"))

findSkill <- function(jobList, skillList, hardSkillList){
  skillList$bgtjobid <- as.character(skillList$bgtjobid)
  jobList$bgtjobid <- as.character(jobList$bgtjobid)

  skillFilter <- filter(skillList, bgtjobid %in% jobList$bgtjobid, skill %in% hardSkillList$skill)
  skillFilter$bgtjobid <- as.character(skillFilter$bgtjobid)

  skillFilter <- left_join(skillFilter, jobList[,c("bgtjobid", "onet", "onetname")], by = "bgtjobid")

  skillFilter
}

r_top5_skill <- findSkill(r_top5_main, job_skill, hard_skills)
b_top5_skill <- findSkill(b_top5_main, job_skill, hard_skills)

###Basic information about skills and skill clusters required for these occupations

skillDescribe <- function(file){
  ad_cluster_summary <- file %>% group_by(bgtjobid, onet, onetname, skillcluster, jobdate) %>%
    summarise(skillcount = n())
  ad_summary <- ad_cluster_summary %>% group_by(bgtjobid, onet, onetname, jobdate) %>%
    summarise(skillcount = sum(skillcount), clustercount = n())
  occ_summary <- ad_summary %>% group_by(onet, onetname) %>% summarise(ad_count = n(),
   avg_skill = round(mean(skillcount),1), min_skill = min(skillcount), first_quart_skill = round(quantile(skillcount,
    probs = .25),1), median_skill = median(skillcount), third_quart_skill = quantile(skillcount, probs = .75),
    max_skill = max(skillcount), avg_cluster = round(mean(clustercount),1), min_cluster = min(clustercount),
    first_quart_cluster = round(quantile(clustercount, probs = .25),1), median_cluster = median(clustercount),
   third_quart_cluster = round(quantile(clustercount, probs = .75),1), max_cluster = max(clustercount))
  occ_summary
}

r_occ_describe <- skillDescribe(r_top5_skill)
b_occ_describe <- skillDescribe(b_top5_skill)

base_spec_desc <- function(file){
  ad_summary <- file %>% group_by(bgtjobid, onet, onetname) %>%
    summarise(skillcount = n(), baselinecount = sum(isbaseline), specializedcount = sum(isspecialized))
  occ_summary <- ad_summary %>% group_by(onet, onetname) %>% summarise(adcount = n(),
    firstquart_allskill = round(quantile(skillcount, probs = .25),1), median_allskill = median(skillcount),
    thirdquart_allskill = round(quantile(skillcount, probs = .75),1), firstquart_baseline = round(quantile(baselinecount, probs = .25),1),
    median_baseline = median(baselinecount), thirdquart_baseline = round(quantile(baselinecount, probs = .75),1),
    firstquart_specialized = round(quantile(specializedcount, probs = .25),1),
    median_specialized = median(specializedcount),
    thirdquart_specialized = round(quantile(specializedcount, probs = .75), 1)) %>% arrange(desc(adcount))
  occ_summary
}

r_base_spec <- base_spec_desc(r_top5_skill)
b_base_spec <- base_spec_desc(b_top5_skill)

##baseline skills are pretty uncommon--what are the most common baseline skills in each place?

r_baseline <- r_top5_skill %>% filter(isbaseline == 1) %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% arrange(desc(count))
b_baseline <- b_top5_skill %>% filter(isbaseline == 1) %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% arrange(desc(count))

##if we look at only the skills that appear in .05% of ads for each occupation, how many unique skills are we looking at here?
##99 for nurses in Richmond
##281 for maintenance and repair workers in Richmond
##245 for computer use support specialists in Richmond
##172 for automotive speciality technicians in Richmond
##287 for web developers in Richmond
##632 unique skills in total in Richmond


r_nurse_skill <- r_top5_skill %>% filter(onetname == "Critical Care Nurses") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% filter(count >= nrow(r_top5_skill %>% filter(onetname == "Critical Care Nurses") %>% group_by(onetname))*.0005)
nrow(r_nurse_skill)

r_maint_skill <- r_top5_skill %>% filter(onetname == "Maintenance and Repair Workers, General") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% filter(count >= nrow(r_top5_skill %>% filter(onetname == "Maintenance and Repair Workers, General") %>% group_by(onetname))*.0005)
nrow(r_maint_skill)

r_comp_skill <- r_top5_skill %>% filter(onetname == "Computer User Support Specialists") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
       summarise(count = n()) %>% filter(count >= nrow(r_top5_skill %>% filter(onetname == "Computer User Support Specialists") %>% group_by(onetname))*.0005)
nrow(r_comp_skill)

r_aut_skill <- r_top5_skill %>% filter(onetname == "Automotive Specialty Technicians") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
       summarise(count = n()) %>% filter(count >= nrow(r_top5_skill %>% filter(onetname == "Automotive Specialty Technicians") %>% group_by(onetname))*.0005)
nrow(r_aut_skill)

r_web_skill <- r_top5_skill %>% filter(onetname == "Web Developers") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
       summarise(count = n()) %>% filter(count >= nrow(r_top5_skill %>% filter(onetname == "Web Developers") %>% group_by(onetname))*.0005)
nrow(r_web_skill)

r_all_top_skills <- rbind(r_nurse_skill, r_comp_skill, r_aut_skill, r_web_skill) %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(occTypes = n(), adCount = sum(count)) %>% arrange(desc(adCount))


##writing out skills and job ads
loc4 <- file.path("data/stem_edu/working/skills_by_occupation/top5_rb_separate")
write.csv(r_top5_main, file.path(loc4, "r_top5-stw-jobmain.csv"))
write.csv(b_top5_main, file.path(loc4, "b_top5-stw-jobmain.csv"))
write.csv(r_top5_skill, file.path(loc4, "r_top5-stw-jobskill.csv"))
write.csv(b_top5_skill, file.path(loc4, "b_top5-stw-jobskill.csv"))

###creating a 'weighted' metric for each skill--how critical is it to the job ads that it appears in?
weightScore <- function(skillfile){
  jobskillcount <- skillfile %>% group_by(bgtjobid) %>% summarise(skillcount = n())
  skillfile <- left_join(skillfile, jobskillcount)
  skillfile$weight <- 1/skillfile$skillcount
  output <- skillfile %>% group_by(onetname, skillclusterfamily, skillcluster, skill) %>%
    summarise(count = n(), weightSum = sum(weight), medianSkillCount = median(skillcount)) %>% arrange(desc(count))
  output
}

skillDist <- function(skillfile){
  jobskillcount <- skillfile %>% group_by(bgtjobid) %>% summarise(skillcount = n())
  skillfile <- left_join(skillfile, jobskillcount)
  skillfile$weight <- 1/skillfile$skillcount
  skillfile
}

r_skill_summary <- weightScore(r_top5_skill)

ggplot(r_nurse_summary, aes(x=count, y=weightSum)) + geom_point() + geom_smooth(method = "lm")

skillfile_nurse_dedupe <- r_top5_skill[r_top5_skill$onetname == "Critical Care Nurses",]
skillfile_nurse_dedupe[skillfile_nurse_dedupe$skill == "Neonatal Intensive Care","skill"] <- "Neonatal Intensive Care Unit (NICU)"
skillfile_nurse_dedupe[skillfile_nurse_dedupe$skill == "Critical Care Nursing","skill"] <- "Critical Care"

r_output_nurse_dedupe <- weightScore(skillfile_nurse_dedupe)
r_nurse_plot <- ggplot(output_nurse_dedupe, aes(x=count, y=weightSum)) + geom_point() +
  geom_smooth(method = "lm")+geom_text(aes(label = skill), size = 2, hjust=1) + ggtitle("Richmond Critical Care Nurse Skills")
r_median_nurse_plot <- ggplot(output_nurse_dedupe, aes(x=log(medianSkillCount), y=log(count))) + geom_point() +
  geom_text(aes(label = skill), size = 2, hjust=1) + ggtitle("Richmond Critical Care Nurses")

r_nurse_skill_dist <- skillDist(r_top5_skill[r_top5_skill$onetname == "Critical Care Nurses",])

b_nurse_summary <- weightScore(b_top5_skill[b_top5_skill$onetname == "Critical Care Nurses",])

r_nurse_histogram <- ggplot(data = r_nurse_skill_dist[r_nurse_skill_dist$skill %in% c("Critical Care", "Neonatal Intensive Care Unit (NICU)",
         "Advanced Cardiac Life Support (ACLS)", "Patient Evaluation", "Blood Administration"),])+ facet_grid(rows = vars(skill))+
  geom_vline(data=filter(r_nurse_skill_dist, skill=="Advanced Cardiac Life Support (ACLS)"), aes(xintercept=median(skillcount)))+
  geom_vline(data=filter(r_nurse_skill_dist, skill=="Blood Administration"), aes(xintercept=median(skillcount)))+
  geom_vline(data=filter(r_nurse_skill_dist, skill=="Critical Care"), aes(xintercept=median(skillcount))) +
  geom_vline(data=filter(r_nurse_skill_dist, skill=="Neonatal Intensive Care Unit (NICU)"), aes(xintercept=median(skillcount))) +
  geom_vline(data=filter(r_nurse_skill_dist, skill=="Patient Evaluation"), aes(xintercept=median(skillcount)))+
  geom_histogram(aes(x = skillcount), fill ="gray") + theme_bw()


skillfile_b_nurse_dedupe <- b_top5_skill[b_top5_skill$onetname == "Critical Care Nurses",]
skillfile_b_nurse_dedupe[skillfile_b_nurse_dedupe$skill == "Neonatal Intensive Care","skill"] <- "Neonatal Intensive Care Unit (NICU)"
skillfile_b_nurse_dedupe[skillfile_b_nurse_dedupe$skill == "Critical Care Nursing","skill"] <- "Critical Care"
b_output_nurse_dedupe <- weightScore(skillfile_b_nurse_dedupe)

b_nurse_plot <- ggplot(b_output_nurse_dedupe, aes(x=count, y=weightSum)) + geom_point() +
  geom_smooth(method = "lm")+geom_text(aes(label = skill), size = 2, hjust=1) + ggtitle("Blacksburg Critical Care Nurse Skills")

b_median_nurse_plot <- ggplot(b_output_nurse_dedupe, aes(x=medianSkillCount, y=count)) + geom_point() +
  geom_text(aes(label = skill), size = 2, hjust=1)+ggtitle("Richmond Critical Care Nurse Skills")

output_nurse_dedupe$avgWeight <- output_nurse_dedupe$weightSum/output_nurse_dedupe$count
output_nurse_dedupe_trim <- output_nurse_dedupe[output_nurse_dedupe$count >= 5,]
ggplot(output_nurse_dedupe_trim, aes(x=count, y=avgWeight)) + geom_point() + geom_text(aes(label = skill), size = 2, hjust=1)

skillfile_maint <- filter(skillfile, onetname == "Maintenance and Repair Workers, General")
output_maint <- skillfile_maint %>% group_by(onetname, skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n(), weightSum = sum(weight), medianSkillCount = median(skillcount)) %>% arrange(desc(count))
output_maint_nrepair <- output_maint[2:731,]

r_maint_plot <- ggplot(output_maint, aes(x=count, y=weightSum)) + geom_point() + geom_smooth(method = "lm")+geom_text(aes(label = skill), size = 2, hjust=1)+ ggtitle("Richmond maintenance worker skills")

r_maint_plot_nrepair <- ggplot(output_maint_nrepair, aes(x=count, y=weightSum)) + geom_point() + geom_smooth(method = "lm")+geom_text(aes(label = skill), size = 2, hjust=1) + ggtitle("Richmond maintenance worker skills (without repair)")

skillfile_comp <- filter(skillfile, onetname == "Computer User Support Specialists")
output_comp <- skillfile_comp %>% group_by(onetname, skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n(), weightSum = sum(weight), medianSkillCount = median(skillcount)) %>% arrange(desc(count))
r_comp_plot <- ggplot(output_comp, aes(x=count, y=weightSum)) + geom_point() + geom_smooth(method = "lm")+geom_text(aes(label = skill), size = 2, hjust=1) + ggtitle("Richmond computer support skills")


ggplot(output_comp, aes(x=count, y=weightSum)) + geom_point() + geom_smooth(method = "lm")+geom_text(aes(label = skill), size = 2, hjust=1)
