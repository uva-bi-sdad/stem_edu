library(data.table)
library(tidyverse)

###Skills required by particular job in particular MSA
loc <- "data/stem_edu/working/burning_glass_ad_combine_16_17"
r_top5_skill <- fread(file.path(loc, "richmond_top5_stw_jobs_hard_skills.csv"))

##Narrowing down to maintenance workers
skill_req <- r_top5_skill[onetname == "Maintenance and Repair Workers, General"]
#looking at top skills, just to check for obvious duplicates:
skill_sum <- skill_req %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(adcount = n()) %>% arrange(desc(adcount))

#narrowing down to just skills that show up in 5% of job ads (or top 10):
skill_req_trim <- as.data.table(skill_sum[skill_sum$count >= length(unique(skill_req$bgtjobid))*.05,])
skill_req_10 <- as.data.table(skill_sum[1:10,])

###of the jobs that require the primary skill for maintenance workers (repair), what % are
###maintenance workers?
all_skill <- fread(file.path(loc, "combine_16_17_job_ad_skill.csv"))
repair_skill <- all_skill[skill == "Repair"]
repair_skill_ad <- unique(maint_skill$bgtjobid)

all_job <- fread(file.path(loc, "combine_16_17_job_ad_main.csv"))
repair_job <- rich_job[bgtjobid %in% has_repair]
repair_job_richmond <- repair_job[msaname == "Richmond, VA"]

###Skills that people have in a particular MSA

loc_res <- "data/stem_edu/working/MSA_Resumes"
skill_exist <- read.table(file.path(loc_res, "richmond_skills.txt"), stringsAsFactors = FALSE)
str(skill_exist)
colnames(skill_exist) <- c("bgtresid", "skillid", "skill", "skillcluster", "skillclusterfamily",
                           "isbaseline", "issoftware", "isspecialized")
skill_exist <- as.data.table(skill_exist)

skill_exist_sum <- skill_exist %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(count = n()) %>%
  arrange(desc(count))

skill_exist_trim <- skill_exist[skill %chin% skill_req_10$skill]

skill_exist_trim_sum <- skill_exist_trim %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% arrange(desc(count))
colnames(skill_exist_trim_sum) <- c("skillclusterfamily", "skillcluster", "skill", "resumecount")

has_repair <- unique(filter(skill_exist, skill == "Repair")$bgtresid)

has_repair_skill_sum <- skill_exist %>% filter(bgtresid %in% has_repair) %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(has_rep_count = n()) %>% arrange(desc(has_rep_count))

###For top 10 skills (or, whatever)--how do the population and job ads compare?

skill_combine <- left_join(skill_req_10, skill_exist_trim_sum[,c("skill", "resumecount")], by = "skill")
skill_combine <- left_join(skill_combine, has_repair_skill_sum[,c("skill", "has_rep_count")], by = "skill")

skill_combine <- mutate(skill_combine, rep_fill = has_rep_count/adcount)

#this looks pretty good--but keeping in mind that only 10% of job ads that request Repair are
#actually Maintenance and Repair Workers, General... will only 10% of the "repair" workforce go
#there?

###what are the Very Necessary Skills for other top jobs that use Repair?
rich_rep_extra_skill <- all_skill %>% filter(bgtjobid %in% repair_skill_ad)
rich_rep_extra_skill <- left_join(rich_rep_extra_skill, repair_job[,c("bgtjobid","onet", "onetname")])

rich_rep_job_10 <- repair_job_richmond_sum$onetname[1:10]
rich_rep_extra_skill_10 <- rich_rep_extra_skill[rich_rep_extra_skill$onetname %in% rich_rep_job_10,]

repcount <- rich_rep_extra_skill_10 %>% filter(skill == "Repair") %>% group_by(onet, onetname) %>% summarise(repcount = n())
repair_job_richmond_sum_10 <- repair_job_richmond_sum[1:10,] %>% left_join(repcount[,c("onet","onetname","repcount")])
