#######Process for determining hard vs soft skills in Burning Glass data

##housekeeping:

library(tidyverse)
library(data.table)

#######There is an overwhelming quantity of skills in all the BGT data, so for the purpose of
#######this study, we only care about the skills that show up in STW occupations.

####getting to just-STW skills data####

#all Rothwell STW occupations:
loc <- file.path("src/ONET_define")
stw_occ <- fread(file.path(loc, "Rothwell_STW_list.csv"))

#all job postings:
loc <- file.path("data/stem_edu/working/burning_glass")
bgt_jobs <- readRDS(file.path(loc, "ads_main_2017_51.RDS"))
bgt_jobs <- as.data.table(bgt_jobs)

#down to just STW-related job postings
stw_jobs <- bgt_jobs[onet %chin% stw_occ$onet]

#skills related to those job postings
bgt_skills <- readRDS(file.path(loc, "ads_skills_2017_51.RDS"))
stw_skills <- bgt_skills[bgtjobid %chin% stw_jobs$bgtjobid]

####some basic descriptives####

#How many unique skills required for STW jobs in Virginia in 2017? (6,130, or 57% of all skills)
length(unique(stw_skills$skill))
length(unique(stw_skills$skill))/length(unique(bgt_skills$skill))

#taking a look at individual STW skills/skill clusters/skill cluster families:
stw_skill_desc <- stw_skills %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% arrange(desc(count))
stw_skillcluster_desc <- stw_skill_desc %>% group_by(skillcluster, skillclusterfamily) %>%
  summarise(skill_count = n(), ad_count = sum(count)) %>% arrange(desc(ad_count))
stw_skillclusterfamily_desc <- stw_skillcluster_desc %>% group_by(skillclusterfamily) %>%
  summarise(skillcluster = n(), skill = sum(skill_count), ad_count = sum(ad_count)) %>%
  arrange(desc(ad_count))

#looking at distribution of how often STW skills show up:
ggplot(stw_skill_desc, aes(x=count)) + geom_histogram(bins = 100)

#how many skills appear in at least 0.05% of STW job postings (30 job postings)? (1735 skills)
nrow(stw_jobs)*.0005
stw_skill_desc_trim <- filter(stw_skill_desc, count >=30)

###Looking at skill cluster families of skills that appear in at least 0.05% of STW job ads

stw_skillcluster_desc_trim <- stw_skill_desc_trim %>% group_by(skillcluster, skillclusterfamily) %>%
  summarise(skill_count = n(), ad_count = sum(count)) %>% arrange(desc(ad_count))
stw_skillclusterfamily_desc_trim <- stw_skillcluster_desc_trim %>% group_by(skillclusterfamily) %>%
  summarise(skillcluster = n(), skill = sum(skill_count), ad_count = sum(ad_count)) %>%
  arrange(desc(ad_count))

###There are 28 of these skill families, some of which look unambiguously "hard" to me (Alyssa) and others
###that could include both hard/soft skills. Put this out in a CSV to manually classify:
loc <- file.path("data/stem_edu/working/hard_soft_skills/for_validation")
write.csv(stw_skillclusterfamily_desc_trim, file.path(loc,"skillclusterfamily_for_validation.csv"))

#Re-upload classified skills:
name <- "skillclusterfamily_classified_alyssa.csv"
stw_skillclusterfamily_classified <- fread(file.path(loc, name))

#How many skill clusters could contain soft skills? (174)
sum(filter(stw_skillclusterfamily_classified, hard_skill == FALSE)$skillcluster)
#How many skill clusters are in the "hard" skill cluster families? (221)
sum(filter(stw_skillclusterfamily_classified, hard_skill == TRUE)$skillcluster)

###What are the skill clusters within the skill cluster families that could contain soft skills?
stw_skillcluster_potential_soft <- filter(stw_skillcluster_desc_trim, skillclusterfamily %in% filter(stw_skillclusterfamily_classified, hard_skill == FALSE)$skillclusterfamily)
stw_skillcluster_potential_soft <- select(stw_skillcluster_potential_soft, skillclusterfamily, skillcluster, skill_count, ad_count)
#(sanity checking that--should be 174:)
nrow(stw_skillcluster_potential_soft)

#write that out to manually classify:
write.csv(stw_skillcluster_potential_soft, file.path(loc,"skillcluster_for_validation.csv"))
