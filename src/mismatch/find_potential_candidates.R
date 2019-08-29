library(data.table)
library(tidyverse)

loc <- "data/stem_edu/working/burning_glass_ad_combine_16_17"
rich_ad_skill <- fread(file.path(loc, "richmond_top5_stw_jobs_all_skills.csv"))
rich_ad_main <- fread(file.path(loc, "richmond_top5_stw_jobs_main.csv"))

nurse_id <- rich_ad_main[onetname == "Critical Care Nurses"]$bgtjobid
nurse_skill <- rich_ad_skill[bgtjobid %in% nurse_id]
length(unique(nurse_skill$bgtjobid))
nrow(nurse_skill)

nurse_skillcluster_sum <- nurse_skill %>% group_by(skillclusterfamily, skillcluster) %>% summarise(unique_skill = length(unique(skill)),
      skillcount = n(), perc_skills = round(n()/11523,2), unique_ad = length(unique(bgtjobid)), perc_ads = round(length(unique(bgtjobid))/1604,2)) %>%
  arrange(desc(skillcount))
nurse_na_skill_sum <- nurse_skill %>% filter(skillcluster == "na") %>% group_by(skill) %>% summarise(ad_count = n(), perc_all_skill =
     round(n()/11523,2), perc_ads = round(n()/1604,2)) %>% arrange(desc(ad_count))

nurse_skill_IC = unique(filter(nurse_skill, skillcluster == "Emergency and Intensive Care")$bgtjobid)

nurse_skill_no_IC <- unique(nurse_skill$bgtjobid[! nurse_skill$bgtjobid %in% nurse_skill_IC])

nurse_skillcluster_sum_no_IC <- nurse_skill %>% filter(bgtjobid %in% nurse_skill_no_IC) %>% group_by(skillclusterfamily, skillcluster) %>% summarise(unique_skill = length(unique(skill)),
                                                                                                         skillcount = n(), unique_ad = length(unique(bgtjobid)), perc_ads = round(length(unique(bgtjobid))/447,2)) %>% arrange(desc(skillcount))
  arrange(desc(skillcount))

  nurse_skill %>% filter(bgtjobid %in% nurse_skill_no_IC, skillcluster == "na") %>% group_by(skill) %>% summarise(unique_skill = length(unique(skill)),
                                                                                                                              skillcount = n(), unique_ad = length(unique(bgtjobid)), perc_ads = round(length(unique(bgtjobid))/447,2)) %>% arrange(desc(skillcount))
  arrange(desc(skillcount))

  nurse_skill %>% filter(bgtjobid %in% nurse_skill_no_IC, skillcluster == "Pediatrics") %>% group_by(skill) %>% summarise(unique_skill = length(unique(skill)),
                                                                                                                  skillcount = n(), unique_ad = length(unique(bgtjobid)), perc_ads = round(length(unique(bgtjobid))/447,2)) %>% arrange(desc(skillcount))
  arrange(desc(skillcount))
