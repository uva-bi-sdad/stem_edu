library(data.table)
library(tidyverse)

####THIS OCCURRED AFTER define_hard_soft_initial output files were classified--but those only looked at 2017.

###load in all jobs and skills in 2016 and 2017

loc <- file.path("data/stem_edu/working/burning_glass_ad_combine_16_17")
job_main <- fread(file.path(loc, "combine_16_17_job_ad_main.csv"))
job_skill <- fread(file.path(loc, "combine_16_17_job_ad_skill.csv"))

###load in list of STW occupations
loc <- file.path("src/ONET_define")
stw_occs <- fread(file.path(loc, "Rothwell_STW_list.csv"))

###get down to just STW jobs
stw_job_main <- job_main[onet %in% stw_occs$onet]
stw_job_main$bgtjobid <- as.character(stw_job_main$bgtjobid)
job_skill$bgtjobid <- as.character(job_skill$bgtjobid)
stw_job_skill <- job_skill[bgtjobid %chin% stw_job_main$bgtjobid]

###group/describe all STW skills
stw_skill_desc <- stw_job_skill %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(ads = n())

###skills that show up in at least .05% of STW job ads
stw_skill_desc_trim <- filter(stw_skill_desc, ads >= nrow(stw_job_main)*.0005)

##all skill clusters that show up in trimmed STW job skills
stw_skillcluster_desc <- stw_skill_desc_trim %>% group_by(skillclusterfamily, skillcluster) %>% summarise(ads = sum(ads), skills = n())

##all skill cluster families hat show up in trimmed STW job skills
stw_skillclusterfamily_desc <- stw_skillcluster_desc %>% group_by(skillclusterfamily) %>%
  summarise(skillclusters = n(), skills = sum(skills), ads = sum(ads))

#####WHAT HAS ALREADY BEEN GROUPED BY US####

loc <- file.path("data/stem_edu/working/hard_soft_skills/for_validation")

###skill cluster families
skillclusterfamily_grouped <- fread(file.path(loc, "skillclusterfamily_classified_together.csv"))
colnames(skillclusterfamily_grouped) <- c("skillclusterfamily", "skillcluster", "skill", "ad_count",
       "alyssa","calvin","sarah","agreement","mostcautious","morecautiousthanoriginal")
##any that still need to be grouped?
sum(!stw_skillclusterfamily_desc$skillclusterfamily %in% skillclusterfamily_grouped$skillclusterfamily)
##what skill cluster families need to be looked at it in more detail?
skillclusterfamily_grouped$sum <- skillclusterfamily_grouped$alyssa + skillclusterfamily_grouped$calvin + skillclusterfamily_grouped$sarah
for(i in 1:nrow(skillclusterfamily_grouped)){
  if(skillclusterfamily_grouped[i,"sum"] < 3){
    skillclusterfamily_grouped[i,"mostcautious"] <- FALSE
  } else {
    skillclusterfamily_grouped[i,"mostcautious"] <- TRUE
  }
}
skillclusterfamily_soft <- filter(skillclusterfamily_grouped, mostcautious == FALSE)
#determining skill clusters that we will need to look at in more detail:
stw_skillcluster_desc <- as.data.table(stw_skillcluster_desc)
skillcluster_potential_soft <- stw_skillcluster_desc[skillclusterfamily %chin% skillclusterfamily_soft$skillclusterfamily]
#sanity checking:
length(unique(skillcluster_potential_soft$skillclusterfamily)) == length(unique(skillclusterfamily_soft$skillclusterfamily))

###skill clusters
#what have we looked at so far?
skillcluster_grouped <- fread(file.path(loc, "skillcluster_classified_together.csv"))
colnames(skillcluster_grouped) <- c("skillclusterfamily", "skillcluster", "skill_count", "ad_count", "alyssa",
                                    "calvin", "sarah", "agreement", "morecautious")
#which of the ones we looked at might be soft?
skillcluster_grouped$sum <- skillcluster_grouped$alyssa + skillcluster_grouped$calvin + skillcluster_grouped$sarah
for(i in 1:nrow(skillcluster_grouped)){
  if(skillcluster_grouped[i,"sum"] < 3){
    skillcluster_grouped[i,"mostcautious"] <- FALSE
  } else {
    skillcluster_grouped[i,"mostcautious"] <- TRUE
  }
}
skillcluster_grouped_soft <- filter(skillcluster_grouped, mostcautious == FALSE)
#any potentially soft skill clusters we haven't looked at? (yes, 58)
sum(!skillcluster_potential_soft$skillcluster %in% skillcluster_grouped$skillcluster)
#what potentially soft skill clusters HAVEN'T we looked at?
skillcluster_to_group <-skillcluster_potential_soft[!skillcluster %in% skillcluster_grouped$skillcluster]

###write out additional skill clusters to group:
write.csv(skillcluster_to_group, file.path(loc,"skillcluster_for_validation_round2.csv"))

##how many skills are we going to have to classify(if none of the additional skill clusters are potentially
##soft)? 398 (ouch)
stw_skill_desc_trim <- as.data.table(stw_skill_desc_trim)
nrow(stw_skill_desc_trim[skillcluster %chin% skillcluster_grouped_soft$skillcluster])

###now that we have done the second round of skill cluster grouping
skillcluster_2 <- fread(file.path(loc, "skillcluster_round2_classified_together.csv"))
colnames(skillcluster_2) <- c("skillclusterfamily", "skillcluster", "ads",
                              "skills", "calvin", "sarah", "alyssa")
skillcluster_2$sum <- skillcluster_2$alyssa + skillcluster_2$calvin + skillcluster_2$sarah
for(i in 1:nrow(skillcluster_2)){
  if(skillcluster_2[i,"sum"] < 3){
    skillcluster_2[i,"mostcautious"] <- FALSE
  } else {
    skillcluster_2[i,"mostcautious"] <- TRUE
  }
}
skillcluster_2_soft <- filter(skillcluster_2, mostcautious == FALSE)

skillcluster_grouped_soft

skillcluster_soft_1 <- select(skillcluster_grouped_soft, skillclusterfamily,
                              skillcluster, skill_count, ad_count)
skillcluster_soft_2 <- select(skillcluster_2_soft, skillclusterfamily,
                              skillcluster, skills, ads)
colnames(skillcluster_soft_2) <- c("skillclusterfamily", "skillcluster",
                                   "skill_count", "ad_count")

skillcluster_soft <- rbind(skillcluster_soft_1, skillcluster_soft_2)

###what skills do we now need to classify?

stw_skill_classify <- filter(stw_skill_desc_trim, skillcluster %in% skillcluster_soft$skillcluster)

###writing this out:
write.csv(stw_skill_classify, file.path(loc, "skill_for_validation.csv"))
