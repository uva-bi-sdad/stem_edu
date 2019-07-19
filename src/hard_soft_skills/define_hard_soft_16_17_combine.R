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
skillclusterfamily_hard <- filter(skillclusterfamily_grouped, mostcautious == TRUE)

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
original_skillcluster_grouped_soft <- filter(skillcluster_grouped, mostcautious == FALSE)
original_skillcluster_grouped_hard <- filter(skillcluster_grouped, mostcautious == TRUE)

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
skillcluster_2_hard <- filter(skillcluster_2, mostcautious == TRUE)


skillcluster_soft_1 <- select(skillcluster_grouped_soft, skillclusterfamily,
                              skillcluster, skill_count, ad_count)
skillcluster_soft_2 <- select(skillcluster_2_soft, skillclusterfamily,
                              skillcluster, skills, ads)
colnames(skillcluster_soft_2) <- c("skillclusterfamily", "skillcluster",
                                   "skill_count", "ad_count")
skillcluster_soft <- rbind(skillcluster_soft_1, skillcluster_soft_2)

skillcluster_hard_1 <- select(skillcluster_grouped_hard, skillclusterfamily,
                              skillcluster, skill_count, ad_count)
skillcluster_hard_2 <- select(skillcluster_2_hard, skillclusterfamily,
                              skillcluster, skills, ads)
colnames(skillcluster_hard_2) <- c("skillclusterfamily", "skillcluster",
                                   "skill_count", "ad_count")
skillcluster_hard <- rbind(skillcluster_hard_1, skillcluster_hard_2)

###what skills do we now need to classify?

stw_skill_classify <- filter(stw_skill_desc_trim, skillcluster %in% skillcluster_soft$skillcluster)

###writing this out:
write.csv(stw_skill_classify, file.path(loc, "skill_for_validation.csv"))

###looking at skills classifications

loc <- file.path("data/stem_edu/working/hard_soft_skills/for_validation")
s_v <- fread(file.path(loc, "skill_classified_vicki.csv"))
s_a <- fread(file.path(loc, "skill_classified_vicki.csv"))
s_s <- fread(file.path(loc, "skill_classified_sarah.csv"))
s_c <- fread(file.path(loc, "skill_classified_calvin.csv"))
s_s2 <- fread(file.path(loc, "skill_classified_sam.csv"))
colnames(s_v)

colnames(s_v)[5] <- "vicki"
colnames(s_a)[5] <- "alyssa"
colnames(s_s)[5] <- "sarah"
colnames(s_c)[5] <- "calvin"
colnames(s_s2)[5] <- "sam"

s_v <- s_v[,1:5]
s_a <- s_a[,1:5]
s_c <- s_c[,1:5]

team_skill <- left_join(s_v, s_a)
team_skill <- left_join(team_skill, s_s)
team_skill <- left_join(team_skill, s_c)
team_skill <- left_join(team_skill, s_s2)

str(team_skill)

unique(team_skill$sam)

team_skill[team_skill$sam %in% c("ERROR COULD BE SOFTWARE", "ERROR!"), "sam"] <- NA

team_skill$sam <- as.logical(team_skill$sam)

str(team_skill)

#checked with sarah, sam, and calvin, "organizational skills" is soft

team_skill[team_skill$skill == "Organizational Skills",c("calvin", "sarah", "sam")] <- FALSE

team_skill$sum <- team_skill$alyssa + team_skill$calvin + team_skill$sarah + team_skill$vicki + team_skill$sam

team_skill %>% group_by(sum) %>% summarise(n())

##what skills weren't classified by the whole team?
team_skill <- as.data.table(team_skill)
team_skill_na <- team_skill[is.na(sum) == TRUE]

##looking at skills that had disagreement

team_skill$agreement <- character()

for(i in 1:nrow(team_skill)){
  if(is.na(team_skill[i,"sum"]) == TRUE){
    team_skill[i,"agreement"] <- NA
  } else if(team_skill[i,"sum"] == 5  | team_skill[i,"sum"] == 0){
    team_skill[i,"agreement"] <- "agree"
  } else if (team_skill[i,"sum"] == 4 | team_skill[i,"sum"] == 1){
    team_skill[i,"agreement"] <- "soft agree"
  } else {
    team_skill[i,"agreement"] <- "disagree"
  }
}

team_skill_disagree <- filter(team_skill, agreement == "disagree")

write.csv(team_skill_disagree, file.path(loc,"skill_disagree.csv"))

team_skill_agree <- filter(team_skill, agreement == "agree")

###decision was to do majority-rules on hard-soft classifications, SO:

team_skill$hard_soft <- character()
for(i in 1:nrow(team_skill)){
  if(is.na(team_skill[i,"sum"]) == TRUE){
    team_skill[i,"hard_soft"] <- NA
  } else if(team_skill[i,"sum"] >= 3){
    team_skill[i,"hard_soft"] <- "hard"
  } else if(team_skill[i,"sum"]<=2){
    team_skill[i,"hard_soft"] <- "soft"
  } else {
    team_skill[i,"hard_soft"] <- "error"
  }
}

team_skill %>% group_by(hard_soft) %>% summarise(n())

#for the NA skills:

#we had previously agreed that all technology skills were hard, so putting the microsoft skills in the "hard" bucket:
team_skill[team_skill$skill %in% c("Microsoft Certified Professional Azure", "Microsoft Excel", "Microsoft Word"),"hard_soft"] <- "hard"
#the remaining two are "empower," which had 4 "softs," so calling it soft, and "na", which is a blank anyways
team_skill[team_skill$skill =="Empower","hard_soft"] <- "soft"

skill_hard <- filter(team_skill, hard_soft == "hard")
skill_soft <- filter(team_skill, hard_soft == "soft")

#####Putting hard/soft markers on the skill description table

stw_skill_desc_trim$hard_soft <- NA

for(i in 1:nrow(stw_skill_desc_trim)){
  if(stw_skill_desc_trim[i, "skillclusterfamily"] %in% skillclusterfamily_hard$skillclusterfamily){
    stw_skill_desc_trim[i, "hard_soft"] <- "hard"
  } else if(stw_skill_desc_trim[i, "skillcluster"] %in% skillcluster_hard$skillcluster){
    stw_skill_desc_trim[i,"hard_soft"] <- "hard"
  } else if(stw_skill_desc_trim[i, "skill"] %in% skill_hard$skill){
    stw_skill_desc_trim[i,"hard_soft"] <- "hard"
  } else if(stw_skill_desc_trim[i, "skill"] %in% skill_soft$skill) {
    stw_skill_desc_trim[i,"hard_soft"] <- "soft"
  } else
    stw_skill_desc_trim[i, "hard_soft"] <- NA
}

stw_skill_desc_trim %>% group_by(hard_soft) %>% summarise(count = n())

##writing out our FINAL LIST of STW hard and soft skill classifications:

loc <- file.path("data/stem_edu/working/hard_soft_skills")
write.csv(stw_skill_desc_trim, file.path(loc,"stw_hard-soft-skill.csv"))

##writing out the files that had votes and counts in them for last group of skills

hard_soft_clean <- team_skill[,c("skillclusterfamily","skillcluster","skill","ads","hard_soft")]

loc <- file.path("data/stem_edu/working/hard_soft_skills/for_validation")
write.csv(hard_soft_clean, file.path(loc,"hard-soft-skills_finalclass_clean.csv"))
write.csv(hard_soft, file.path(loc,"hard-soft-skills_finalclass_votes.csv"))

