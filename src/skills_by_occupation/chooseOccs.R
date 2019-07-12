library(tidyverse)
library(data.table)

####Finding most common STW positions in both Richmond and Blacksburg

#reading in STW jobs in Richmond/Blacksburg

loc <- file.path("data/stem_edu/working/Team_SA_job_skills_filter")
b_stw <- fread(file.path(loc, "rothwell_blacksburg_stw_job.csv"))
r_stw <- fread(file.path(loc, "rothwell_richmond_stw_job.csv"))

#combining into one dataframe

all_stw <- rbind(cbind(b_stw, place = "Blacksburg"), cbind(r_stw, place = "Richmond"))
all_stw <- all_stw[,3:57]

all_stw %>% group_by(place) %>% summarise(count = n())

#most common STW job postings

stw_desc <- all_stw %>% group_by(onet, onetname) %>% summarise(all_count = n(),
              all_perc = n()/length(unique(all_stw$onet))) %>% arrange(desc(all_count))

stw_desc$all_perc <- round(stw_desc$all_count/nrow(all_stw)*100,2)

b_count <- function(x){
  nrow(b_stw[b_stw$onet == x,])
}

r_count <- function(x){
  nrow(r_stw[r_stw$onet == x,])
}

stw_desc$b_count <- unname(sapply(stw_desc$onet, b_count))
stw_desc$b_perc <- round(stw_desc$b_count/nrow(b_stw)*100,2)

stw_desc$r_count <- unname(sapply(stw_desc$onet, r_count))
stw_desc$r_perc <- round(stw_desc$r_count/nrow(r_stw)*100,2)

sum(stw_desc$b_perc)
sum(stw_desc$r_perc)
sum(stw_desc$all_perc)

###three most common STW positions are critical care nurses (29-1141.03), computer user support specialists
###(15-1151.00), and Maintenance and Repair Workers, general (49-9071.00)

###finding the most common STW positions in Richmond and Blacksburg separately, looking only at ads that require
###a bachelor's degree



#removing irrelevant and problematic V1 columns
r_stw <- r_stw[,3:56]
b_stw <- b_stw[,3:56]

loc2 <- file.path("data/stem_edu/working/burning_glass")
bgt_skills <- readRDS(file.path(loc2, "ads_skills_2017_51.RDS"))

top5job <- function(adList){
  assocBelow <- adList %>% filter(edu <= 14 | is.na(edu) == TRUE, jobhours != "parttime")
  jobDesc <- assocBelow %>% group_by(onet, onetname) %>% summarise(count = n()) %>% arrange(desc(count))
  top5 <- jobDesc[1:5,]
  print(top5)

  filter(assocBelow, onetname %in% top5$onetname)
}

r_top5_main <- top5job(r_stw)
b_top5_main <- top5job(b_stw)

findSkill <- function(jobList, skillList){
  skillList$bgtjobid <- as.character(skillList$bgtjobid)
  jobList$bgtjobid <- as.character(jobList$bgtjobid)
  filter(skillList, bgtjobid %in% jobList$bgtjobid)
}

r_top5_skill <- findSkill(r_top5_main, bgt_skills)
b_top5_skill <- findSkill(b_top5_main, bgt_skills)

###what are the unique skills required for STW positions?

bgt_skills <- readRDS("~/stem_edu/data/stem_edu/working/burning_glass/ads_skills_2017_51.RDS")
bgt_jobs <- readRDS("~/stem_edu/data/stem_edu/working/burning_glass/ads_main_2017_51.RDS")

stw_job <- bgt_jobs %>% filter(onet %in% stw_desc$onet) %>%
  select(bgtjobid, onet, onetname, msa, msaname)

stw_skill <- bgt_skills %>% filter(bgtjobid %in% stw_job$bgtjobid)

length(unique(stw_skill$skill))
length(unique(bgt_skills$skill))

skill_desc <- stw_skill %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% arrange(desc(count))
skillcluster_desc <- skill_desc %>% group_by(skillcluster, skillclusterfamily) %>%
  summarise(skill_count = n(), ad_count = sum(count)) %>% arrange(desc(ad_count))
skillclusterfamily_desc <- skillcluster_desc %>% group_by(skillclusterfamily) %>%
  summarise(skillcluster = n(), skill = sum(skill_count), ad_count = sum(ad_count)) %>%
  arrange(desc(ad_count))

hist(skill_desc$count)
ggplot(skill_desc, aes(x=count)) + geom_histogram(bins = 100)
ggplot(skillcluster_desc, aes(x=ad_count)) + geom_histogram(bins = 100)

##how many skills required for nurses
nurse_job <- bgt_jobs %>% filter(onet == "29-1141.03")
nurse_skill <- bgt_skills %>% filter(bgtjobid %in% nurse_job$bgtjobid)
nurse_skill_desc <- nurse_skill %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% arrange(desc(count))
length(unique(nurse_skill_desc$skillclusterfamily))
nurse_skill_desc %>% group_by(skillclusterfamily) %>% summarise(count = n()) %>% arrange(desc(count))
nurse_skill_desc %>% group_by(skillcluster) %>% summarise(count = n()) %>% arrange(desc(count))

#how many skills are included less than 10 times among all stw jobs?
nrow(skill_desc %>% filter(count <= 10))
nrow(skill_desc %>% filter(count <= 20))
nrow(skill_desc %>% filter(count <= 50))

summary(skill_desc$count)
summary(nurse_skill_desc$count)

#how many skills does it take to cover 90% of listed skills in critical care nursing?
##(in retrospect this is: a bad idea), and also the code doesn't work
#going down the nurse_skill_desc table:
#nurse_skill_desc$cumulative_perc <- 0

#for(n in 1:nrow(nurse_skill_desc)){
#  if(n == 1){
#    nurse_skill_desc[n,"cumulative_perc"] <- nurse_skill_desc[n,"count"]/nrow(nurse_job)
#  } else {
#    skill_list <- nurse_skill_desc[1:n, "skill"]
#    nurse_skill_desc[n,"cumulative_perc"] <- length()/nrow(nurse_job)
#  }
#}

###how many skills are required per STW job?
skill_per_job <- stw_skill %>% group_by(bgtjobid) %>% summarise(count = n())
summary(skill_per_job)

###how many skills can we classify with skill cluster families and skill clusters?

scf_hard <- c("Information Technology", "Maintenance, Repair, and Installation", "Health Care", "Manufacturing and Production", "Architecture and Construction", "Engineering", "Analysis", "Personal Care and Services", "Energy and Utilities", "Environment", "Science and Research", "Agriculture, Horticulture, and the Outdoors")
scf_potential_soft <- c("na", "Finance", "Business", "Administration", "Customer and Client Support", "Design", "Supply Chain and Logistics", "Human Resources", "Industry Knowledge", "Media and Writing", "Sales", "Education and Training", "Marketing and Public Relations", "Public Safety and National Security", "Legal", "Economics, Policy, and Social Studies")

nrow(skillclusterfamily_desc)
length(scf_hard) + length (scf_potential_soft)

sum(scf_hard %in% skillclusterfamily_desc$skillclusterfamily) + sum(scf_potential_soft %in% skillclusterfamily_desc$skillclusterfamily)

sum(skillclusterfamily_desc[skillclusterfamily_desc$skillclusterfamily %in% scf_hard, "ad_count"])/nrow(stw_skill)

#so we can call 46% of skill requirements "hard skills" just based on the skill cluster family

#within the remaining, how many skill clusters exist? (275) how many skills? (3182)
sum(skillclusterfamily_desc[skillclusterfamily_desc$skillclusterfamily %in% scf_potential_soft, "skillcluster"])
sum(skillclusterfamily_desc[skillclusterfamily_desc$skillclusterfamily %in% scf_potential_soft, "skill"])

#how many skills in potential_soft have a skill that is requested at least 10 times? (1343)
#how many skill clusters contain skills that are requested at least 10 times? (223)
nrow(skill_desc %>% filter(count >= 10, skillclusterfamily %in% scf_potential_soft))
skill_desc %>% filter(count >= 10, skillclusterfamily %in% scf_potential_soft) %>% group_by(skillcluster) %>% summarise(ad_count = sum(count), skill_count = n()) %>% arrange(desc(ad_count))

#how many job ads represent 0.1% of STW ads in Blacksburg and Richmond? (~60). how many represent 0.05%? (30)
nrow(stw_job)*.0005
#seems reasonable to me to focus on skills that show up in 0.05% of job ads
#(that is, at least 30). that's 1,730 total skills.
nrow(skill_desc %>% filter(count >= 30))
#how many skills are NOT covered by scf_hard and are listed at least 30 times? (716)
nrow(skill_desc %>% filter(count >= 30, skillclusterfamily %in% scf_potential_soft))
#how many skill clusters are included in the above? (174)
length(unique((skill_desc %>% filter(count >= 30, skillclusterfamily %in% scf_potential_soft))$skillcluster))
#can I pare down those skill clusters to those that are potentially soft skills?
potential_soft_cluster_30plus <- skill_desc %>% filter(count >= 30, skillclusterfamily %in% scf_potential_soft) %>% group_by(skillclusterfamily, skillcluster) %>% summarise(skill_count = n())
#my (manual) classifications are in the file skillcluster_potential-soft_skill-30-plus.csv
loc <- (file.path("data/stem_edu/working/skills_by_occupation"))
cluster_class <- read.csv(file.path(loc, "skillcluster_potential-soft_skill-30-plus.csv"), stringsAsFactors = FALSE)
hard_cluster <- cluster_class[cluster_class$potential_soft. == FALSE, "skillcluster"]
pot_soft_cluster <- cluster_class[cluster_class$potential_soft. == TRUE, "skillcluster"]
#how many skills do I have to classify now? (189)
nrow(skill_desc %>% filter(skillcluster %in% pot_soft_cluster, count >= 30))
pot_soft_skill_30plus <- skill_desc %>% filter(skillcluster %in% pot_soft_cluster, count >= 30)
#writing that out for manual classificatoin
write.csv(pot_soft_skill_30plus, file.path(loc, "skill_potential-soft_30-plus.csv"))
#reading back in my manual classifications
skill_class <- read.csv(file.path(loc, "skill_potential-soft_30-plus_sorted.csv"))


###PUTTING IT ALL TOGETHER:
##1. only skills that appear in at least 0.05% of job ads
##2. skills in the unambiguously "hard" skill cluster families
##3. skills in the unambiguously "hard" skill clusters
##4. manually sorted skills

hard_skill_30filter <- hard_skill %>% filter(count >= 30, skillclusterfamily %in% scf_hard | skillcluster %in% hard_cluster | skill %in% skill_class[skill_class$hard == TRUE, "skill"])

write.csv(hard_skill_30filter, file.path(loc, "workingHardSkills.csv"))

###OR
## 1. skills in the unambiguously "hard" skill cluster families
## 2. skills in the unambiguously "hard" skill clusters
## 3. of the remaining skills, which appear in at least 0.05% of job ads? of those, which were manually sorted into "hard skills"
hard_skill <- skill_desc %>% filter(skillclusterfamily %in% scf_hard | skillcluster %in% hard_cluster | skill %in% skill_class[skill_class$hard == TRUE, "skill"])

#how much more coverage does hard_skill have than hard_skill_30filter? (22,299 job ads, or about 5% of the job ads in hard_skill)
(sum(hard_skill$count) - sum(hard_skill_30filter$count))/sum(hard_skill$count)

#how many hard skills are in critical care nursing, using 30filter?
nrow(filter(nurse_skill_desc, skill %in% hard_skill_30filter$skill))
#what non-hard skills are we excluding
nurse_skill_desc %>% filter(! skill %in% hard_skill_30filter$skill, count >= 30)
#how many hard skills per critical care nursing job (median of 4)
summary(nurse_skill %>% filter(skill %in% hard_skill_30filter$skill) %>% group_by(bgtjobid) %>% summarise(count = n()))

nurse_hard_skill <- nurse_skill %>% filter(skill %in% hard_skill_30filter$skill)
nurse_hard_skill_desc <- nurse_hard_skill %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% arrange(desc(count))
nurse_hs_comb <- expand.grid(nurse_hard_skill_desc$skill, nurse_hard_skill_desc$skill)

##pulling out ads/hard skills for top 3 STW occupations: critical care nurses (29-1141.03),
##computer user support specialists (15-1151.00), and Maintenance and Repair Workers, general (49-9071.00)
#going to ask everyone to think about a method to describe these occupations in terms
#of skills, and then to apply them to all 3 occupations so we can compare our results
#on Wednesday

top3job <- all_stw %>% filter(onet %in% c("29-1141.03", "15-1151.00", "49-9071.00"))
top3job$bgtjobid <- as.character(top3job$bgtjobid)
top3skill <- bgt_skills[bgtjobid %chin% top3job$bgtjobid]
top3skill <- inner_join(top3skill, top3job[,c("onet", "onetname", "bgtjobid", "place")], by = "bgtjobid")
top3skill <- as.data.table(top3skill)
top3hardskill <- top3skill[skill %chin% hard_skill$skill]

loc <- file.path("data/stem_edu/working/skills_by_occupation/top3occ")

write.csv(top3hardskill, file.path(loc,"skills_top3-stw-position.csv"))

###what % of each STW occupation requires a bachelor's degree or higher?

edu_cast <- dcast(all_stw, formula = onet + onetname ~ degree)

colnames(edu_cast) <- c("onet", "onetname", "assoc", "bach", "hs", "master","phd", "less_hs")
edu_cast <- select(edu_cast, onet, onetname, less_hs, hs, assoc, bach, master, phd)

edu_cast$bach_up_perc <- 0

for(i in 1:nrow(edu_cast)){
  edu_cast[i,"bach_up"] <- sum(edu_cast[i,"bach"], edu_cast[i,"master"], edu_cast[i,"phd"])
  edu_cast[i,"bach_up_perc"] <- sum(edu_cast[i,"bach"], edu_cast[i,"master"], edu_cast[i,"phd"])/sum(edu_cast[i,"less_hs"], edu_cast[i,"hs"], edu_cast[i,"assoc"], edu_cast[i,"bach"], edu_cast[i,"master"], edu_cast[i,"phd"])
  }
