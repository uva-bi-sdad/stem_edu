library(tidyverse)
library(data.table)

####Finding most common STW positions in Richmond and Blacksburg

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
