library(tidyverse)
library(data.table)
library(gtools)


loc <- file.path("data/stem_edu/working/skills_by_occupation/top3occ")
skills <- fread(file.path(loc, "hardskills_top3-stw-position.csv"))

str(skills)


ad_cluster_summary <- skills %>% group_by(bgtjobid, onet, onetname, skillcluster, jobdate, place) %>%
  summarise(skillcount = n())
ad_summary <- ad_cluster_summary %>% group_by(bgtjobid, onet, onetname, jobdate, place) %>%
  summarise(skillcount = sum(skillcount), clustercount = n())
occ_summary <- ad_summary %>% group_by(onet, onetname) %>% summarise(ad_count = n(),
                  avg_skill = mean(skillcount),
                  min_skill = min(skillcount), first_quart_skill = quantile(skillcount, probs = .25),
                  median_skill = median(skillcount), third_quart_skill = quantile(skillcount, probs = .75),
                  max_skill = max(skillcount), avg_cluster = mean(clustercount),
                  min_cluster = min(clustercount), first_quart_cluster = quantile(clustercount, probs = .25),
                  median_cluster = median(clustercount), third_quart_cluster = quantile(clustercount, probs = .75),
                  max_cluster = max(clustercount))


count_w_1_skill <- function(x){
  nrow(filter(ad_summary, skillcount == 1, onetname == x))
}

count_w_1_cluster <- function(x){
  nrow(filter(ad_summary, clustercount == 1, onetname == x))
}

id_w_1 <- (ad_summary[ad_summary$skillcount == 1 & ad_summary$onetname == "Critical Care Nurses", "bgtjobid"])[,1]
id_w_1 <- id_w_1$bgtjobid

skills[skills$bgtjobid %in% id_w_1, "skill"] %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))

occ_summary$count_w_1_skill <- sapply(occ_summary$onetname, count_w_1_skill)
occ_summary$count_w_1_cluster <- sapply(occ_summary$onetname, count_w_1_cluster)


nurse_skill <- skills %>% filter(onetname == "Critical Care Nurses") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n(), perc = round((n()/1180)*100, 2)) %>% arrange(desc(count))

nurse_cluster_sum <- skills %>% filter(onetname == "Critical Care Nurses") %>% group_by(skillclusterfamily, skillcluster, bgtjobid) %>%
  summarise(skill_count = n(), skill_perc = round((n()/1180)*100, 2)) %>% group_by(skillclusterfamily, skillcluster) %>%
  summarise(skill_count = sum(skill_count), ad_count = n()) %>% arrange(desc(ad_count))

nrow(skills %>% filter(onetname == "Critical Care Nurses") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% filter(count == 1))

ggplot(nurse_skill)+geom_point(aes(x=count, y = 1), alpha = .1)+theme_minimal()
ggplot(nurse_skill)+geom_histogram(aes(x=count), binwidth = 10) + theme_minimal()

#how many unique skills are listed for nurses (251)?
nrow(nurse_skill)
#how many unique skills are listed in at least .5% of nursing ads (6 ads)? (90)
1180*.005
nrow(filter(nurse_skill, count >= 6))

##repeated skills within/between clusters
write.csv(nurse_skill %>% arrange(skillcluster, skill), file.path(loc, "nurse_common_skill_cluster.csv"))

nurse_skill_2 <- expand.grid(filter(nurse_skill, perc >= .5)$skill, filter(nurse_skill, perc >= .5)$skill, stringsAsFactors = FALSE)
colnames(nurse_skill_2) <- c("skill1", "skill2")
nurse_skill_2 <- nurse_skill_2[nurse_skill_2$skill1 != nurse_skill_2$skill2,]

ad_id <- filter(ad_summary, onetname == "Critical Care Nurses")$bgtjobid


job_comb2 <- function(x, name = NA){
  skillcomb <- matrix(ncol = 2, nrow = 0)
  colnames(skillcomb) <- c("skill1", "skill2")

  if(is.na(name) == FALSE){
    skillcount <- x %>% filter(onetname == name) %>% group_by(bgtjobid) %>% summarise(count = n())
  } else {
    skillcount <- x %>% group_by(bgtjobid) %>% summarise(count = n())
  }
  skill2 <- filter(skillcount, count > 1)
  #print(skill2)
  id <- unique(skill2$bgtjobid)
  for(i in 1:nrow(skill2)){
    skill_i <- x %>% filter(bgtjobid == id[i])
    skillcomb_i <- combinations(n = nrow(skill_i), r = 2, v = skill_i$skill)
    skillcomb <- rbind(skillcomb, skillcomb_i)
    #print(skillcomb_i)
  }
  skillcomb
}

cluster_comb2 <- function(x, name = NA){
  clustercomb <- matrix(ncol = 2, nrow = 0)
  colnames(clustercomb) <- c("cluster1", "cluster2")

  if(is.na(name) == FALSE){
    clustercount <- x %>% filter(onetname == name) %>% group_by(bgtjobid, skillcluster) %>% summarise(count = n()) %>% group_by(bgtjobid) %>% summarise(ad_count = sum(count), cluster_count = n())
  } else {
    clustercount <- x %>% group_by(bgtjobid, skillcluster) %>% summarise(count = n()) %>% group_by(bgtjobid) %>% summarise(ad_count = sum(count), cluster_count = n())
  }
  cluster2 <- filter(clustercount, cluster_count > 1)
  #print(skill2)
  id <- unique(cluster2$bgtjobid)
  for(i in 1:nrow(cluster2)){
    cluster_i <- x %>% filter(bgtjobid == id[i]) %>% group_by(skillcluster) %>% summarise(count = n())
    clustercomb_i <- combinations(n = nrow(cluster_i), r = 2, v = cluster_i$skillcluster)
    clustercomb <- rbind(clustercomb, clustercomb_i)
    #print(skillcomb_i)
  }
  clustercomb
}

cluster_comb3 <- function(x, name = NA){
  clustercomb <- matrix(ncol = 3, nrow = 0)
  colnames(clustercomb) <- c("cluster1", "cluster2", "cluster3")

  if(is.na(name) == FALSE){
    clustercount <- x %>% filter(onetname == name) %>% group_by(bgtjobid, skillcluster) %>% summarise(count = n()) %>% group_by(bgtjobid) %>% summarise(ad_count = sum(count), cluster_count = n())
  } else {
    clustercount <- x %>% group_by(bgtjobid, skillcluster) %>% summarise(count = n()) %>% group_by(bgtjobid) %>% summarise(ad_count = sum(count), cluster_count = n())
  }
  cluster2 <- filter(clustercount, cluster_count > 2)
  #print(skill2)
  id <- unique(cluster2$bgtjobid)
  for(i in 1:nrow(cluster2)){
    cluster_i <- x %>% filter(bgtjobid == id[i]) %>% group_by(skillcluster) %>% summarise(count = n())
    clustercomb_i <- combinations(n = nrow(cluster_i), r = 3, v = cluster_i$skillcluster)
    clustercomb <- rbind(clustercomb, clustercomb_i)
    #print(skillcomb_i)
  }
  clustercomb
}

nurse <- skills %>% filter(onetname == "Critical Care Nurses")
test_jobid <- unique(ad_summary[ad_summary$onetname == "Critical Care Nurses", "bgtjobid"])$bgtjobid[1:4]

nursetest <- nurse[nurse$bgtjobid %in% test_jobid,]

nurse_comb <- job_comb2(nurse)
nurse_cluster_comb <- cluster_comb2(nurse)
nurse_cluster_comb3 <- cluster_comb3(nurse)

nurse_comb_sum <- as.tibble(nurse_comb) %>% group_by(skill1, skill2) %>% summarise(count = n()) %>% arrange(desc(count))
nurse_cluster_comb_sum <- as.tibble(nurse_cluster_comb) %>% group_by(cluster1, cluster2) %>% summarise(count = n()) %>% arrange(desc(count))
nurse_cluster_comb3_sum <- as.tibble(nurse_cluster_comb3) %>% group_by(cluster1, cluster2, cluster3) %>% summarise(count = n()) %>% arrange(desc(count))

computer_skill <- skills %>% filter(onetname == "Computer User Support Specialists") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n(), perc = round((n()/1154)*100, 2)) %>% arrange(desc(count))

computer_cluster <- skills %>% filter(onetname == "Computer User Support Specialists") %>% group_by(skillclusterfamily, skillcluster, bgtjobid) %>%
  summarise(skill_count = n(), skill_perc = round((n()/1154)*100, 2)) %>% group_by(skillclusterfamily, skillcluster) %>%
  summarise(skill_count = sum(skill_count), ad_count = n(), ad_perc = round((n()/1154)*100, 2)) %>% arrange(desc(ad_count))
computer_comb <- job_comb2(skills, name = "Computer User Support Specialists")
computer_comb_sum <- as.tibble(computer_comb) %>% group_by(skill1, skill2) %>% summarise(count = n()) %>% arrange(desc(count))

computer_cluster_comb <- cluster_comb2(skills, name = "Computer User Support Specialists")
computer_cluster_comb_sum <- as.tibble(computer_cluster_comb) %>% group_by(cluster1, cluster2) %>% summarise(count = n()) %>% arrange(desc(count))
computer_cluster_comb3 <- cluster_comb3(skills, name = "Computer User Support Specialists")
computer_cluster_comb3_sum <- as.tibble(computer_cluster_comb3) %>% group_by(cluster1, cluster2, cluster3) %>% summarise(count = n()) %>% arrange(desc(count))

maint_skill <- skills %>% filter(onetname == "Maintenance and Repair Workers, General") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n(), perc = round((n()/970)*100, 2)) %>% arrange(desc(count))

maint_cluster <- skills %>% filter(onetname == "Maintenance and Repair Workers, General") %>% group_by(skillclusterfamily, skillcluster, bgtjobid) %>%
  summarise(skill_count = n(), skill_perc = round((n()/1154)*100, 2)) %>% group_by(skillclusterfamily, skillcluster) %>%
  summarise(skill_count = sum(skill_count), ad_count = n(), ad_perc = round((n()/1154)*100, 2)) %>% arrange(desc(ad_count))


maintenance_comb <- job_comb2(skills, name = "Maintenance and Repair Workers, General")
maintenance_comb_sum <- as.tibble(maintenance_comb) %>% group_by(skill1, skill2) %>% summarise(count = n()) %>% arrange(desc(count))


maint_cluster_comb <- cluster_comb2(skills, name = "Maintenance and Repair Workers, General")
maint_cluster_comb_sum <- as.tibble(maint_cluster_comb) %>% group_by(cluster1, cluster2) %>% summarise(count = n()) %>% arrange(desc(count))
maint_cluster_comb3 <- cluster_comb3(skills, name = "Maintenance and Repair Workers, General")
maint_cluster_comb3_sum <- as.tibble(maint_cluster_comb3) %>% group_by(cluster1, cluster2, cluster3) %>% summarise(count = n()) %>% arrange(desc(count))



###how many combinations does it take to describe 90% of nurse ads

cluster_cast <- dcast(nurse, formula = bgtjobid ~ skillcluster)





#how many *other* skills does a particular skill show up with?

skilltogether <- function(x){
  #nrow(unique(rbind(nurse_comb_sum[nurse_comb_sum$skill1 == x,"skill2"], nurse_comb_sum[nurse_comb_sum$skill2 == x,"skill1"])))
  sum(nurse_comb_sum$skill1 == x) + sum(nurse_comb_sum$skill2 == x)
}

skilltogether_poss <- function(x){
  if(nurse_skill[nurse_skill$skill == x, "count"] >= 250){
    denom <- 250
  } else {
    denom <- nurse_skill[nurse_skill$skill == x, "count"]
  }
  together <- nurse_skill[nurse_skill$skill == x, "together_with"]
  (together/denom)[1,1]
}

nurse_skill$together_with <- sapply(nurse_skill$skill, skilltogether)
nurse_skill$together_with_poss  <- sapply(nurse_skill$skill, skilltogether_poss)

n1 <- nurse_comb_sum[nurse_comb_sum$skill1 == "Medical Triage","skill2"]
n2 <- nurse_comb_sum[nurse_comb_sum$skill2 == "Medical Triage","skill1"]

c(n1, n2)

View(nurse %>% group_by(skillcluster) %>% summarise(count = n()) %>% arrange(desc(count)))
View(nurse %>% group_by(skillclusterfamily) %>% summarise(count = n()) %>% arrange(desc(count)))
View(nurse %>% filter(skillcluster == "Pediatrics") %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count)))
View(nurse %>% filter(skillcluster == "Cardiology") %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count)))
View(nurse %>% filter(skillcluster == "Emergency and Intensive Care") %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count)))


#####thiiiiis doesn't work
cluster_comb <- function(x, name = NA){
  clustercomb <- matrix(ncol = 2, nrow = 0)
  colnames(clustercomb) <- c("cluster1", "cluster2")

  if(is.na(name) == FALSE){
    clustercount <- x %>% filter(onetname == name) %>% group_by(bgtjobid, skillcluster) %>% summarise(count = n())
  } else {
    clustercount <- x %>% group_by(bgtjobid, skillcluster) %>% summarise(count = n())
  }
  cluster2 <- filter(clustercount, count > 1)
  id <- unique(cluster2$bgtjobid)
  for(i in 1:nrow(cluster2)){
    cluster_i <- x %>% filter(bgtjobid == id[i])
    #print(cluster_i)
    clustercomb_i <- combinations(n = nrow(cluster_i), r = 2, v = cluster_i$skillcluster)
    clustercomb <- rbind(clustercomb, clustercomb_i)
    #print(skillcomb_i)
  }
  clustercomb
}

nurse_cc <- cluster_comb(nurse)



computer_skill <- skills %>% filter(onetname == "Computer User Support Specialists") %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n(), perc = round((n()/1154)*100, 2)) %>% arrange(desc(count))

computer_cluster <- skills %>% filter(onetname == "Computer User Support Specialists") %>% group_by(skillclusterfamily, skillcluster) %>%
  summarise(count = n(), perc = round((n()/1180)*100, 2)) %>% arrange(desc(count))
