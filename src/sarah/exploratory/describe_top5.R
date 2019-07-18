library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(data.table)

#Read in top5 data
b_tot <- read.csv("data/stem_edu/working/burning_glass_ad_combine_16_17/blacksburg_top5_stw_jobs_all_skills.csv")
b_tot$variable = "Blacksburg"
r_tot <- read.csv("data/stem_edu/working/burning_glass_ad_combine_16_17/richmond_top5_stw_jobs_all_skills.csv")
  r_tot$variable = "Richmond"

#BLACKSBURG Overall####

# number of unique skills, clusters, families associated with each job
count_b <- b_tot %>%
  select(bgtjobid, skill, skillcluster, skillclusterfamily, onet, onetname, variable) %>%
  group_by(bgtjobid, onet, onetname, variable) %>%
  summarize(num_s=n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))

#number of skills, cluster, family frequency
freq_s_b <- count_b %>% group_by(num_s, variable) %>% summarise(count = n(), per =(count/(length(unique(b_tot$bgtjobid))))*100)

freq_sc_b <- count_b %>% group_by(num_sc, variable) %>% summarise(count = n(), per=(count/(length(unique(b_tot$bgtjobid))))*100)

freq_scf_b <- count_b %>% group_by(num_scf, variable) %>% summarise(count = n(), per=((count)/(length(unique(b_tot$bgtjobid))))*100)


#RICHMOND Overall######

# number of unique skills, clusters, families associated with each job
count_r <- r_tot %>%
  select(bgtjobid, skill, skillcluster, skillclusterfamily, onet, onetname, variable) %>%
  group_by(bgtjobid, onet, onetname, variable) %>%
  summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))

#number of skills, cluster, family frequency
freq_s_r <- count_r %>% group_by(num_s, variable) %>% summarise(count = n(), per =((count)/(length(unique(r_tot$bgtjobid))))*100)

freq_sc_r <- count_r %>% group_by(num_sc, variable) %>% summarise(count = n(), per=((count)/(length(unique(r_tot$bgtjobid))))*100)

freq_scf_r <- count_r %>% group_by(num_scf, variable) %>% summarise(count = n(), per=((count)/(length(unique(r_tot$bgtjobid))))*100)

#COMPARE FREQUENCIES FOR Overall BLACKSBURG AND RICHMOND####

freq_s_br <- rbind(freq_s_b, freq_s_r)
  ggplot(freq_s_br, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nTop Five STW Occupations in Richmond and Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skills")

freq_sc_br <- rbind(freq_sc_b, freq_sc_r)
  ggplot(freq_sc_br, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity")+
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nTop Five STW Occupations in Richmond and Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters")

freq_scf_br <- rbind(freq_scf_b, freq_scf_r)
  ggplot(freq_scf_br, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity")+
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nTop Five STW Occupations in Richmond and Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families")

#ensuring the nurse onet codes are the same except for machinists and web developers
b_onet<- select(b_tot, c("onet", "onetname"))
r_onet<- select(r_tot, c("onet", "onetname"))
  unique(b_tot$onet[!(b_tot$onet %in% r_tot$onet)])
  unique(r_tot$onet[!(r_tot$onet %in% b_tot$onet)])


#Blacksburg by ONETname####
count_b_maint <- filter(count_b, onet=="49-9071.00")
count_b_maint$variable = "Blacksburg Maintenance"
  freq_s_b_maint <- count_b_maint %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_maint$bgtjobid))))*100)
  freq_sc_b_maint <- count_b_maint %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_maint$bgtjobid))))*100)
  freq_scf_b_maint <- count_b_maint %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_maint$bgtjobid))))*100)

count_b_nurs <- filter(count_b, onet == "29-1141.03")
count_b_nurs$variable = "Blacksburg Nurses"
  freq_s_b_nurs <- count_b_nurs %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_nurs$bgtjobid))))*100)
  freq_sc_b_nurs <- count_b_nurs %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_nurs$bgtjobid))))*100)
  freq_scf_b_nurs <- count_b_nurs %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_nurs$bgtjobid))))*100)

count_b_comp <- filter(count_b, onet=="15-1151.00")
count_b_comp$variable = "Blacksburg Computer"
  freq_s_b_comp <- count_b_comp %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_comp$bgtjobid))))*100)
  freq_sc_b_comp <- count_b_comp %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_comp$bgtjobid))))*100)
  freq_scf_b_comp <- count_b_comp %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_comp$bgtjobid))))*100)

count_b_mach <- filter(count_b, onet =="51-4041.00")
count_b_mach$variable = "Blacksburg Machinists"
  freq_s_b_mach <- count_b_mach %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_mach$bgtjobid))))*100)
  freq_sc_b_mach <- count_b_mach %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_mach$bgtjobid))))*100)
  freq_scf_b_mach <- count_b_mach %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_mach$bgtjobid))))*100)

count_b_auto <- filter(count_b, onet == "49-3023.02")
count_b_auto$variable = "Blacksburg Auto"
  freq_s_b_auto <- count_b_auto %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_auto$bgtjobid))))*100)
  freq_sc_b_auto <- count_b_auto %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_auto$bgtjobid))))*100)
  freq_scf_b_auto <- count_b_auto %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_auto$bgtjobid))))*100)


#CHART TOP 5 OCCUPATIONS OF BLACKSBURG BY SKILL, CLUSTER, FAMILIES####
freq_s_b_onet <- rbind(freq_s_b_auto, freq_s_b_comp, freq_s_b_mach, freq_s_b_maint, freq_s_b_nurs)
  ggplot(freq_s_b_onet, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nTop Five STW Occupations in   Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skills")

freq_sc_b_onet <- rbind(freq_sc_b_auto, freq_sc_b_comp, freq_sc_b_mach, freq_sc_b_maint, freq_sc_b_nurs)
  ggplot(freq_sc_b_onet, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nTop Five STW Occupations in Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters")

freq_scf_b_onet <- rbind(freq_scf_b_auto, freq_scf_b_comp, freq_scf_b_mach, freq_scf_b_maint, freq_scf_b_nurs)
  ggplot(freq_scf_b_onet, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nTop Five STW Occupations in Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families")

#Richmond by ONETname####
count_r_maint <- filter(count_r, onet=="49-9071.00")
count_r_maint$variable = "Richmond Maintenance"
  freq_s_r_maint <- count_r_maint %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_maint$bgtjobid))))*100)
  freq_sc_r_maint <- count_r_maint %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_maint$bgtjobid))))*100)
  freq_scf_r_maint <- count_r_maint %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_maint$bgtjobid))))*100)

count_r_nurs <- filter(count_r, onet == "29-1141.03")
count_r_nurs$variable = "Richmond Nurses"
  freq_s_r_nurs <- count_r_nurs %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_nurs$bgtjobid))))*100)
  freq_sc_r_nurs <- count_r_nurs %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_nurs$bgtjobid))))*100)
  freq_scf_r_nurs <- count_r_nurs %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_nurs$bgtjobid))))*100)

count_r_comp <- filter(count_r, onet=="15-1151.00")
count_r_comp$variable = "Richmond Computer"
  freq_s_r_comp <- count_r_comp %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_comp$bgtjobid))))*100)
  freq_sc_r_comp <- count_r_comp %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_comp$bgtjobid))))*100)
  freq_scf_r_comp <- count_r_comp %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_comp$bgtjobid))))*100)

count_r_auto <- filter(count_r, onet == "49-3023.02")
count_r_auto$variable = "Richmond Auto"
  freq_s_r_auto <- count_r_auto %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_auto$bgtjobid))))*100)
  freq_sc_r_auto <- count_r_auto %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_auto$bgtjobid))))*100)
  freq_scf_r_auto <- count_r_auto %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_auto$bgtjobid))))*100)

count_r_web <- filter(count_r, onet =="15-1134.00")
count_r_web$variable = "Richmond Web"
  freq_s_r_web <- count_r_web %>% group_by(num_s, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_web$bgtjobid))))*100)
  freq_sc_r_web <- count_r_web %>% group_by(num_sc, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_web$bgtjobid))))*100)
  freq_scf_r_web <- count_r_web %>% group_by(num_scf, variable) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_web$bgtjobid))))*100)

#CHART TOP 5 OCCUPATIONS OF RICHMOND BY SKILL, CLUSTER, FAMILIES####
freq_s_r_onet <- rbind(freq_s_r_auto, freq_s_r_comp, freq_s_r_web, freq_s_r_maint, freq_s_r_nurs)
  ggplot(freq_s_r_onet, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nTop Five STW Occupations in Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skills")

freq_sc_r_onet <- rbind(freq_sc_r_auto, freq_sc_r_comp, freq_sc_r_web, freq_sc_r_maint, freq_sc_r_nurs)
  ggplot(freq_sc_r_onet, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nTop Five STW Occupations in Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters")

freq_scf_r_onet <- rbind(freq_scf_r_auto, freq_scf_r_comp, freq_scf_r_web, freq_scf_r_maint, freq_scf_r_nurs)
  ggplot(freq_scf_r_onet, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nTop Five STW Occupations in Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families")

#Compare individual onets Blacksburg and Richmond####
#COMPARE NURSES
freq_s_nurs <- rbind(freq_s_b_nurs, freq_s_r_nurs)
  ggplot(freq_s_nurs, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nCritical Care Nurses in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skills")
freq_sc_nurs <- rbind(freq_sc_b_nurs, freq_sc_r_nurs)
  ggplot(freq_sc_nurs, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nCritical Care Nurses in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters")
freq_scf_nurs <- rbind(freq_scf_b_nurs, freq_scf_r_nurs)
  ggplot(freq_scf_nurs, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nCritical Care Nurses in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families")

#COMPARE MAINTENANCE
freq_s_maint <- rbind(freq_s_b_maint, freq_s_r_maint)
  ggplot(freq_s_maint, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nMaintenance Workers in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skills")
freq_sc_maint <- rbind(freq_sc_b_maint, freq_sc_r_maint)
  ggplot(freq_sc_maint, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nMaintenance Workers in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters")
freq_scf_maint <- rbind(freq_scf_b_maint, freq_scf_r_maint)
  ggplot(freq_scf_maint, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nMaintenance Workers in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families")

#COMPARE COMPUTER
freq_s_comp <- rbind(freq_s_b_comp, freq_s_r_comp)
  ggplot(freq_s_comp, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nComputer User Support Specialists in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skills")
freq_sc_comp <- rbind(freq_sc_b_comp, freq_sc_r_comp)
  ggplot(freq_sc_comp, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nComputer User Support Specialists in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters")
freq_scf_comp <- rbind(freq_scf_b_comp, freq_scf_r_comp)
  ggplot(freq_scf_comp, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nComputer User Support Specialists in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families")

#COMPARE AUTO
freq_s_auto <- rbind(freq_s_b_auto, freq_s_r_auto)
  ggplot(freq_s_auto, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nAutomotive Specialty Technicians in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skills")
freq_sc_auto <- rbind(freq_sc_b_auto, freq_sc_r_auto)
  ggplot(freq_sc_auto, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nAutomotive Specialty Technicians in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters")
freq_scf_auto <- rbind(freq_scf_b_auto, freq_scf_r_auto)
  ggplot(freq_scf_auto, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nAutomotive Specialty Technicians in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families")


#ADDITIONAL METHOD####
r_fam_onet <- r_tot %>% select(skillclusterfamily, onetname)
  r_fam_onet$count= 1
  r_fam_onet <-  aggregate(r_fam_onet$count, by= list(r_fam_onet$skillclusterfamily, r_fam_onet$onetname), FUN= sum)

ggballoonplot(r_fam_onet, fill = "value")+
    scale_fill_viridis_c(option = "C")+
    ggtitle("Frequency of Skill Cluster Families by Occupation in Richmond")

b_fam_onet <- b_tot %>% select(skillclusterfamily, onetname)
  b_fam_onet$count= 1
  b_fam_onet <-  aggregate(b_fam_onet$count, by= list(b_fam_onet$skillclusterfamily, b_fam_onet$onetname), FUN= sum)

ggballoonplot(b_fam_onet, fill = "value")+
  scale_fill_viridis_c(option = "C")+
  ggtitle("Frequency of Skill Cluster Families by Occupation in Blacksburg")

#SPECIALIZED####
b_special <-filter(b_tot, b_tot$isspecialized ==1)
  count_b_special <- b_special %>%
    select(bgtjobid, skill, skillcluster, skillclusterfamily, onet) %>%
    group_by(bgtjobid, onet) %>%
    summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))

freq_s_b_special <- count_b_special %>% group_by(num_s) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_special$bgtjobid))))*100)
freq_s_b_special$variable = "b_special"

freq_scf_b_special <- count_b_special %>% group_by(num_scf) %>% summarise(count = n(), per = ((count)/(length(unique(count_b_special$bgtjobid))))*100)
freq_scf_b_special$variable = "b_special"

r_special <-filter(r_tot, r_tot$isspecialized ==1)
count_r_special <- r_special %>%
  select(bgtjobid, skill, skillcluster, skillclusterfamily, onet) %>%
  group_by(bgtjobid, onet) %>%
  summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))

freq_s_r_special <- count_r_special %>% group_by(num_s) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_special$bgtjobid))))*100)
freq_s_r_special$variable = "r_special"

freq_scf_r_special <- count_r_special %>% group_by(num_scf) %>% summarise(count = n(), per = ((count)/(length(unique(count_r_special$bgtjobid))))*100)
freq_scf_r_special$variable = "r_special"

freq_s_special <- rbind(freq_s_r_special, freq_s_b_special)
  ggplot(freq_s_special, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nSpecialized Jobs in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skills")

freq_scf_special <- rbind(freq_scf_r_special, freq_scf_b_special)
  ggplot(freq_scf_special, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nSpecialized Jobs in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families")


b_one_skill <- count_b %>%
  filter(num_scf==1 & num_s ==1)
  b_one_skill <- merge(b_one_skill, b_tot, by = "bgtjobid")
  b_one_skill %>% group_by(onetname)%>% summarise(count = n())
  b_one_skill$count = 1
  b_one_skill <- b_one_skill[c(9, 17,19)]
  b_one_skill <- aggregate(b_one_skill$count, by= list(b_one_skill$skill, b_one_skill$onetname), FUN= sum)
  ggballoonplot(b_one_skill, fill = "value")+
    scale_fill_viridis_c(option = "C")+
    ggtitle("Count of One Skill Jobs, by Skill and Onet Name in Blacksburg")

r_one_skill <- count_r %>%
  filter(num_scf==1 & num_s==1)
  r_one_skill <- merge(r_one_skill, r_tot, by = "bgtjobid")
  r_one_skill %>% group_by(onetname)%>% summarise(count= n())
  r_one_skill$count = 1
  r_one_skill <- r_one_skill[c(9, 17,19)]
  r_one_skill <- aggregate(r_one_skill$count, by= list(r_one_skill$skill, r_one_skill$onetname), FUN= sum)
  ggballoonplot(r_one_skill, fill = "value")+
    scale_fill_viridis_c(option = "C")+
    ggtitle("Count of One Skill Jobs, by Skill and Onet Name in Richmond")
##############3

test <- b_tot %>% filter(skillclusterfamily=="Information Technology") %>% select(skill, onetname)
test$count= 1
  test <-  aggregate(test$count, by= list(test$skill, test$onetname), FUN= sum)

  ggballoonplot(test, fill = "value")+
    scale_fill_viridis_c(option = "C")+
    ggtitle("")

#BOXPLOTS############

box_b <-  b_tot %>%
    select(bgtjobid, skill, skillcluster, skillclusterfamily, onet, onetname, variable) %>%
    group_by(bgtjobid, onet, onetname, variable) %>%
    summarize(skill=n_distinct(skill), skill_cluster = n_distinct(skillcluster), skill_cluster_family=n_distinct(skillclusterfamily))
box_b<- gather(box_b, "skill_cat", "count", 5:7)

box_r <-  r_tot %>%
  select(bgtjobid, skill, skillcluster, skillclusterfamily, onet, onetname, variable) %>%
  group_by(bgtjobid, onet, onetname, variable) %>%
  summarize(skill=n_distinct(skill), skill_cluster = n_distinct(skillcluster), skill_cluster_family=n_distinct(skillclusterfamily))
box_r<- gather(box_r, "skill_cat", "count", 5:7)

box_t <- rbind(box_b, box_r)

#Blacksburg and Richmond Comparison
ggplot(box_t, aes(x=skill_cat,y=count, fill= variable, color=variable, alpha= .1)) +
  geom_boxplot(outlier.shape=NA, color="black", alpha=0)+
  geom_point(position = position_jitterdodge())+
  ggtitle("Count of Skills, Skill Clusters, and Skill Cluster Families Requested in Blacksburg and Richmond Job Ads")

ggplot(box_r, aes(x=skill_cat,y=count, fill= variable, color=onetname, alpha= .1)) +
  geom_boxplot(outlier.shape=NA, color="black", alpha=0)+
  geom_point(position = position_jitterdodge())+
  ggtitle("Count of Skills Requested in Richmond Job Ads")


#Top Five Blacksburg
ggplot(box_b, aes(x=skill_cat, y=count, fill=onetname, color=onetname, alpha =.1))+
  geom_boxplot(outlier.shape=NA, color="black", alpha = 0)+
  geom_point(position = position_jitterdodge(jitter.height= .4)) +
  ggtitle("Count of Skills, Skill Clusters, and Skill Cluster Families Requested in Job Ads for Top Five Occupations in Blacksburg")

#Top Five Richmond
ggplot(box_r, aes(x=skill_cat, y=count, fill=onetname, color=onetname, alpha =.1))+
  geom_boxplot(outlier.shape=NA, color="black", alpha = 0)+
  geom_point(position = position_jitterdodge(jitter.height= .4)) +
  ggtitle("Count of Skills, Skill Clusters, and Skill Cluster Families Requested in Job Ads for Top Five Occupations in Richmond")

#Maintenance Comparison
box_maint <- box_t %>% filter(onet == "49-9071.00")

ggplot(box_maint, aes(x=skill_cat, y=count, fill=variable, color=variable, alpha =.1))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0)+
  geom_point(position = position_jitterdodge(jitter.height= .4)) +
  ggtitle("Count of Skills, Skill Clusters, and Skill Cluster Families \nRequested in Job Ads for Maintenance and Repair Workers \nin Blacksburg and Richmond")

#Nurse Comparison
box_nurs <-box_t %>% filter(onet =="29-1141.03")

ggplot(box_nurs, aes(x=skill_cat, y=count, fill=variable, color=variable, alpha =.1))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0)+
  geom_point(position = position_jitterdodge(jitter.height= .4)) +
  ggtitle("Count of Skills, Skill Clusters, and Skill Cluster Families \nRequested in Job Ads for Critical Care Nurses \nin Blacksburg and Richmond")

#Auto Comparison
box_auto <- box_t %>% filter(onet =="49-3023.02")

ggplot(box_auto, aes(x=skill_cat, y=count, fill=variable, color=variable, alpha =.1))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0)+
  geom_point(position = position_jitterdodge(jitter.height= .4)) +
  ggtitle("Count of Skills, Skill Clusters, and Skill Cluster Families \nRequested in Job Ads for Automotive Specialty Technicians \nin Blacksburg and Richmond")

#Comp Comparison
box_comp <-box_t %>% filter(onet == "15-1151.00")

ggplot(box_comp, aes(x=skill_cat, y=count, fill=variable, color=variable, alpha =.1))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0)+
  geom_point(position = position_jitterdodge(jitter.height= .4)) +
  ggtitle("Count of Skills, Skill Clusters, and Skill Cluster Families \nRequested in Job Ads for Computer User Support Specialists \nin Blacksburg and Richmond")

#Blacksburg Machinists
box_b_mach <- box_b %>% filter(onet == "51-4041.00")

ggplot(box_b_mach, aes(x=skill_cat, y=count, fill=variable, color=variable, alpha =.1))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0)+
  geom_point(position = position_jitterdodge(jitter.height= .4)) +
  ggtitle("Count of Skills, Skill Clusters, and Skill Cluster Families \nRequested in Job Ads for Machinists \nin Blacksburg")

#Richmond Web Developers Specialty Technicians
box_r_web <- box_r %>% filter(onet == "15-1134.00")

ggplot(box_r_web, aes(x=skill_cat, y=count, fill=variable, color=variable, alpha =.1))+
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0)+
  geom_point(position = position_jitterdodge(jitter.height= .4)) +
  ggtitle("Count of Skills, Skill Clusters, and Skill Cluster Families \nRequested in Job Ads for Web Developers \nin Richmond")



count_b2 <- b_tot %>%
  select(bgtjobid, skill, skillcluster, skillclusterfamily, onet, onetname, variable) %>%
  group_by(bgtjobid, onet, onetname, variable) %>%
  summarize(num_s=n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))



#Plot Counts against eachother######
dat <- data.frame(skills = count_b2$num_s, families = count_b2$num_scf, job = count_b2$bgtjobid)
ggplot(data = dat, aes(x = skills, y = families))+
  geom_point()+
  geom_jitter() +
  ggtitle("The Relationship Between Number of Skills and Number of Skill Cluster Families in Blacksburg (Jittered)")

dat2 <- data.frame(skills = avg$num_s, clusters = avg$num_sc, job = avg$bgtjobid)
ggplot(data = dat2, aes(x = skills, y = clusters))+
  geom_point()+
  geom_jitter()+
  ggtitle("The Relationship Between Number of Skills and Number of Skill Clusters in Blacksburg (Jittered)")

dat3 <- data.frame(clusters = avg$num_sc, families = avg$num_scf, job = avg$bgtjobid)
ggplot(data = dat3, aes(x = clusters, y = families))+
  geom_point()+
  geom_jitter()+
  ggtitle("The Relationship Between Number of Skill Clusters and Number of Skill Clusters Families in Blacksburg (Jittered)")


#RICHMOND
dat4 <- data.frame(skills = count_r$num_s, families = count_r$num_scf, job = count_r$bgtjobid)
ggplot(data = dat4, aes(x = skills, y = families))+
  geom_point()+
  geom_jitter() +
  ggtitle("The Relationship Between Number of Skills and Number of Skill Cluster Families in Richmond (Jittered)")

dat5 <- data.frame(skills = count_r$num_s, clusters = count_r$num_sc, job = count_r$bgtjobid)
ggplot(data = dat5, aes(x = skills, y = clusters))+
  geom_point()+
  geom_jitter()+
  ggtitle("The Relationship Between Number of Skills and Number of Skill Clusters in Richmond (Jittered)")

dat6 <- data.frame(clusters = count_r$num_sc, families = count_r$num_scf, job = count_r$bgtjobid)
ggplot(data = dat6, aes(x = clusters, y = families))+
  geom_point()+
  geom_jitter()+
  ggtitle("The Relationship Between Number of Skill Clusters and Number of Skill Clusters Families in Richmond (Jittered)")
