library(dplyr)
library(ggplot2)

#Read in top5 data
b_tot <- read.csv("data/stem_edu/working/skills_by_occupation/top5_rb_separate/b_top5-stw-jobskill.csv")
r_tot <- read.csv("data/stem_edu/working/skills_by_occupation/top5_rb_separate/r_top5-stw-jobskill.csv")

#Onetnames of top 5 for blacksburg and richmond
b_onet <- unique(b_tot$onet)
r_onet <- unique(r_tot$onet)

b_onetnames <- unique(b_tot$onetname)
r_onetnames <- unique(r_tot$onetname)

###BLACKSBURG
# number of unique skills, clusters, families associated with each job

count_b <- b_tot %>%
  select(bgtjobid, skill, skillcluster, skillclusterfamily) %>%
  group_by(bgtjobid) %>%
  summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))



#number of skills, cluster, family frequency

freq_num_s_b <- count_b %>% group_by(num_s) %>% summarise(count = n())
  freq_num_s_b$total = length(unique(b_tot$bgtjobid))
  freq_num_s_b$per = ((freq_num_s_b$count)/(freq_num_s_b$total))*100
  freq_num_s_b$variable = "Blacksburg"

freq_num_sc_b <- count_b %>% group_by(num_sc) %>% summarise(count = n())
  freq_num_sc_b$total = length(unique(b_tot$bgtjobid))
  freq_num_sc_b$per = ((freq_num_sc_b$count)/(freq_num_sc_b$total))*100
  freq_num_sc_b$variable = "Blacksburg"

freq_num_scf_b <- count_b %>% group_by(num_scf) %>% summarise(count = n())
  freq_num_scf_b$total = length(unique(b_tot$bgtjobid))
  freq_num_scf_b$per = ((freq_num_scf_b$count)/(freq_num_scf_b$total))*100
  freq_num_scf_b$variable = "Blacksburg"

###RICHMOND
# number of unique skills, clusters, families associated with each job
count_r <- r_tot %>%
  select(bgtjobid, skill, skillcluster, skillclusterfamily) %>%
  group_by(bgtjobid) %>%
  summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))

#number of skills, cluster, family frequency
freq_num_s_r <- count_r %>% group_by(num_s) %>% summarise(count = n())
  freq_num_s_r$total = length(unique(r_tot$bgtjobid))
  freq_num_s_r$per = ((freq_num_s_r$count)/(freq_num_s_r$total))*100
  freq_num_s_r$variable = "Richmond"

freq_num_sc_r <- count_r %>% group_by(num_sc) %>% summarise(count = n())
  freq_num_sc_r$total = length(unique(r_tot$bgtjobid))
  freq_num_sc_r$per = ((freq_num_sc_r$count)/(freq_num_sc_r$total))*100
  freq_num_sc_r$variable = "Richmond"

freq_num_scf_r <- count_r %>% group_by(num_scf) %>% summarise(count = n())
  freq_num_scf_r$total = length(unique(r_tot$bgtjobid))
  freq_num_scf_r$per = ((freq_num_scf_r$count)/(freq_num_scf_r$total))*100
  freq_num_scf_r$variable = "Richmond"

#COMPARE FREQUENCIES FOR TOTAL BLACKSBURG AND RICHMOND
freq_num_s_br <- rbind(freq_num_s_b, freq_num_s_r)
  ggplot(freq_num_s_br, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity")
freq_num_sc_br <- rbind(freq_num_sc_b, freq_num_sc_r)
  ggplot(freq_num_sc_br, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity")
freq_num_scf_br <- rbind(freq_num_scf_b, freq_num_scf_r)
  ggplot(freq_num_scf_br, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity")

#ensuring the nurse onet codes are the same except for machinists and web developers
b_onet<- select(b_tot, c("onet", "onetname"))
r_onet<- select(r_tot, c("onet", "onetname"))
  unique(b_tot$onet[!(b_tot$onet %in% r_tot$onet)])
  unique(r_tot$onet[!(r_tot$onet %in% b_tot$onet)])

#Blacksburg by ONETname
b_maint <- filter(b_tot, onet == "49-9071.00")
  count_b_maint <- b_maint %>%
    select(bgtjobid, skill, skillcluster, skillclusterfamily) %>%
    group_by(bgtjobid) %>%
    summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))
b_nurs <- filter(b_tot, onet == "29-1141.03")
  count_b_nurs <- b_nurs %>%
    select(bgtjobid, skill, skillcluster, skillclusterfamily) %>%
    group_by(bgtjobid) %>%
    summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))
b_comp <- filter(b_tot, onet=="15-1151.00")
  count_b_comp <- b_comp %>%
    select(bgtjobid, skill, skillcluster, skillclusterfamily) %>%
    group_by(bgtjobid) %>%
    summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))
b_mach <- filter(b_tot, onet =="51-4041.00")
  count_b_mach <- b_mach %>%
    select(bgtjobid, skill, skillcluster, skillclusterfamily) %>%
    group_by(bgtjobid) %>%
    summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))
b_auto <- filter(b_tot, onet == "49-3023.02")
  count_b_auto <- b_auto %>%
    select(bgtjobid, skill, skillcluster, skillclusterfamily) %>%
    group_by(bgtjobid) %>%
    summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))

#Richmond by ONETname
r_maint <- filter(r_tot, onet == "49-9071.00")
  count_r_maint <- r_maint %>%
    select(bgtjobid, skill, skillcluster, skillclusterfamily) %>%
    group_by(bgtjobid) %>%
    summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))
r_auto <- filter(r_tot, onet == "49-3023.02")
  count_r_auto <- r_auto %>%
    select(bgtjobid, skill, skillcluster, skillclusterfamily) %>%
    group_by(bgtjobid) %>%
    summarize(num_s =n_distinct(skill), num_sc = n_distinct(skillcluster), num_scf=n_distinct(skillclusterfamily))
r_comp <- filter(r_tot, onet=="15-1151.00")
r_nurs <- filter(r_tot, onet == "29-1141.03")
r_web <- filter(r_tot, onet == "15-1134.00")

