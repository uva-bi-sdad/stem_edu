library(dplyr)
library(ggplot2)

#Read in top5 data
b_tot <- read.csv("data/stem_edu/working/skills_by_occupation/top5_rb_separate/b_top5-stw-jobskill.csv")
r_tot <- read.csv("data/stem_edu/working/skills_by_occupation/top5_rb_separate/r_top5-stw-jobskill.csv")

#Onetnames of top 5 for blacksburg and richmond
b_onet <- unique(b_tot$onet)
r_onet <- unique(r_tot$onet)

###BLACKSBURG

# number of unique skills, clusters, families associated with each job
count_b <- b_tot %>%
  select(bgtjobid, skill, skillcluster, skillclusterfamily, onet) %>%
  group_by(bgtjobid, onet) %>%
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
  select(bgtjobid, skill, skillcluster, skillclusterfamily, onet) %>%
  group_by(bgtjobid, onet) %>%
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
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nTop Five STW Occupations in Richmond and Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skills Requested")

freq_num_sc_br <- rbind(freq_num_sc_b, freq_num_sc_r)
  ggplot(freq_num_sc_br, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity")+
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nTop Five STW Occupations in Richmond and Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters Requested")

freq_num_scf_br <- rbind(freq_num_scf_b, freq_num_scf_r)
  ggplot(freq_num_scf_br, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity")+
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nTop Five STW Occupations in Richmond and Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families Requested")

#ensuring the nurse onet codes are the same except for machinists and web developers
b_onet<- select(b_tot, c("onet", "onetname"))
r_onet<- select(r_tot, c("onet", "onetname"))
  unique(b_tot$onet[!(b_tot$onet %in% r_tot$onet)])
  unique(r_tot$onet[!(r_tot$onet %in% b_tot$onet)])

#Blacksburg by ONETname
count_b_maint <- filter(count_b, onet=="49-9071.00")
  freq_s_b_maint <- count_b_maint %>% group_by(num_s) %>% summarise(count = n())
    freq_s_b_maint$total = length(unique(count_b_maint$bgtjobid))
    freq_s_b_maint$per = ((freq_s_b_maint$count)/(freq_s_b_maint$total))*100
    freq_s_b_maint$variable = "b_maint"
  freq_sc_b_maint <- count_b_maint %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_b_maint$total = length(unique(count_b_maint$bgtjobid))
    freq_sc_b_maint$per = ((freq_sc_b_maint$count)/(freq_sc_b_maint$total))*100
    freq_sc_b_maint$variable = "b_maint"
  freq_scf_b_maint <- count_b_maint %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_b_maint$total = length(unique(count_b_maint$bgtjobid))
    freq_scf_b_maint$per = ((freq_scf_b_maint$count)/(freq_scf_b_maint$total))*100
    freq_scf_b_maint$variable = "b_maint"

count_b_nurs <- filter(count_b, onet == "29-1141.03")
  freq_s_b_nurs <- count_b_nurs %>% group_by(num_s) %>% summarise(count = n())
    freq_s_b_nurs$total = length(unique(count_b_nurs$bgtjobid))
    freq_s_b_nurs$per = ((freq_s_b_nurs$count)/(freq_s_b_nurs$total))*100
    freq_s_b_nurs$variable = "b_nurs"
  freq_sc_b_nurs <- count_b_nurs %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_b_nurs$total = length(unique(count_b_nurs$bgtjobid))
    freq_sc_b_nurs$per = ((freq_sc_b_nurs$count)/(freq_sc_b_nurs$total))*100
    freq_sc_b_nurs$variable = "b_nurs"
  freq_scf_b_nurs <- count_b_nurs %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_b_nurs$total = length(unique(count_b_nurs$bgtjobid))
    freq_scf_b_nurs$per = ((freq_scf_b_nurs$count)/(freq_scf_b_nurs$total))*100
    freq_scf_b_nurs$variable = "b_nurs"

count_b_comp <- filter(count_b, onet=="15-1151.00")
  freq_s_b_comp <- count_b_comp %>% group_by(num_s) %>% summarise(count = n())
    freq_s_b_comp$total = length(unique(count_b_comp$bgtjobid))
    freq_s_b_comp$per = ((freq_s_b_comp$count)/(freq_s_b_comp$total))*100
    freq_s_b_comp$variable = "b_comp"
  freq_sc_b_comp <- count_b_comp %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_b_comp$total = length(unique(count_b_comp$bgtjobid))
    freq_sc_b_comp$per = ((freq_sc_b_comp$count)/(freq_sc_b_comp$total))*100
    freq_sc_b_comp$variable = "b_comp"
  freq_scf_b_comp <- count_b_comp %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_b_comp$total = length(unique(count_b_comp$bgtjobid))
    freq_scf_b_comp$per = ((freq_scf_b_comp$count)/(freq_scf_b_comp$total))*100
    freq_scf_b_comp$variable = "b_comp"
count_b_mach <- filter(count_b, onet =="51-4041.00")
  freq_s_b_mach <- count_b_mach %>% group_by(num_s) %>% summarise(count = n())
    freq_s_b_mach$total = length(unique(count_b_mach$bgtjobid))
    freq_s_b_mach$per = ((freq_s_b_mach$count)/(freq_s_b_mach$total))*100
    freq_s_b_mach$variable = "b_mach"
  freq_sc_b_mach <- count_b_mach %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_b_mach$total = length(unique(count_b_mach$bgtjobid))
    freq_sc_b_mach$per = ((freq_sc_b_mach$count)/(freq_sc_b_mach$total))*100
    freq_sc_b_mach$variable = "b_mach"
  freq_scf_b_mach <- count_b_mach %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_b_mach$total = length(unique(count_b_mach$bgtjobid))
    freq_scf_b_mach$per = ((freq_scf_b_mach$count)/(freq_scf_b_mach$total))*100
    freq_scf_b_mach$variable = "b_mach"
count_b_auto <- filter(count_b, onet == "49-3023.02")
  freq_s_b_auto <- count_b_auto %>% group_by(num_s) %>% summarise(count = n())
    freq_s_b_auto$total = length(unique(count_b_auto$bgtjobid))
    freq_s_b_auto$per = ((freq_s_b_auto$count)/(freq_s_b_auto$total))*100
    freq_s_b_auto$variable = "b_auto"
  freq_sc_b_auto <- count_b_auto %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_b_auto$total = length(unique(count_b_auto$bgtjobid))
    freq_sc_b_auto$per = ((freq_sc_b_auto$count)/(freq_sc_b_auto$total))*100
    freq_sc_b_auto$variable = "b_auto"
  freq_scf_b_auto <- count_b_auto %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_b_auto$total = length(unique(count_b_auto$bgtjobid))
    freq_scf_b_auto$per = ((freq_scf_b_auto$count)/(freq_scf_b_auto$total))*100
    freq_scf_b_auto$variable = "b_auto"
#CHART TOP 5 OCCUPATIONS OF BLACKSBURG BY SKILL, CLUSTER, FAMILIES
freq_s_b_onet <- rbind(freq_s_b_auto, freq_s_b_comp, freq_s_b_mach, freq_s_b_maint, freq_s_b_nurs)
  ggplot(freq_s_b_onet, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nTop Five STW Occupations in   Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skills Requested")

freq_sc_b_onet <- rbind(freq_sc_b_auto, freq_sc_b_comp, freq_sc_b_mach, freq_sc_b_maint, freq_sc_b_nurs)
  ggplot(freq_sc_b_onet, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nTop Five STW Occupations in Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters Requested")

freq_scf_b_onet <- rbind(freq_scf_b_auto, freq_scf_b_comp, freq_scf_b_mach, freq_scf_b_maint, freq_scf_b_nurs)
  ggplot(freq_scf_b_onet, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nTop Five STW Occupations in Blacksburg") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families Requested")

#Richmond by ONETname
count_r_maint <- filter(count_r, onet=="49-9071.00")
  freq_s_r_maint <- count_r_maint %>% group_by(num_s) %>% summarise(count = n())
    freq_s_r_maint$total = length(unique(count_r_maint$bgtjobid))
    freq_s_r_maint$per = ((freq_s_r_maint$count)/(freq_s_r_maint$total))*100
    freq_s_r_maint$variable = "r_maint"
  freq_sc_r_maint <- count_r_maint %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_r_maint$total = length(unique(count_r_maint$bgtjobid))
    freq_sc_r_maint$per = ((freq_sc_r_maint$count)/(freq_sc_r_maint$total))*100
    freq_sc_r_maint$variable = "r_maint"
  freq_scf_r_maint <- count_r_maint %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_r_maint$total = length(unique(count_r_maint$bgtjobid))
    freq_scf_r_maint$per = ((freq_scf_r_maint$count)/(freq_scf_r_maint$total))*100
    freq_scf_r_maint$variable = "r_maint"
count_r_nurs <- filter(count_r, onet == "29-1141.03")
  freq_s_r_nurs <- count_r_nurs %>% group_by(num_s) %>% summarise(count = n())
    freq_s_r_nurs$total = length(unique(count_r_nurs$bgtjobid))
    freq_s_r_nurs$per = ((freq_s_r_nurs$count)/(freq_s_r_nurs$total))*100
    freq_s_r_nurs$variable = "r_nurs"
  freq_sc_r_nurs <- count_r_nurs %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_r_nurs$total = length(unique(count_r_nurs$bgtjobid))
    freq_sc_r_nurs$per = ((freq_sc_r_nurs$count)/(freq_sc_r_nurs$total))*100
    freq_sc_r_nurs$variable = "r_nurs"
  freq_scf_r_nurs <- count_r_nurs %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_r_nurs$total = length(unique(count_r_nurs$bgtjobid))
    freq_scf_r_nurs$per = ((freq_scf_r_nurs$count)/(freq_scf_r_nurs$total))*100
    freq_scf_r_nurs$variable = "r_nurs"
count_r_comp <- filter(count_r, onet=="15-1151.00")
  freq_s_r_comp <- count_r_comp %>% group_by(num_s) %>% summarise(count = n())
    freq_s_r_comp$total = length(unique(count_r_comp$bgtjobid))
    freq_s_r_comp$per = ((freq_s_r_comp$count)/(freq_s_r_comp$total))*100
    freq_s_r_comp$variable = "r_comp"
  freq_sc_r_comp <- count_r_comp %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_r_comp$total = length(unique(count_r_comp$bgtjobid))
    freq_sc_r_comp$per = ((freq_sc_r_comp$count)/(freq_sc_r_comp$total))*100
    freq_sc_r_comp$variable = "r_comp"
  freq_scf_r_comp <- count_r_comp %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_r_comp$total = length(unique(count_r_comp$bgtjobid))
    freq_scf_r_comp$per = ((freq_scf_r_comp$count)/(freq_scf_r_comp$total))*100
    freq_scf_r_comp$variable = "r_comp"
count_r_auto <- filter(count_r, onet == "49-3023.02")
  freq_s_r_auto <- count_r_auto %>% group_by(num_s) %>% summarise(count = n())
    freq_s_r_auto$total = length(unique(count_r_auto$bgtjobid))
    freq_s_r_auto$per = ((freq_s_r_auto$count)/(freq_s_r_auto$total))*100
    freq_s_r_auto$variable = "r_auto"
  freq_sc_r_auto <- count_r_auto %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_r_auto$total = length(unique(count_r_auto$bgtjobid))
    freq_sc_r_auto$per = ((freq_sc_r_auto$count)/(freq_sc_r_auto$total))*100
    freq_sc_r_auto$variable = "r_auto"
  freq_scf_r_auto <- count_r_auto %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_r_auto$total = length(unique(count_r_auto$bgtjobid))
    freq_scf_r_auto$per = ((freq_scf_r_auto$count)/(freq_scf_r_auto$total))*100
    freq_scf_r_auto$variable = "r_auto"
count_r_web <- filter(count_r, onet =="15-1134.00")
  freq_s_r_web <- count_r_web %>% group_by(num_s) %>% summarise(count = n())
    freq_s_r_web$total = length(unique(count_r_web$bgtjobid))
    freq_s_r_web$per = ((freq_s_r_web$count)/(freq_s_r_web$total))*100
    freq_s_r_web$variable = "r_web"
  freq_sc_r_web <- count_r_web %>% group_by(num_sc) %>% summarise(count = n())
    freq_sc_r_web$total = length(unique(count_r_web$bgtjobid))
    freq_sc_r_web$per = ((freq_sc_r_web$count)/(freq_sc_r_web$total))*100
    freq_sc_r_web$variable = "r_web"
  freq_scf_r_web <- count_r_web %>% group_by(num_scf) %>% summarise(count = n())
    freq_scf_r_web$total = length(unique(count_r_web$bgtjobid))
    freq_scf_r_web$per = ((freq_scf_r_web$count)/(freq_scf_r_web$total))*100
    freq_scf_r_web$variable = "r_web"

#CHART TOP 5 OCCUPATIONS OF RICHMOND BY SKILL, CLUSTER, FAMILIES
freq_s_r_onet <- rbind(freq_s_r_auto, freq_s_r_comp, freq_s_r_web, freq_s_r_maint, freq_s_r_nurs)
  ggplot(freq_s_r_onet, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nTop Five STW Occupations in Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skills Requested")

freq_sc_r_onet <- rbind(freq_sc_r_auto, freq_sc_r_comp, freq_sc_r_web, freq_sc_r_maint, freq_sc_r_nurs)
  ggplot(freq_sc_r_onet, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nTop Five STW Occupations in Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters Requested")

freq_scf_r_onet <- rbind(freq_scf_r_auto, freq_scf_r_comp, freq_scf_r_web, freq_scf_r_maint, freq_scf_r_nurs)
  ggplot(freq_scf_r_onet, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nTop Five STW Occupations in Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families Requested")
  freq_sc_r_onet <- rbind(freq_sc_r_auto, freq_sc_r_comp, freq_sc_r_web, freq_sc_r_maint, freq_sc_r_nurs)
  ggplot(freq_sc_r_onet, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nTop Five STW Occupations in Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters Requested")

  freq_scf_r_onet <- rbind(freq_scf_r_auto, freq_scf_r_comp, freq_scf_r_web, freq_scf_r_maint, freq_scf_r_nurs)
  ggplot(freq_scf_r_onet, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nTop Five STW Occupations in Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families Requested")


#COMPARE NURSES
freq_s_nurs <- rbind(freq_s_b_nurs, freq_s_r_nurs)
  ggplot(freq_s_nurs, aes(num_s, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skills for \nCritical Care Nurses in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skills Requested")
freq_sc_nurs <- rbind(freq_sc_b_nurs, freq_sc_r_nurs)
  ggplot(freq_sc_nurs, aes(num_sc, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Clusters for \nCritical Care Nurses in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Clusters Requested")
freq_scf_nurs <- rbind(freq_scf_b_nurs, freq_scf_r_nurs)
  ggplot(freq_scf_nurs, aes(num_scf, per)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle("Percent of Job Ads Requesting Number of Skill Cluster Families for \nCritical Care Nurses in Blacksburg and Richmond") +
    labs(y="% Percent of Job Ads", x ="Number of Skill Cluster Families Requested")









###ADDITIONAL METHOD
  test <- r_tot %>% select(skillclusterfamily, onetname)
  head(test)
  test$count= 1
  head(test)
  test <-  aggregate(test$count, by= list(test$skillclusterfamily, test$onetname), FUN= sum)
  head(test)

  library(ggpubr)
  ggballoonplot(test, fill = "value")+
    scale_fill_viridis_c(option = "C")

  test2 <- b_tot %>% select(skillclusterfamily, onetname)
  head(test2)
  test2$count= 1
  head(test2)
  test2 <-  aggregate(test2$count, by= list(test2$skillclusterfamily, test2$onetname), FUN= sum)
  head(test2)

  library(ggpubr)
  ggballoonplot(test2, fill = "value")+
    scale_fill_viridis_c(option = "C")







  #colnames(freq_num_s_r)[colnames(freq_num_s_r)=="num_s"] <- "range"
#colnames(freq_num_s_r)[colnames(freq_num_s_r)=="count"] <- "freq_num_s"
#colnames(freq_num_s_r)[colnames(freq_num_s_r)=="per"] <- "per_num_s"

#colnames(freq_num_sc_r)[colnames(freq_num_sc_r)=="num_sc"] <- "range"
#colnames(freq_num_sc_r)[colnames(freq_num_sc_r)=="count"] <- "freq_num_sc"
#colnames(freq_num_sc_r)[colnames(freq_num_sc_r)=="per"] <- "per_num_sc"


#merge(freq_num_s_r, freq_num_sc_r, by=c("range", "total", "variable")
)
