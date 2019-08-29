library(tidyverse)
library(data.table)


loc <- "data/stem_edu/working/burning_glass_ad_combine_16_17"
rich_skill <- fread(file.path(loc, "richmond_top5_stw_jobs_all_skills.csv"))
rich_job <- fread(file.path(loc, "richmond_top5_stw_jobs_main.csv"))

loc_skill_class <- "data/stem_edu/working/hard_soft_skills"
hard_soft_class <- fread(file.path(loc_skill_class, "stw_hard-soft-skill.csv"))

hard_skills <- hard_soft_class[hard_soft == "hard"]$skill

loc_res <- "data/stem_edu/working/MSA_Resumes"
richmond_resume <- read.table(file.path(loc_res, "richmond_skills.txt"), stringsAsFactors = FALSE)
colnames(richmond_resume) <- c("bgtresid", "skillid", "skill", "skillcluster", "skillclusterfamily", "isbaseline", "issoftware", "isspecialized")

res_hard_skill <- richmond_resume[richmond_resume$skill %chin% hard_skills,]

####maintenance workers#####


maint_job_id <- rich_job[onetname == "Maintenance and Repair Workers, General"]$bgtjobid
maint_skill <- rich_skill[bgtjobid %in% maint_job_id]
maint_skill_sample <- rich_skill[bgtjobid %in% maint_job_id[1:10]]

res_has_repair_id <- unique(filter(res_hard_skill, skill == "Repair")$bgtresid)
res_has_repair <- res_hard_skill[res_hard_skill$bgtresid %in% res_has_repair_id,]
View(res_has_repair %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count)))
res_has_repair_top <- res_has_repair %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))
res_has_repair_top <- res_has_repair_top[1:10,]

maint_skill_hard <- maint_skill[maint_skill$skill %chin% hard_skills,]
maint_skill_top <- maint_skill %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))
maint_skill_top <- maint_skill_top[1:10,]

maint_skill_repair <- maint_skill[maint_skill$bgtjobid %in% maint_repair_jobid,]

maint_res_skill_top <- res_has_repair %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))
maint_res_skill_top <- maint_res_skill_top[1:10,]

res_additl_skill <- res_has_repair_top[! res_has_repair_top$skill %in% maint_skill_top$skill,]$skill

comb_list <- c(res_additl_skill, maint_skill_top$skill)

skill_comb <- data.frame(skill = comb_list, res_count = NA, ad_count = NA)
skill_comb$res_count <- sapply(skill_comb$skill, function(x){length(unique(res_has_repair[res_has_repair$skill == x,]$bgtresid))})
skill_comb$ad_count <- sapply(skill_comb$skill, function(x){length(unique(maint_skill_repair[maint_skill_repair$skill == x,]$bgtjobid))})

skill_comb$ad_count <- sapply(skill_comb$skill, function(x){sum(maint_skill$skill == x)})
skill_comb$res_perc <- skill_comb$res_count/length(unique(res_has_repair$bgtresid))
skill_comb$ad_perc <- skill_comb$ad_count/length(unique(maint_skill_repair$bgtjobid))

skill_comb$perc_overlap <- sapply(skill_comb$skill, function(x){min(skill_comb[skill_comb$skill == x, "ad_perc"], skill_comb[skill_comb$skill == x, "res_perc"])})
skill_comb$count_overlap <- sapply(skill_comb$skill, function(x){min(skill_comb[skill_comb$skill == x, "ad_count"], skill_comb[skill_comb$skill == x, "res_count"])})

skill_comb$ad_count_over <- sapply(skill_comb$skill, function(x){if(skill_comb[skill_comb$skill == x, "ad_count"] == skill_comb[skill_comb$skill == x, "count_overlap"]){0} else {skill_comb[skill_comb$skill == x, "ad_count"] - skill_comb[skill_comb$skill == x, "count_overlap"]}})
skill_comb$res_count_over <- sapply(skill_comb$skill, function(x){if(skill_comb[skill_comb$skill == x, "res_count"] == skill_comb[skill_comb$skill == x, "count_overlap"]){0} else {skill_comb[skill_comb$skill == x, "res_count"] - skill_comb[skill_comb$skill == x, "count_overlap"]}})

skill_comb$ad_perc_over <- sapply(skill_comb$skill, function(x){if(skill_comb[skill_comb$skill == x, "ad_perc"] == skill_comb[skill_comb$skill == x, "perc_overlap"]){0} else {skill_comb[skill_comb$skill == x, "ad_perc"] - skill_comb[skill_comb$skill == x, "perc_overlap"]}})
skill_comb$res_perc_over <- sapply(skill_comb$skill, function(x){if(skill_comb[skill_comb$skill == x, "res_perc"] == skill_comb[skill_comb$skill == x, "perc_overlap"]){0} else {skill_comb[skill_comb$skill == x, "res_perc"] - skill_comb[skill_comb$skill == x, "perc_overlap"]}})


min(skill_comb$res_perc, skill_comb$ad_perc)
skill_comb$count_overlap <- min(skill_comb$res_count, skill_comb$ad_count)
skill_comb$id <- 1:18

skill_comb_perc <- melt(skill_comb, id = c("skill", "id"), measure = c("perc_overlap", "ad_perc_over", "res_perc_over"))

ggplot(data = skill_comb) + geom_segment(aes(y = id, yend = id, x = perc_overlap*-.5,
       xend =  perc_overlap*.5), color = "purple", size = 2) + geom_segment(aes(y = id,
       yend = id, x = perc_overlap*-.5 - ad_perc_over, xend = perc_overlap*-.5), color = "red", size = 2) +
  geom_segment(aes(y = id, yend = id, x = perc_overlap*.5,
                   xend = res_perc_over + perc_overlap*.5), color = "blue", size = 2) +
  scale_y_continuous(breaks = 1:nrow(skill_comb), labels = skill_comb$skill)+ggtitle("Skill Mismatch: Maintenance and Repair Workers, General")

ggplot(data = skill_comb) + geom_point(aes(x = ad_count, y = res_count - ad_count))
ggplot(data = skill_comb) + geom_point(aes(x = ad_perc, y = res_perc - ad_perc))


maint_skill_rel <- ggplot(data = skill_comb) + geom_point(aes(x = ad_perc, y = res_perc - ad_perc)) +
  scale_y_continuous(limits = c(-.4, .4))+geom_hline(yintercept = 0, linetype = "dashed", alpha = .2) +
  geom_text_repel(aes(x = ad_perc, y = res_perc - ad_perc, label = skill), size = 3) +
  ylab("Over and under supply") + xlab("Demand")+
  labs(title = "Maintenance Workers: Relative Skills",
       subtitle = "Percentages of skills in job ads and resumes") +
  theme_bw()
maint_skill_rel

maint_skill_abs <- ggplot(data = skill_comb) + geom_point(aes(x = ad_count, y = res_count - ad_count)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .5) + scale_y_continuous(limits = c(-5000, 10000))+
  geom_text_repel(aes(x = ad_count, y = res_count - ad_count, label = skill), size = 3) +
  ylab("Over and under supply") + xlab("Demand")+
  labs(title = "Maintenance Workers: Absolute Skills", subtitle = "Counts of skills in job ads and resumes") +
  theme_bw()
maint_skill_abs


maint_job_cont_data <- maint_skill_hard[maint_skill_hard$skill %in% comb_list,]
maint_res_cont_data <- res_has_repair[res_has_repair$skill %in% comb_list,]

maint_res_contingency <- as.data.frame.matrix(table(maint_res_cont_data[,c("bgtresid", "skill")]))
maint_job_contingency <- as.data.frame.matrix(table(maint_job_cont_data[,c("bgtjobid", "skill")]))

maint_res_contingency[maint_res_contingency > 1] <- 1
maint_job_contingency[maint_job_contingency > 1] <- 1
maint_res_summary <- apply(maint_res_contingency, MARGIN = 2, sum)
maint_job_summary <- apply(maint_job_contingency, MARGIN = 2, sum)

maint_job_demand_skill <- maint_skill_top$skill

maint_res_supply_skill <- maint_res_skill_top$skill

maint_job_demand_data <- maint_skill[maint_skill$skill %in% maint_job_demand_skill,]
maint_res_demand_data <- res_has_repair[res_has_repair$skill %in% maint_job_demand_skill,]
maint_job_demand_contingency <- as.data.frame.matrix(table(maint_job_demand_data[,c("bgtjobid", "skill")]))
maint_res_demand_contingency <- as.data.frame.matrix(table(maint_res_demand_data[,c("bgtresid", "skill")]))

maint_res_demand_contingency[maint_res_demand_contingency > 1] <- 1
maint_job_demand_contingency[maint_job_demand_contingency > 1] <- 1

maint_job_demand_summary <- apply(maint_job_demand_contingency, MARGIN = 2, function(x){sum(x)/nrow(maint_job_demand_contingency)})
maint_res_demand_summary <- apply(maint_res_demand_contingency, MARGIN = 2, function(x){sum(x)/nrow(maint_res_demand_contingency)})

jaccard(maint_res_demand_summary, maint_job_demand_summary, testNA = FALSE)

nurse_job_supply_data <- nurse_skill[nurse_skill$skill %in% nurse_res_supply_skill,]
nurse_res_supply_data <- res_has_crit[res_has_crit$skill %in% nurse_res_supply_skill,]
nurse_job_supply_contingency <- as.data.frame.matrix(table(nurse_job_supply_data[,c("bgtjobid", "skill")]))
nurse_res_supply_contingency <- as.data.frame.matrix(table(nurse_res_supply_data[,c("bgtresid", "skill")]))

nurse_res_supply_contingency[nurse_res_supply_contingency > 1] <- 1
nurse_job_supply_contingency[nurse_job_supply_contingency > 1] <- 1

nurse_job_supply_summary <- apply(nurse_job_supply_contingency, MARGIN = 2, function(x){sum(x)/nrow(nurse_job_supply_contingency)})
nurse_res_supply_summary <- apply(nurse_res_supply_contingency, MARGIN = 2, function(x){sum(x)/nrow(nurse_res_supply_contingency)})

library(philentropy)


jaccard(nurse_res_summary, nurse_job_summary, testNA = FALSE)
jaccard(nurse_res_supply_summary, nurse_job_supply_summary, testNA = FALSE)
jaccard(nurse_res_demand_summary, nurse_job_demand_summary, testNA = FALSE)


####critical care nurses#####


nurse_job_id <- rich_job[onetname == "Critical Care Nurses"]$bgtjobid
nurse_skill <- rich_skill[rich_skill$bgtjobid %in% nurse_job_id & rich_skill$skill %in% hard_skills,]
top_nurse_skill <- nurse_skill %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))
nurse_skill[nurse_skill$skill == "Critical Care Nursing","skill"] <- "Critical Care"
nurse_skill[nurse_skill$skill == "Neonatal Intensive Care","skill"] <- "Neonatal Intensive Care Unit (NICU)"



res_has_crit_id <- unique(filter(res_hard_skill, skill == "Critical Care" | skill == "Critical Care Nursing")$bgtresid)
res_has_crit <- res_hard_skill[res_hard_skill$bgtresid %in% res_has_crit_id,]
res_has_crit[res_has_crit$skill == "Critical Care Nursing","skill"] <- "Critical Care"
res_has_crit[res_has_crit$skill == "Neonatal Intensive Care","skill"] <- "Neonatal Intensive Care Unit (NICU)"

View(res_has_crit %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count)))
res_has_crit_top <- res_has_crit %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))
res_has_crit_top <- res_has_crit_top[1:10,]

nurse_skill_hard <- nurse_skill[nurse_skill$skill %chin% hard_skills,]
nurse_skill_top <- nurse_skill %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))
nurse_skill_top <- nurse_skill_top[1:10,]

nurse_crit_jobid <- unique(nurse_skill[nurse_skill$skill == "Critical Care",]$bgtjobid)
nurse_skill_crit <- nurse_skill[nurse_skill$bgtjobid %in% nurse_crit_jobid,]

res_nurse_additl_skill <- res_has_crit_top[! res_has_crit_top$skill %in% nurse_skill_top$skill,]$skill

comb_list_nurse <- c(res_nurse_additl_skill, nurse_skill_top$skill)

skill_comb_nurse <- data.frame(skill = comb_list_nurse, res_count = NA, ad_count = NA)
skill_comb_nurse$res_count <- sapply(skill_comb_nurse$skill, function(x){length(unique(res_has_crit[res_has_crit$skill == x,]$bgtresid))})
skill_comb_nurse$ad_count <- sapply(skill_comb_nurse$skill, function(x){length(unique(nurse_skill_crit[nurse_skill_crit$skill == x,]$bgtjobid))})

skill_comb_nurse$res_perc <- skill_comb_nurse$res_count/length(unique(res_has_crit$bgtresid))
skill_comb_nurse$ad_perc <- skill_comb_nurse$ad_count/length(unique(nurse_skill_crit$bgtjobid))

skill_comb_nurse$perc_overlap <- sapply(skill_comb_nurse$skill, function(x){min(skill_comb_nurse[skill_comb_nurse$skill == x, "ad_perc"], skill_comb_nurse[skill_comb_nurse$skill == x, "res_perc"])})
skill_comb_nurse$count_overlap <- sapply(skill_comb_nurse$skill, function(x){min(skill_comb_nurse[skill_comb_nurse$skill == x, "ad_count"], skill_comb_nurse[skill_comb_nurse$skill == x, "res_count"])})

skill_comb_nurse$ad_count_over <- sapply(skill_comb_nurse$skill, function(x){if(skill_comb_nurse[skill_comb_nurse$skill == x, "ad_count"] == skill_comb_nurse[skill_comb_nurse$skill == x, "count_overlap"]){0} else {skill_comb_nurse[skill_comb_nurse$skill == x, "ad_count"] - skill_comb_nurse[skill_comb_nurse$skill == x, "count_overlap"]}})
skill_comb_nurse$res_count_over <- sapply(skill_comb_nurse$skill, function(x){if(skill_comb_nurse[skill_comb_nurse$skill == x, "res_count"] == skill_comb_nurse[skill_comb_nurse$skill == x, "count_overlap"]){0} else {skill_comb_nurse[skill_comb_nurse$skill == x, "res_count"] - skill_comb_nurse[skill_comb_nurse$skill == x, "count_overlap"]}})

skill_comb_nurse$ad_perc_over <- sapply(skill_comb_nurse$skill, function(x){if(skill_comb_nurse[skill_comb_nurse$skill == x, "ad_perc"] == skill_comb_nurse[skill_comb_nurse$skill == x, "perc_overlap"]){0} else {skill_comb_nurse[skill_comb_nurse$skill == x, "ad_perc"] - skill_comb_nurse[skill_comb_nurse$skill == x, "perc_overlap"]}})
skill_comb_nurse$res_perc_over <- sapply(skill_comb_nurse$skill, function(x){if(skill_comb_nurse[skill_comb_nurse$skill == x, "res_perc"] == skill_comb_nurse[skill_comb_nurse$skill == x, "perc_overlap"]){0} else {skill_comb_nurse[skill_comb_nurse$skill == x, "res_perc"] - skill_comb_nurse[skill_comb_nurse$skill == x, "perc_overlap"]}})


skill_comb_nurse$id <- 1:nrow(skill_comb_nurse)

skill_comb_nurse_perc <- melt(skill_comb_nurse, id = c("skill", "id"), measure = c("perc_overlap", "ad_perc_over", "res_perc_over"))

ggplot(data = skill_comb_nurse) + geom_segment(aes(y = id, yend = id, x = perc_overlap*-.5,
         xend =  perc_overlap*.5), color = "purple", size = 2) + geom_segment(aes(y = id,
        yend = id, x = perc_overlap*-.5 - ad_perc_over, xend = perc_overlap*-.5), color = "red", size = 2) +
        geom_segment(aes(y = id, yend = id, x = perc_overlap*.5,
         xend = res_perc_over + perc_overlap*.5), color = "blue", size = 2) +
      scale_y_continuous(breaks = 1:nrow(skill_comb_nurse), labels = skill_comb_nurse$skill) + ggtitle("Skill Mismatch: Critical Care Nursing")

nurse_skill_rel <- ggplot(data = skill_comb_nurse) + geom_point(aes(x = ad_perc, y = res_perc - ad_perc)) +
  scale_y_continuous(limits = c(-.5, .5))+geom_hline(yintercept = 0) +
  geom_text(aes(x = ad_perc, y = res_perc - ad_perc, label = skill), hjust = 1.2, size = 3) +
  ylab("Over and under supply") + xlab("Demand")+ labs(title = "Critical Care Nurses: Relative Skills", subtitle = "Percentages of skills in job ads and resumes")

nurse_skill_rel <- ggplot(data = skill_comb_nurse) + geom_point(aes(x = ad_perc, y = res_perc - ad_perc)) +
  scale_y_continuous(limits = c(-.5, .5))+geom_hline(yintercept = 0, linetype = "dashed", alpha = .2) +
  geom_text_repel(aes(x = ad_perc, y = res_perc - ad_perc, label = skill)) +
  ylab("Over and under supply") + xlab("Demand")+ labs(title = "Critical Care Nurses: Relative Skills", subtitle = "Percentages of skills in job ads and resumes")+theme_bw()


nurse_skill_abs <- ggplot(data = skill_comb_nurse) + geom_point(aes(x = ad_count, y = res_count - ad_count)) +
  geom_hline(yintercept = 0, alpha = .5, linetype = "dashed") + scale_y_continuous(limits = c(-500, 500))+
  geom_text_repel(aes(x = ad_count, y = res_count - ad_count, label = skill)) +
  ylab("Over and under supply") + xlab("Demand")+ labs(title = "Critical Care Nurses: Absolute Skills", subtitle = "Counts of skills in job ads and resumes")+
  theme_bw()


ggplot(data = skill_comb_nurse) + geom_point(aes(x = ad_count, y = res_count - ad_count))

nurse_job_cont_data <- nurse_skill_hard[nurse_skill_hard$skill %in% comb_list_nurse,]
nurse_res_cont_data <- res_has_crit[res_has_crit$skill %in% comb_list_nurse,]

nurse_res_contingency <- as.data.frame.matrix(table(nurse_res_cont_data[,c("bgtresid", "skill")]))
nurse_job_contingency <- as.data.frame.matrix(table(nurse_job_cont_data[,c("bgtjobid", "skill")]))

nurse_res_contingency[nurse_res_contingency > 1] <- 1
nurse_job_contingency[nurse_job_contingency > 1] <- 1
nurse_res_summary <- apply(nurse_res_contingency, MARGIN = 2, sum)
nurse_job_summary <- apply(nurse_job_contingency, MARGIN = 2, sum)

nurse_job_demand_skill <- nurse_skill_top$skill

nurse_res_supply_skill <- res_has_crit_top$skill

nurse_job_demand_data <- nurse_skill[nurse_skill$skill %in% nurse_job_demand_skill,]
nurse_res_demand_data <- res_has_crit[res_has_crit$skill %in% nurse_job_demand_skill,]
nurse_job_demand_contingency <- as.data.frame.matrix(table(nurse_job_demand_data[,c("bgtjobid", "skill")]))
nurse_res_demand_contingency <- as.data.frame.matrix(table(nurse_res_demand_data[,c("bgtresid", "skill")]))

nurse_res_demand_contingency[nurse_res_demand_contingency > 1] <- 1
nurse_job_demand_contingency[nurse_job_demand_contingency > 1] <- 1

nurse_job_demand_summary <- apply(nurse_job_demand_contingency, MARGIN = 2, function(x){sum(x)/nrow(nurse_job_demand_contingency)})
nurse_res_demand_summary <- apply(nurse_res_demand_contingency, MARGIN = 2, function(x){sum(x)/nrow(nurse_res_demand_contingency)})

nurse_job_supply_data <- nurse_skill[nurse_skill$skill %in% nurse_res_supply_skill,]
nurse_res_supply_data <- res_has_crit[res_has_crit$skill %in% nurse_res_supply_skill,]
nurse_job_supply_contingency <- as.data.frame.matrix(table(nurse_job_supply_data[,c("bgtjobid", "skill")]))
nurse_res_supply_contingency <- as.data.frame.matrix(table(nurse_res_supply_data[,c("bgtresid", "skill")]))

nurse_res_supply_contingency[nurse_res_supply_contingency > 1] <- 1
nurse_job_supply_contingency[nurse_job_supply_contingency > 1] <- 1

nurse_job_supply_summary <- apply(nurse_job_supply_contingency, MARGIN = 2, function(x){sum(x)/nrow(nurse_job_supply_contingency)})
nurse_res_supply_summary <- apply(nurse_res_supply_contingency, MARGIN = 2, function(x){sum(x)/nrow(nurse_res_supply_contingency)})

library(philentropy)
set_similarity(nurse_res_summary, nurse_job_summary)
set_similarity(nurse_res_demand_summary, nurse_job_demand_summary)

jaccard(nurse_res_summary, nurse_job_summary, testNA = FALSE)
jaccard(nurse_res_supply_summary, nurse_job_supply_summary, testNA = FALSE)
jaccard(nurse_res_demand_summary, nurse_job_demand_summary, testNA = FALSE)

nurse_skill_abs
nurse_skill_rel
maint_skill_abs
maint_skill_rel
