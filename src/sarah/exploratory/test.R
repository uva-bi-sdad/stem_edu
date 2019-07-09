library(dplyr)
library(ggplot2)

#Read in top5 data
b_tot <- read.csv("data/stem_edu/working/skills_by_occupation/top5_rb_separate/b_top5-stw-jobskill.csv")


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


colnames(freq_num_s_b)[colnames(freq_num_s_b)=="num_s"] <- "range"
colnames(freq_num_s_b)[colnames(freq_num_s_b)=="count"] <- "freq_num_s"
colnames(freq_num_s_b)[colnames(freq_num_s_b)=="per"] <- "per_num_s"

colnames(freq_num_sc_b)[colnames(freq_num_sc_b)=="num_sc"] <- "range"
colnames(freq_num_sc_b)[colnames(freq_num_sc_b)=="count"] <- "freq_num_sc"
colnames(freq_num_sc_b)[colnames(freq_num_sc_b)=="per"] <- "per_num_sc"


test <- merge(freq_num_s_b, freq_num_sc_b, by=c("range", "total", "variable")
)
