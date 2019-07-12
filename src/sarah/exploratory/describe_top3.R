library(dplyr)
library(ggplot2)

top3_t <- read.csv("data/stem_edu/working/skills_by_occupation/top3occ/hardskills_top3-stw-position.csv")

top3_b <- filter(top3_t, top3_t$place == "Blacksburg")
top3_r <- filter(top3_t, top3_t$place =="Richmond")

# number of unique skills associated with each job
count_s_t <- top3_t %>% select(bgtjobid, skill)
count_s_t <- count_s_t %>%
  group_by(bgtjobid) %>%
  summarize(num_s = n_distinct(skill))

#number of unique skill clusters associated with each job
count_sc_t <- top3_t %>% select(bgtjobid, skillcluster)
count_sc_t <- count_sc_t %>%
  group_by(bgtjobid) %>%
  summarize(num_sc = n_distinct(skillcluster))

#number of unique skill cluster families associated with each job
count_scf_t <- top3_t %>% select(bgtjobid, skillclusterfamily)
count_scf_t <- count_scf_t %>%
  group_by(bgtjobid) %>%
  summarize(num_scf = n_distinct(skillclusterfamily))

#combine skill counts
count_t <- merge(count_s_t, count_sc_t, by = "bgtjobid")
count_t <- merge(count_t, count_scf_t, by = "bgtjobid")

#number of skills frequency
freq_num_s_t <- count_t %>% group_by(num_s) %>% summarise(count = n())

#ggplot(data=freq_num_s_t, aes(x=num_s, y=count)) +
 # geom_bar(stat="identity") +
  #ggtitle("Plot of Number of Jobs Requiring Number of Skills")

#number of skill cluster  frequency
freq_num_sc_t <- count_t %>% group_by(num_sc) %>% summarise(count = n())

#ggplot(data=freq_num_sc_t, aes(x=num_sc, y=count)) +
 # geom_bar(stat="identity") +
  #ggtitle("Plot of Number of Jobs Requiring Number of Skill Clusters")

#number of skill cluster family frequency
freq_num_scf_t <- count_t %>% group_by(num_scf) %>% summarise(count = n())

#ggplot(data=freq_num_scf_t, aes(x=num_scf, y=count)) +
 # geom_bar(stat="identity") +
  #ggtitle("Plot of Number of Jobs Requiring Number of Skill Cluster Families")

#boxplot(count_t$num_s, horizontal = TRUE)
#summary(count_t$num_s)

#boxplot(count_t$num_sc, horizontal = TRUE)
#summary(count_t$num_sc)

#boxplot(count_t$num_scf, horizontal = TRUE)
#summary(count_t$num_scf)

specialized <- filter(count_t, count_t$num_scf==1 & count_t$num_s ==1)
specialized <- merge(specialized, top3_t, by = "bgtjobid")
specialized %>% group_by(skillclusterfamily)%>% summarise(count = n())

#BLACKSBURG
# number of unique skills associated with each job
count_s_b <- top3_b %>% select(bgtjobid, skill)
count_s_b <- count_s_b %>%
  group_by(bgtjobid) %>%
  summarize(num_s = n_distinct(skill))

#number of unique skill clusters associated with each job
count_sc_b <- top3_b %>% select(bgtjobid, skillcluster)
count_sc_b <- count_sc_b %>%
  group_by(bgtjobid) %>%
  summarize(num_sc = n_distinct(skillcluster))

#number of unique skill cluster families associated with each job
count_scf_b <- top3_b %>% select(bgtjobid, skillclusterfamily)
count_scf_b <- count_scf_b %>%
  group_by(bgtjobid) %>%
  summarize(num_scf = n_distinct(skillclusterfamily))

#combine skill counts
count_b <- merge(count_s_b, count_sc_b, by = "bgtjobid")
count_b <- merge(count_b, count_scf_b, by = "bgtjobid")

#number of skills frequency
freq_num_s_b <- count_b %>% group_by(num_s) %>% summarise(count = n())

#ggplot(data=freq_num_s_b, aes(x=num_s, y=count)) +
 # geom_bar(stat="identity") +
  #ggtitle("Plot of Number of Jobs Requiring Number of Skills BLACKSBURG")

#number of skill cluster  frequency
freq_num_sc_b <- count_b %>% group_by(num_sc) %>% summarise(count = n())

#ggplot(data=freq_num_sc_b, aes(x=num_sc, y=count)) +
 # geom_bar(stat="identity") +
  #ggtitle("Plot of Number of Jobs Requiring Number of Skill Clusters BLACKSBURG")

#number of skill cluster family frequency
freq_num_scf_b <- count_b %>% group_by(num_scf) %>% summarise(count = n())

#ggplot(data=freq_num_scf_b, aes(x=num_scf, y=count)) +
 # geom_bar(stat="identity") +
  #ggtitle("Plot of Number of Jobs Requiring Number of Skill Cluster Families BLACKSBURG")

#RICHMOND
# number of unique skills associated with each job
count_s_r <- top3_r %>% select(bgtjobid, skill)
count_s_r <- count_s_r %>%
  group_by(bgtjobid) %>%
  summarize(num_s = n_distinct(skill))

#number of unique skill clusters associated with each job
count_sc_r <- top3_r %>% select(bgtjobid, skillcluster)
count_sc_r <- count_sc_r %>%
  group_by(bgtjobid) %>%
  summarize(num_sc = n_distinct(skillcluster))

#number of unique skill cluster families associated with each job
count_scf_r <- top3_r %>% select(bgtjobid, skillclusterfamily)
count_scf_r <- count_scf_r %>%
  group_by(bgtjobid) %>%
  summarize(num_scf = n_distinct(skillclusterfamily))

#combine skill counts
count_r <- merge(count_s_r, count_sc_r, by = "bgtjobid")
count_r <- merge(count_r, count_scf_r, by = "bgtjobid")

#number of skills frequency
freq_num_s_r <- count_r %>% group_by(num_s) %>% summarise(count = n())

#ggplot(data=freq_num_s_r, aes(x=num_s, y=count)) +
 # geom_bar(stat="identity") +
  #ggtitle("Plot of Number of Jobs Requiring Number of Skills RICHMOND")

#number of skill cluster  frequency
freq_num_sc_r <- count_r %>% group_by(num_sc) %>% summarise(count = n())

#ggplot(data=freq_num_sc_r, aes(x=num_sc, y=count)) +
 # geom_bar(stat="identity") +
  #ggtitle("Plot of Number of Jobs Requiring Number of Skill Clusters RICHMOND")

#number of skill cluster family frequency
freq_num_scf_r <- count_r %>% group_by(num_scf) %>% summarise(count = n())

#ggplot(data=freq_num_scf_r, aes(x=num_scf, y=count)) +
 # geom_bar(stat="identity") +
  #ggtitle("Plot of Number of Jobs Requiring Number of Skill Cluster Families RICHMOND")


#plot percent of jobs requiring number of skills for blacksburg and richmond
freq_num_s_b$total = length(unique(top3_b$bgtjobid))
freq_num_s_b$per = ((freq_num_s_b$count)/(freq_num_s_b$total))*100
freq_num_s_b$variable = "Blacksburg"

freq_num_s_r$total = length(unique(top3_r$bgtjobid))
freq_num_s_r$per = ((freq_num_s_r$count)/(freq_num_s_r$total))*100
freq_num_s_r$variable ="Richmond"

freq_num_s_br <- rbind(freq_num_s_b, freq_num_s_r)

ggplot(freq_num_s_br, aes(num_s, per)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

#plot percent of jobs requiring number of skill clusters for blacksburg and richmond
freq_num_sc_b$total = length(unique(top3_b$bgtjobid))
freq_num_sc_b$per = ((freq_num_sc_b$count)/(freq_num_sc_b$total))*100
freq_num_sc_b$variable = "Blacksburg"

freq_num_sc_r$total = length(unique(top3_r$bgtjobid))
freq_num_sc_r$per = ((freq_num_sc_r$count)/(freq_num_sc_r$total))*100
freq_num_sc_r$variable ="Richmond"

freq_num_sc_br <- rbind(freq_num_sc_b, freq_num_sc_r)

ggplot(freq_num_sc_br, aes(num_sc, per)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")




#plot percent of jobs requiring number of skill cluster families for blacksburg and richmond
freq_num_scf_b$total = length(unique(top3_b$bgtjobid))
freq_num_scf_b$per = ((freq_num_scf_b$count)/(freq_num_scf_b$total))*100
freq_num_scf_b$variable = "Blacksburg"

freq_num_scf_r$total = length(unique(top3_r$bgtjobid))
freq_num_scf_r$per = ((freq_num_scf_r$count)/(freq_num_scf_r$total))*100
freq_num_scf_r$variable ="Richmond"

freq_num_scf_br <- rbind(freq_num_scf_b, freq_num_scf_r)

ggplot(freq_num_scf_br, aes(num_scf, per)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")






#looking just at Critical Care Nurses
#c <- filter(top3, top3$onetname == "Critical Care Nurses")
#c <-merge(count_combine, c, by= "bgtjobid")
#c_num_skill <- c %>% group_by(num_skill) %>% summarise(count = n())
#ggplot(data=c_num_skill, aes(x=num_skill, y=count)) +
 # geom_bar(stat="identity") +
 # ggtitle("Plot of Number of Jobs Requiring Number of Skills for Critical Care Nurses")
