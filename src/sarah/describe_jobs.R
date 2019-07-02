library(dplyr)
library(ggplot2)

top3<- read.csv("data/stem_edu/working/skills_by_occupation/top3occ/hardskills_top3-stw-position.csv")

# number of unique skills associated with each job
count_skill <- top3 %>% select(bgtjobid, skill)
count_skill <- count_skill %>%
  group_by(bgtjobid) %>%
  summarize(num_skill = n_distinct(skill))

#number of unique skill clusters associated with each job
count_skillcluster <- top3 %>% select(bgtjobid, skillcluster)
count_skillcluster <- count_skillcluster %>%
  group_by(bgtjobid) %>%
  summarize(num_skillcluster = n_distinct(skillcluster))

#number of unique skill cluster families associated with each job
count_skillclusterfamily <- top3 %>% select(bgtjobid, skillclusterfamily)
count_skillclusterfamily <- count_skillclusterfamily %>%
  group_by(bgtjobid) %>%
  summarize(num_skillclusterfamily = n_distinct(skillclusterfamily))

#combine skill counts
count_combine <- merge(count_skill, count_skillcluster, by = "bgtjobid")
count_combine <- merge(count_combine, count_skillclusterfamily, by = "bgtjobid")

#number of skills frequency
freq_num_skill <- count_combine %>% group_by(num_skill) %>% summarise(count = n())

ggplot(data=freq_num_skill, aes(x=num_skill, y=count)) +
  geom_bar(stat="identity") +
  ggtitle("Plot of Number of Jobs Requiring Number of Skills")

#number of skill cluster  frequency
freq_num_skillcluster <- count_combine %>% group_by(num_skillcluster) %>% summarise(count = n())

ggplot(data=freq_num_skillcluster, aes(x=num_skillcluster, y=count)) +
  geom_bar(stat="identity") +
  ggtitle("Plot of Number of Jobs Requiring Number of Skill Clusters")

#number of skill cluster family frequency
freq_num_skillclusterfamily <- count_combine %>% group_by(num_skillclusterfamily) %>% summarise(count = n())

ggplot(data=freq_num_skillclusterfamily, aes(x=num_skillclusterfamily, y=count)) +
  geom_bar(stat="identity") +
  ggtitle("Plot of Number of Jobs Requiring Number of Skill Cluster Families")

specialized <- filter(count_combine, count_combine$num_skillclusterfamily==1 & count_combine$num_skill ==1)

boxplot(count_combine$num_skill, horizontal = TRUE)
summary(count_combine$num_skill)

boxplot(count_combine$num_skillcluster, horizontal = TRUE)
summary(count_combine$num_skillcluster)

boxplot(count_combine$num_skillclusterfamily, horizontal = TRUE)
summary(count_combine$num_skillclusterfamily)
