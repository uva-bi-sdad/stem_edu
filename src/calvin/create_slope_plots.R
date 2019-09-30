# Graphs for the most common skills
# Calvin Isch
# 2019-07-30
library(data.table)
library(dplyr)
library(ggplot2)
# library(geom_bar)

# Tables to read in
jobAds1 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/FINAL_BLACKSBURG_NURSE_AD.csv")
jobAds2 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/FINAL_RICHMOND_NURSE_AD.csv")
resumes1 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/FINAL_BLACKSBURG_NURSE_RESUME.csv")
resumes2 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/FINAL_RICHMOND_NURSE_RESUME.csv")

# Combines both MSAs together
bothJobAds <- rbindlist(list(jobAds1,jobAds2))
bothResumes <- rbindlist(list(resumes1,resumes2))


# narrows to just the variables we care about
nurses <- bothJobAds %>%
   select(hard_soft,skill,bgtjobid)

# And separates based on hard or soft
nurse_hard <- nurses[nurses$hard_soft == "hard"] %>%
  select(hard_soft,skill)
nurse_soft <- nurses[nurses$hard_soft == "soft"] %>%
  select(hard_soft,skill)

# Rearrange the data to just be a count of the top 5.
r <- as.data.frame(table(nurse_hard$skill))
r <- r[with(r,order(-Freq)),]
r <- r[1:5,]
r$hard_soft <- "hard"
s <- as.data.frame(table(nurse_soft$skill))
s <- s[with(s,order(-Freq)),]
s <- s[1:5,]
s$hard_soft <- "soft"
r <- r[with(r,order(Var1)),]
s <- s[with(r,order(Var1)),]

# Put them into a datatable and add proportion
all_skills1 <- rbindlist(list(r,s))
colnames(all_skills1) <- c("Skill","Frequency","Hard/Soft")
all_skills1$Proportions <- all_skills1$Frequency / length(unique(nurses$bgtjobid)) * 100

# Narrow down resumes to info that matters
nursesRes_all <- bothResumes %>%
  select(skill,bgtresid)

# Looks at the same skills
nursesRes_hard <- nursesRes_all[nursesRes_all$skill == "Critical Care Nursing"
                                | nursesRes_all$skill == "Advanced Cardiac Life Support (ACLS)"
                                | nursesRes_all$skill == "Patient Care"
                                | nursesRes_all$skill == "Neonatal Intensive Care Unit (NICU)"
                                | nursesRes_all$skill == "Life Support"]
nursesRes_soft <- nursesRes_all[nursesRes_all$skill == "Planning"
                                | nursesRes_all$skill == "Problem Solving"
                                | nursesRes_all$skill == "Communication Skills"
                                | nursesRes_all$skill == "Teamwork / Collaboration"
                                | nursesRes_all$skill == "Critical Thinking"]


# Puts them in frequency format
r2 <- as.data.frame(table(nursesRes_hard$skill))
r2 <- r2[with(r2,order(Var1)),]
r2$hard_soft <- "hard"
s2 <- as.data.frame(table(nursesRes_soft$skill))
s2 <- s2[with(s2,order(Var1)),]
s2$hard_soft <- "soft"

# And combines to a nice data.table
all_skills2 <- rbindlist(list(r2,s2))
colnames(all_skills2) <- c("Skill","Frequency","Hard/Soft")
all_skills2$Proportions <- all_skills2$Frequency / length(unique(nursesRes_all$bgtresid)) * 100


# Puts the resumes and job ads together
all_skills1 <- all_skills1[order(as.character(all_skills1$Skill)),]
all_skills1$JobRes <- "Job_Ads"
all_skills2 <- all_skills2[order(as.character(all_skills2$Skill)),]
all_skills2$JobRes <- "Resumes"
all_skills <- rbindlist(list(all_skills1,all_skills2))

# Put the table in usable format for a slope plot
skills_nice <- data.frame(  Skill = as.factor(all_skills$Skill),
                            Frequency = as.numeric(all_skills$Frequency),
                            HardSoft = as.factor(all_skills$`Hard/Soft`),
                            Proportions = as.numeric(round(all_skills$Proportions,2)),
                            JobRes = as.factor(all_skills$JobRes))

# Puts the data in usable format for bar chart
all_skills3 <- all_skills2
all_skills3$Proportions <- -all_skills2$Proportions
all_skills4 <- rbindlist(list(all_skills1,all_skills3))
skills_nice2 <- data.frame(  Skill = as.factor(all_skills4$Skill),
                            Frequency = as.numeric(all_skills4$Frequency),
                            HardSoft = as.factor(all_skills4$`Hard/Soft`),
                            Proportions = as.numeric(round(all_skills4$Proportions,2)),
                            JobRes = as.factor(all_skills4$JobRes))
skills_nice2_soft <- skills_nice2[skills_nice2$HardSoft == "soft",]
skills_nice2_hard <- skills_nice2[skills_nice2$HardSoft == "hard",]

# Makes the bar chart
ggplot(skills_nice2_hard, aes(x=Skill, y=Proportions, fill=JobRes)) +
  geom_bar(stat="identity", position="identity") +
  scale_fill_manual(values = c("#f17e1d","#02abd6")) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(limits=c(-90, 90)) +
  theme(panel.border     = element_blank()) +
  theme(axis.title.y     = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x     = element_blank()) +
  #theme(panel.grid.minor.x = element_blank()) +
  #theme(panel.grid.major.x = element_blank()) +
  labs(
    title = "Skills requested in Job Ads vs Skills Listed on Resumes"
    #caption = "based on data collected from Burning Glass for the years 2016-17"
  )
ggplot(skills_nice2_soft, aes(x=Skill, y=Proportions, fill=JobRes)) +
  geom_bar(stat="identity", position="identity") +
  scale_fill_manual(values = c("#f17e1d","#02abd6")) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(limits=c(-90, 90)) +
  theme(panel.border     = element_blank()) +
  theme(axis.title.y     = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x     = element_blank()) +
  #theme(panel.grid.minor.x = element_blank()) +
  #theme(panel.grid.major.x = element_blank()) +
  labs(
    #title = "Skills requested in Job Ads vs Skills Listed on Resumes",
    caption = "based on data collected from Burning Glass for the years 2016-17"
  )


# Creates a slope plot.
ggplot(data = skills_nice, aes(x = JobRes, y = Proportions, group = Skill)) +
  geom_line(aes(color = HardSoft), size = 2) +
  geom_point(aes(color = HardSoft), size = 4) +
  #  Labelling as desired
  scale_x_discrete(position = "top") +
  scale_color_manual(values = c("orange","blue")) +
  theme_bw() +
  # Things I'm changing
  #theme(legend.position = "none") +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.ticks       = element_blank()) +
  labs(
    title = "Skills requested in Job Ads vs Skills Listed on Resumes",
    caption = "based on data collected from Burning Glass"
  )
