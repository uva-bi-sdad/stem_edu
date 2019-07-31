# Graphs for the most common skills
# Calvin Isch
# 2019-07-30
library(data.table)
library(dplyr)
library(ggplot2)
library(geom_text)

# Tables to read in
hmm <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/FINAL_BLACKSBURG_NURSE_RESUME.csv")

jobAds1 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/FINAL_BLACKSBURG_NURSE_AD.csv")
jobAds2 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/FINAL_RICHMOND_NURSE_AD.csv")
resumes1 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/FINAL_BLACKSBURG_NURSE_RESUME.csv")
resumes2 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/FINAL_RICHMOND_NURSE_RESUME.csv")

bothJobAds <- rbindlist(list(jobAds1,jobAds2))
bothResumes <- rbindlist(list(resumes1,resumes2))


# select only the nurses
# nurses <- bothJobAds[bothJobAds$onet == "29-1141.03",]
nurses <- bothJobAds %>%
   select(hard_soft,skill,bgtjobid)

nurse_hard <- nurses[nurses$hard_soft == "hard"] %>%
  select(hard_soft,skill)
nurse_soft <- nurses[nurses$hard_soft == "soft"] %>%
  select(hard_soft,skill)

# get the counts of each skill
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

# Get's the resumes that might matter
#nursesRes <- bothResumes[bothResumes$skillName == "Critical Care" | bothResumes$skillName == "Advanced Cardiac Life Support (ACLS)" | bothResumes$skillName == "Patient Care" | bothResumes$skillName == "Neonatal Intensive Care Unit (NICU)" ,]$BGTResId
#nursesRes_all <- bothResumes[as.character(bothResumes$BGTResId) %chin% as.character(nursesRes),]
#colnames(nursesRes_all) <- c("V1", "V2", "BGTResId", "skillId", "skillName","SkillClusterName", "SkillClusterFamilyName", "IsBaseline","IsSoftware","IsSpecialized" ,"bachelor")
nursesRes_all <- bothResumes %>%
  select(skill,bgtresid)

# Looks at the same skills
nursesRes_hard <- nursesRes_all[nursesRes_all$skillName == "Critical Care"
                                | nursesRes_all$skillName == "Advanced Cardiac Life Support (ACLS)"
                                | nursesRes_all$skillName == "Patient Care"
                                | nursesRes_all$skillName == "Neonatal Intensive Care Unit (NICU)"
                                | nursesRes_all$skillName == "Life Support"]

nursesRes_soft <- nursesRes_all[nursesRes_all$skillName == "Planning"
                                | nursesRes_all$skillName == "Problem Solving"
                                | nursesRes_all$skillName == "Communication Skills"
                                | nursesRes_all$skillName == "Teamwork / Collaboration"
                                | nursesRes_all$skillName == "Critical Thinking"]

# Checks the 10 most common skills out of these selected
hmm <- as.data.frame(table(nursesRes_all$skill))
hmm <- hmm[with(hmm,order(-Freq)),]


# Puts them in frequency format
r2 <- as.data.frame(table(nursesRes_hard$skill))
r2 <- r2[with(r2,order(Var1)),]
r2$hard_soft <- "hard"
s2 <- as.data.frame(table(nursesRes_soft$skill))
s2 <- s2[with(s2,order(Var1)),]
s2$hard_soft <- "soft"

all_skills2 <- rbindlist(list(r2,s2))
colnames(all_skills2) <- c("Skill","Frequency","Hard/Soft")
all_skills2$Proportions <- all_skills2$Frequency / length(unique(nursesRes)) * 100


# Puts the resumes and job ads together
all_skills1 <- all_skills1[order(as.character(all_skills1$Skill)),]
all_skills1$JobRes <- "Job_Ads"
all_skills2 <- all_skills2[order(as.character(all_skills2$Skill)),]
all_skills2$JobRes <- "Resumes"

all_skills <- rbindlist(list(all_skills1,all_skills2))

# Example code for how to create a slope plot

# Put the table in usable format
skills_nice <- data.frame(  Skill = as.factor(all_skills$Skill),
                            Frequency = as.numeric(all_skills$Frequency),
                            HardSoft = as.factor(all_skills$`Hard/Soft`),
                            Proportions = as.numeric(round(all_skills$Proportions,2)),
                            JobRes = as.factor(all_skills$JobRes))

# Things for the barchart
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
  scale_fill_manual(values = c("orange","blue")) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(limits=c(-90, 90)) +
  theme(panel.border     = element_blank()) +
  theme(axis.title.y     = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    title = "Skills requested in Job Ads vs Skills Listed on Resumes"
    #caption = "based on data collected from Burning Glass for the years 2016-17"
  )

ggplot(skills_nice2_soft, aes(x=Skill, y=Proportions, fill=JobRes)) +
  geom_bar(stat="identity", position="identity") +
  scale_fill_manual(values = c("red","green")) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(limits=c(-90, 90)) +
  theme(panel.border     = element_blank()) +
  theme(axis.title.y     = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    #title = "Skills requested in Job Ads vs Skills Listed on Resumes",
    caption = "based on data collected from Burning Glass for the years 2016-17"
  )


# Plot it
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

# PERHAPS AN ALTERNATIVE TO MAKE IT PRETTIER
MySpecial <- list(
  # move the x axis labels up top
  scale_x_discrete(position = "top"),
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top      = element_text(size=12)),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5))
)

ggplot(data = skills_nice, aes(x = JobRes, y = Proportions, group = Skill)) +
  geom_line(aes(color = HardSoft, alpha = 1), size = 1) +
  geom_point(aes(color = HardSoft, alpha = 1), size = 3) +
  geom_text_repel(data = skills_nice %>% filter(JobRes == "Job_Ads"),
                  aes(label = paste0(Skill, " : ", Proportions, "%")) ,
                  hjust = "left",
                  fontface = "bold",
                  size = 4,
                  nudge_x = -.45,
                  direction = "y") +
  geom_text_repel(data = skills_nice %>% filter(JobRes == "Resumes"),
                  aes(label = paste0(Skill, " : ", Proportions, "%")) ,
                  hjust = "right",
                  fontface = "bold",
                  size = 4,
                  nudge_x = .5,
                  direction = "y") +
  MySpecial +
  labs(
    title = "Skills requested in Job Ads vs Skills Listed on Resumes",
    caption = "based on data collected from Burning Glass"
  )

