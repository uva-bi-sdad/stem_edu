# Graphs for the most common skills
# Calvin Isch
# 2019-07-30
library(data.table)
library(dplyr)

# Tables to read in
jobAds1 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/blacksburg_top5_stw_jobs_all_skills.csv")
jobAds2 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/richmond_top5_stw_jobs_all_skills.csv")
resumes1 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/resume_with_bachelors_b_skill.csv")
resumes2 <- fread("../stem_edu/data/stem_edu/final/dspg19_analysis/resume_with_bachelors_r_skill.csv")

bothJobAds <- rbindlist(list(jobAds1,jobAds2))
bothResumes <- rbindlist(list(resumes1,resumes2))


# select only the nurses
nurses <- bothJobAds[bothJobAds$onet == "29-1141.03",]
nurses <- nurses %>%
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
nursesRes <- bothResumes[bothResumes$skillName == "Critical Care" | bothResumes$skillName == "Advanced Cardiac Life Support (ACLS)" | bothResumes$skillName == "Patient Care" | bothResumes$skillName == "Neonatal Intensive Care Unit (NICU)" | bothResumes$skillName == "Life Support",]$BGTResId
nursesRes_all <- bothResumes[as.character(bothResumes$BGTResId) %chin% as.character(nursesRes),]
colnames(nursesRes_all) <- c("V1", "V2", "BGTResId", "skillId", "skillName","SkillClusterName", "SkillClusterFamilyName", "IsBaseline","IsSoftware","IsSpecialized" ,"bachelor")
nursesRes_all <- nursesRes_all %>%
  select(skillName,bachelor)

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
all_skills2 <- all_skills2[order(as.character(all_skills2$Skill)),]

all_skills <- all_skills1
all_skills$resFreq <- all_skills2$Frequency
all_skills$resProp <- all_skills2$Proportions
colnames(all_skills) <- c("Skill", "jobFreq", "Hard/Soft", "jobProp","resFreq","resProp")


# Example code for how to create a slope plot
licenseBreakdown <- rbindlist(list(a,b))

licensesNice <- data.frame( Year = as.factor(licenseBreakdown$year),
                            licenses = as.factor(licenseBreakdown$V1),
                            Percent = as.numeric(round(licenseBreakdown$percent * 100,2)))

ggplot(data = licensesNice, aes(x = Year, y = Percent, group = licenses)) +
  geom_line(aes(color = licenses, alpha = 1), size = 2) +
  geom_point(aes(color = licenses, alpha = 1), size = 4) +
  #  Labelling as desired
  scale_x_discrete(position = "top") +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.ticks       = element_blank()) +
  labs(
    title = "Change in Open Source License Proportion, 2012-2018",
    caption = "based on a query of the top 5 OSS licenses on Github"
  )
