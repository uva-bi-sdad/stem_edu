# Graphs for the most common skills
# Calvin Isch
# 2019-07-30
library(data.table)
library(dplyr)

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
nurse_hard <- nurses[nurses$hard_soft == "hard"]
nurse_soft <- nurses[nurses$hard_soft == "soft"]

# get the counts of each skill
r <- as.data.frame(table(nurse_hard$skill))
r <- r[with(r,order(-Freq)),]
r <- r[1:5,]
r$hard_soft <- "hard"
s <- as.data.frame(table(nurse_soft$skill))
s <- s[with(s,order(-Freq)),]
s <- s[1:5,]
s$hard_soft <- "soft"

# Put them into a datatable and add proportion
all_skills <- rbindlist(list(r,s))
colnames(all_skills) <- c("Skill","Frequency","Hard/Soft")
all_skills$Proportions <- all_skills$Frequency / length(unique(nurses$bgtjobid))

# Get's the resumes that might matter
nursesRes <- bothResumes[bothResumes$skillName == "Critical Care" | bothResumes$skillName == "Advanced Cardiac Life Support (ACLS)" | bothResumes$skillName == "Patient Care" | bothResumes$skillName == "Neonatal Intensive Care Unit (NICU)" | bothResumes$skillName == "Life Support",]
nursesRes_all <- bothResumes[as.character(bothResumes$BGTResId) %chin% as.character(nursesRes),]
nursesRes_all <- nursesRes_all %>%
  select(skillName,bachelor)


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
