# Attempting to cluster the jobs by skills

library(DataExplorer)

job_skills <- read.csv("../stem_edu//data/stem_edu/working/skills_by_occupation/top3occ/hardskills_top3-stw-position.csv")
job_skills[job_skills == "na"] <- NA

all_vars <- c("bgtjobid", "jobdate", "skill","skillcluster", "skillclusterfamily","onetname", "place")

job_skills <- job_skills %>%
  select(all_vars)

plot_bar(job_skills)

job_skills$skill <- as.character(job_skills$skill)
job_skills$skillcluster <- as.character(job_skills$skillcluster)
job_skills$skillclusterfamily <- as.character(job_skills$skillclusterfamily)
job_skills$onetname <- as.character(job_skills$onetname)
job_skills$place <- as.character(job_skills$place)

maintenance <- job_skills[job_skills$onetname == "Maintenance and Repair Workers, General", ]
computers <- job_skills[job_skills$onetname == "Computer User Support Specialists", ]
nurses <- job_skills[job_skills$onetname == "Critical Care Nurses", ]

print(paste("There are ", length(unique(nurses$skill)), " skills in nursing, ", length(unique(computers$skill)), " skills in computers, and ", length(unique(maintenance$skill)), " skills in maintaenance.", sep = ""))
three_jobs <- c(maintenance,computers,nurses)

plot_bar(nurses)
plot_bar(maintenance)
plot_bar(computers)

# Aggregates all of the skills for each job (at least for nursing)
nurses1 <- unique(nurses$bgtjobid)
nurses_skill <- c()
for (n in seq(1:length(nurses1))) {
  nurse_skill <- c()
  print(n)
  for (m in which(nurses$bgtjobid == y[n])){
    nurse_skill <- c(nurse_skill,nurses$skill[m])
  }
  nurses_skill <- c(nurses_skill,list(nurse_skill))
}
length(nurses_skill)

# Aggregates all of the skillclusters for each job (at least for nursing)
nurses_skillc <- c()
for (n in seq(1:length(nurses1))) {
  nurse_skill <- c()
  print(n)
  for (m in which(nurses$bgtjobid == y[n])){
    nurse_skill <- c(nurse_skill,nurses$skill[m])
  }
  nurses_skillc <- c(nurses_skillc,list(nurse_skill))
}
length(nurses_skillc)


nurse_skill_cluster <- unique(nurses$skillcluster)
nurse_skill_cluster

nurse_skillc_nice <- c()
for (n in length(nurses_skillc)) {
  skillset <- rep(0,length(nurse_skill_cluster))
  matchedSkills <- match(nurses_skillc[n],nurse_skill_cluster)
  for (m in matchedSkills) {
    skillset[m] <- 1
  }
  nurse_skillc_nice <- c(nurse_skillc_nice,list(skillset))
}
nurse_skillc_nice
