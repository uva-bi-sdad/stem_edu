# Attempting to cluster the jobs by skills

library(DataExplorer)
library(data.table)
library(ggplot2)

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
    nurse_skill <- c(nurse_skill,nurses$skillcluster[m])
  }
  nurses_skillc <- c(nurses_skillc,list(nurse_skill))
}
length(nurses_skillc)

# Get's a list of the unique skill clusters for nurses
nurse_skill_cluster <- unique(nurses$skillcluster)
nurse_skill_cluster

# Creates a list of lists where for each jobad, it has a number for all of the skill clusters asked
# for and it has a 0 in the rest of the spaces
nurse_skillc_nice <- c()
for (n in seq(1:length(nurses_skillc))) {
  skillset <- rep(0,length(nurse_skill_cluster))
  matchedSkills <- match(nurses_skillc[[n]],nurse_skill_cluster)
  #print(matchedSkills)
  for (m in matchedSkills) {
    skillset[m] <- skillset[m] + 1
  }
  # skillset[1]<-0 # This would make the NA skill clusters 0 so they don't influence the result
  nurse_skillc_nice <- c(nurse_skillc_nice,list(skillset))
}
nurse_skillc_nice

# Changes the list of lists to a nice data-table
nurse_dt <- do.call(rbind, nurse_skillc_nice)
ob <- kmeans(nurse_dt, centers = 15,iter.max = 10, nstart =1)
which_groups <- fitted(ob, method = "classes")
plot_histogram(which_groups)





# ===================== THAT WAS A TEST NOW LETS GET FOR REAL ====================== #
# The goal of this section of code it to take the data we have on the top three jobs in the
# MSAs, run it through a k-means clustering with k = 3 that clusters solely on skills, then get
# statistics for the number of each job we have in the categories. This tells us how much overlap
# the different jobs have in terms of their skill requirements for our MSAs. For jobs with
# many overlapping skills (e.g. IT and CS), this might shed light on just how similar the two
# jobs are.

# All unique job postings
all_jobs <- unique(job_skills$bgtjobid)

# Aggregates the skill clusters for each job-id.
skill_clusters_top_three <- c()
for (n in seq(1:length(all_jobs))) {
  job_skill <- c()
  print(n)
  for (m in which(all_jobs$bgtjobid == y[n])){
    job_skill <- c(job_skill,job_skills$skillcluster[m])
  }
  skill_clusters_top_three <- c(skill_clusters_top_three,list(job_skill))
}
length(skill_clusters_top_three)

# Get's a list of the all unique skill clusters for this job set
all_skill_clusters <- unique(job_skills$skillcluster)
all_skill_clusters

# Creates a list of lists where for each jobad, it has a number for all of the skill clusters asked
# for and it has a 0 in the rest of the spaces
all_jobs_skill_clusters <- c()
for (n in seq(1:length(skill_clusters_top_three))) {
  skillset <- rep(0,length(all_skill_clusters))
  matchedSkills <- match(skill_clusters_top_three[[n]],all_skill_clusters)
  for (m in matchedSkills) {
    skillset[m] <- skillset[m] + 1
  }
  skillset[1]<-0 # This would make the NA skill clusters 0 so they don't influence the result
  all_jobs_skill_clusters <- c(all_jobs_skill_clusters,list(skillset))
  print(length(all_jobs_skill_clusters))
}
all_jobs_skill_clusters


# Changes the list of lists to a nice data-table
all_jobs_dt <- do.call(rbind, all_jobs_skill_clusters)
ob <- kmeans(all_jobs_dt, centers = 3,iter.max = 10, nstart =1)
which_groups <- fitted(ob, method = "classes")

onets <- c()
for (id in all_jobs) {
  n <- match(id,job_skills$bgtjobid)
  onets <- c(onets, job_skills$onetname[n])
}

groups <- data.table(bgtjobid = all_jobs, cluster = which_groups, onet = onets)
groups2 <- groups %>%
  select("cluster","onet")
plot_histogram(which_groups)

w = table(groups2$cluster,groups2$onet)
w = t(w)
w <- as.data.frame.matrix(w)

# Still working on the neat visualization...
p  <- ggplot(data=w, aes(x=factor(1), stat="bin")) + geom_bar(position="fill")
# p  <- ggplot(data=mtcars, aes(x=factor(1), stat="bin", fill=cyl)) + geom_bar(position="fill") # Stacked bar chart
# p <- p + ggtitle("Cylinders by Gears") + xlab("") + ylab("Gears") # Adds titles
# p <- p + facet_grid(facets=. ~ gear) # Side by side bar chart
p <- p + coord_polar(theta="y") # side by side pie chart
p
