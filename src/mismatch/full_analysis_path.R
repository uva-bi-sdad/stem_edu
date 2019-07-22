library(data.table)
library(tidyverse)
library(ggrepel)

ad_loc <- "data/stem_edu/working/burning_glass_ad_combine_16_17"
res_loc <- "data/stem_edu/working/MSA_Resumes"

############GOING INTO THIS, WE WILL NEED###########
#ad main file (already filtered to specific MSA)
ad_main_base <- fread(file.path(ad_loc, "richmond_top5_stw_jobs_main.csv"), drop = "V1")

#ad skill file (already filtered to specific MSA)
ad_skill_base <- fread(file.path(ad_loc, "richmond_top5_stw_jobs_all_skills.csv"), drop = "V1")

#resume main file (already filtered to specific MSA)
res_main_base <- read.delim2(file.path(res_loc, "richmond_personal.txt"), stringsAsFactors = FALSE)
colnames(res_main_base) <- c("bgtresid", "statename", "cityname", "msa", "zipcode", "gender", "noofschooldegrees", "noofcertifications", "noofjobs")

#resume skill file (already filtered to specific MSA)
res_skill_base <- read.delim2(file.path(res_loc, "richmond_skills.txt"), stringsAsFactors = FALSE)
colnames(res_skill_base) <- c("bgtresid", "skillid", "skill", "skillcluster", "skillclusterfamily",
                        "isbaseline", "issoftware", "isspecialized")

#list of near-duplicate skills
dupe_list <- as.data.frame(rbind(c("Critical Care", "Critical Care Nursing"),
                                 c("Neonatal Intensive Care Unit", "Neonatal Intensive Care Unit (NICU)"),
                                 c("Neonatal Intensive Care", "Neonatal Intensive Care Unit (NICU)"),
                                 c("Surgery", "Surgical Services"),
                                 c("Environmental Laws and Regulations", "Environmental Regulations"),
                                 c("PC Installation", "Computer Installation and Setup"),
                                 c("Preventive Maintenance", "Predictive / Preventative Maintenance")),
                                  stringsAsFactors =FALSE)

####### PREPPING SKILLS FILES ##########

###Filtering to only ads of interest (occupation name, requires bachelor +/does not require bachelor/all)
###ADJUST THIS CODE TO MEET YOUR PARTICULAR ANALYSIS

ad_main <- filter(ad_main_base, onetname == "Critical Care Nurses", edu <= 14 | is.na(edu) == TRUE)

###Filtering to only skills of interest (hard/soft/all)

ad_skill_hs <- filter(ad_skill_base, hard_soft == "hard")

###Finding skills related to the ads we want

ad_skill <- filter(ad_skill_hs, bgtjobid %in% unique(ad_main$bgtjobid))

###Cleaning (removing near-duplicates)

skill_clean <- function(dupe_list, skill_file){
  for(i in 1:nrow(dupe_list)){
    skill_file[skill_file$skill == dupe_list[i,1],"skill"] <- dupe_list[i,2]
  }
  skill_file <- filter(skill_file, skill != "na")
  skill_file
}

ad_skill <- skill_clean(dupe_list, ad_skill)

###Summary of skills for job
##ADJUST THRESHOLD FOR INCLUSION AS NEEDED--DEFAULT IS SET TO 0.5% (only looking at skills that show up
##in 0.5% of ads for the job)
ad_skill_sum <- ad_skill %>% group_by(skillclusterfamily, skillcluster, skill, hard_soft) %>% summarise(count = n(),
                     perc = round(n()/length(unique(ad_skill$bgtjobid)), 3)) %>% arrange(desc(perc))

skill_for_eval <- filter(ad_skill_sum, perc >= .005)

ad_eval <- filter(ad_skill, skill %in% skill_for_eval$skill)

###Making contingency table

ad_contingency <- as.data.frame.matrix(table(ad_eval[,c("bgtjobid", "skill")]))
ad_contingency[ad_contingency > 1] <- 1

####### PREPPING RESUME FILES ##########

###Filtering only to candidates of interest (has bachelor+/does not have bachelor+/all)
##TO BE FILLED IN ONCE WE HAVE FINAL RESUME DATA

###Clean resume skills (rename near-duplicates)

res_skill_base <- skill_clean(dupe_list, res_skill_base)

###Finding the resume skills that are included in at least 5% of job ads, and getting the unique
###resume IDs. ADJUST THIS THRESHHOLD FOR INCLUSION AS NEEDED.

skill_for_candidate <- filter(ad_skill_sum, perc >= .05)

##Look through that skill list--is there anything that is a common skill, but not specific to/sufficient for the job?
##Put in the list below.

exclude_c_skill <- c("Building Effective Relationships", "Teaching", "Customer Service")

skill_for_candidate <- filter(skill_for_candidate, ! skill %in% exclude_c_skill, hard_soft == "hard")

res_id_include <- unique(filter(res_skill_base, skill %in% skill_for_candidate$skill)$bgtresid)

###Finding the potential candidates, who have at least one of the hard skills included in (some threshhold) of jobs above

res_main <- filter(res_main_base, bgtresid %in% res_id_include)

###Finding all the skills related to potential candidates, which we are actually tracking for the match measure

res_skill <- filter(res_skill_base, bgtresid %in% res_id_include, skill %in% skill_for_eval$skill)

###Summary of skills of potential candidates

res_skill_sum <- res_skill %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(count = n(),
                                         perc = round(n()/nrow(ad_main), 3)) %>% arrange(desc(perc))

###Making contingency table

res_contingency <- as.data.frame.matrix(table(res_skill[,c("bgtresid", "skill")]))
res_contingency[res_contingency > 1] <- 1

#Are there any skills in the ad contingency table that are NOT in the resume contingency table? If so,
#note what they are and then put in a column in the right place with just zeroes.

#how many are missing? (if 0, skip to MISMATCH MEASURE)
sum(!colnames(ad_contingency) %in% colnames(res_contingency))
#what are they?
colnames(ad_contingency)[which(!colnames(ad_contingency) %in% colnames(res_contingency))]
#prepping empty columns, putting them in ad contingency, and reordering
missing <- ad_contingency[1:nrow(res_contingency),which(!colnames(ad_contingency) %in% colnames(res_contingency))]
missing[missing > 0] <- 0
res_contingency<- cbind(res_contingency, missing)
res_contingency <- select(res_contingency, colnames(ad_contingency))
#checking to make sure it worked:
sum(!colnames(ad_contingency) %in% colnames(res_contingency))
sum(colnames(ad_contingency) == colnames(res_contingency))/length(ad_contingency)

######### MISMATCH MEASURE ############

###make mismatch matrix. this might take a while.

mismatch_matrix <- function(ad_skill, res_skill){
  apply(ad_skill, MARGIN = 1, function(y){apply(res_skill, MARGIN = 1,
  function(x){sum(x[x == y], na.rm = TRUE)/sum(y, na.rm = TRUE)})})
}

mis_matrix <- mismatch_matrix(ad_contingency, res_contingency)

###find mismatch scores

mis_score <- sum(apply(mis_matrix, MARGIN = 1, mean))/nrow(ad_contingency)

#descriptives for each resume

res_desc <- data.frame(bgtresid = rownames(mis_matrix), min = NA, mean = NA, median = NA, max = NA)
res_desc$min <- apply(mis_matrix, MARGIN = 1, min)
res_desc$mean <- apply(mis_matrix, MARGIN = 1, mean)
res_desc$median <- apply(mis_matrix, MARGIN = 1, median)
res_desc$max <- apply(mis_matrix, MARGIN = 1, max)
res_desc$matchcount <- apply(mis_matrix, MARGIN = 1, function(x){sum(x>0)})

###make process-histogram to decide who is a plausible candidate

min(mis_matrix)
sum(mis_matrix == 0)

hist(mis_matrix, main = "All match scores for critical care nurses, Richmond")
hist(mis_matrix[mis_matrix > 0])

hist(res_desc[res_desc$max >= .5, "mean"], main = "Average match score for plausible critical care nursing candidates, Blacksburg")

###Use mismatch matrix to get down to plausible candidates--at least one match where they can do
###half the job?

plausible <- as.character(res_desc[res_desc$max >= .5,]$bgtresid)
mis_matrix_plausible <- mis_matrix[rownames(mis_matrix) %in% plausible,]

###generate final histogram

hist(mis_matrix_plausible, main = "All match scores for plausible critical care nursing candidates, Richmond")

######### SKILL SUPPLY ############

###just plausible candidates:

res_plausible <- res_skill[res_skill$bgtresid %in% plausible,]

###Summary table of skills for plausible candidates and ads

top_skill_plausible <- res_plausible %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% arrange(desc(count))

supply_skill <- unique(c(top_skill_plausible[1:10, ]$skill, skill_for_eval[1:10, ]$skill))

skill_table <- data.frame(skill = supply_skill, res_count = NA, ad_count = NA)
colnames(skill_table) <- c("skillclusterfamily", "skillcluster", "skill", "count", "perc", "res_count", "ad_count")
skill_table$res_count <- sapply(skill_table$skill, function(x){length(unique(res_plausible[res_plausible$skill == x,]$bgtresid))})
skill_table$ad_count <- sapply(skill_table$skill, function(x){length(unique(ad_skill[ad_skill$skill == x,]$bgtjobid))})
skill_table$res_perc <- skill_table$res_count/length(plausible)
skill_table$ad_perc <- skill_table$ad_count/length(unique(ad_skill$bgtjobid))

#Relative skill demand chart

rel_demand <- ggplot(data = skill_table) + geom_point(aes(x = ad_perc, y = res_perc - ad_perc)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .2) +
  geom_text_repel(aes(x = ad_perc, y = res_perc - ad_perc, label = skill)) +
  ylab("Over and under supply") + xlab("Demand")+ labs(title = "Critical Care Nurses in Richmond: Relative Skills", subtitle = "Percentages of skills in job ads and resumes")+theme_bw()

rel_demand

#Absolute skill demand chart

abs_demand <- ggplot(data = skill_table) + geom_point(aes(x = ad_count, y = res_count - ad_count)) +
  geom_hline(yintercept = 0, alpha = .5, linetype = "dashed") +
  geom_text_repel(aes(x = ad_count, y = res_count - ad_count, label = skill)) +
  ylab("Over and under supply") + xlab("Demand")+ labs(title = "Critical Care Nurses in Richmond: Absolute Skills", subtitle = "Counts of skills in job ads and resumes")+
  theme_bw()

abs_demand

