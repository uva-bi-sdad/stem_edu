library(data.table)
library(tidyverse)
library(ggrepel)

loc <- "data/stem_edu/final/dspg19_analysis"

#######################################################
################## LOADING DATA FILES #################
#### please customize for your analysis by putting ####
####### the right MSA name in the 4 file paths. #######
#######################################################

#ad main file (already filtered to specific MSA)
ad_main_base <- fread(file.path(loc, "richmond_top5_stw_jobs_main.csv"), drop = "V1")

#ad skill file (already filtered to specific MSA)
ad_skill_base <- fread(file.path(loc, "richmond_top5_stw_jobs_all_skills.csv"), drop = "V1")

#resume main file (already filtered to specific MSA, with bachelor's/not-bachelor's flag)
res_main_base <- fread(file.path(loc, "resume_with_bachelors_r_main.csv"), drop = "V1")
res_main_base <- select(res_main_base, colnames(res_main_base)[colnames(res_main_base) != "V1"])
colnames(res_main_base) <- c("bgtresid", "statename", "cityname", "msa", "zipcode", "gender", "noofschooldegrees",
                             "noofcertifications", "noofjobs", "bachelor")

#resume skill file (already filtered to specific MSA, with bachelor's/not-bachelor's flag)
res_skill_base <- fread(file.path(loc, "resume_with_bachelors_r_skill.csv"), drop = "V1")
res_skill_base <- select(res_skill_base, colnames(res_skill_base)[colnames(res_skill_base) != "V1"])
colnames(res_skill_base) <- c("bgtresid", "skillid", "skill", "skillcluster", "skillclusterfamily",
                        "isbaseline", "issoftware", "isspecialized", "bachelor")

#list of near-duplicate skills (note: "word processing" and "spreadsheets" are in the skill cluster for Microsoft
#Office products, making them essentially duplicates of Word and Excel rather than overall types of software)
dupe_list <- as.data.frame(rbind(c("Critical Care", "Critical Care Nursing"),
                                 c("Neonatal Intensive Care Unit", "Neonatal Intensive Care Unit (NICU)"),
                                 c("Neonatal Intensive Care", "Neonatal Intensive Care Unit (NICU)"),
                                 c("Surgery", "Surgical Services"),
                                 c("Environmental Laws and Regulations", "Environmental Regulations"),
                                 c("PC Installation", "Computer Installation and Setup"),
                                 c("Preventive Maintenance", "Predictive / Preventative Maintenance"),
                                  c("ADLs Assistance", "Activities of Daily Living (ADLS)"),
                                c("Hipaa Compliance", "Health Insurance Portability and Accountability Act (HIPAA)"),
                                c("Bathing", "Patient Bathing"),
                                c("Statistical Analysis", "Statistics"),
                                c("Spreadsheets", "Microsoft Excel"),
                                c("Word Processing", "Microsoft Word")),
                                stringsAsFactors = FALSE)


#######################################################
################ PREPPING AD SKILL FILE ###############
############## no customization needed ################
#######################################################

###Filtering to only ads of interest (occupation name, requires bachelor +/does not require bachelor/all)
###ADJUST THIS CODE TO MEET YOUR PARTICULAR ANALYSIS

ad_main <- filter(ad_main_base, onetname == "Computer User Support Specialists", edu <= 14 | is.na(edu) == TRUE)

###Filtering to only skills of interest (hard/soft/all)

ad_skill_hs <- filter(ad_skill_base, hard_soft %in% c("hard", "soft"))

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

###Summary of skills for job (only including skills that appear in at least 3% of job ads)
ad_skill_sum <- ad_skill %>% group_by(skillclusterfamily, skillcluster, skill, hard_soft) %>% summarise(count = n(),
                     perc = round(n()/length(unique(ad_skill$bgtjobid)), 3)) %>% arrange(desc(perc))

skill_for_eval <- filter(ad_skill_sum, perc >= .03)

ad_eval <- filter(ad_skill, skill %in% skill_for_eval$skill)

###Making contingency table

ad_contingency <- as.data.frame.matrix(table(ad_eval[,c("bgtjobid", "skill")]))
colnames(ad_contingency) <- make.names(colnames(ad_contingency))
ad_contingency[ad_contingency > 1] <- 1

#######################################################
################ PREPPING RESUME SKILL FILE ###########
##### please customize the education level of the #####
########## candidates you are intereseted in ##########
#######################################################

###Filtering only to candidates of interest (has bachelor+/does not have bachelor+/all)

res_main_int <- filter(res_main_base, bachelor == 0)

###Clean resume skills (rename near-duplicates)

res_skill_int <- skill_clean(dupe_list, res_skill_base)

###Finding the "critical resume skills that are included in at least 30% of job ads, and getting the unique
###resume IDs.

skill_for_candidate <- filter(ad_skill_sum, perc >= .3, hard_soft == "hard")

################################################
####### MANUAL DATA CHECK NEEDED HERE ##########
################################################

##Look through that skill list--is there anything that is a common skill, but not specific to/sufficient for the job?
##Put in the exclude_c_skill list below. Note down the final essential skills for this part of the analysis.

View(skill_for_candidate)

exclude_c_skill <- c("Building Effective Relationships", "Teaching", "Customer Service", "Customer Contact", "Bilingual",
                     "English", "Scheduling", "Multilingual", "Research", "Microsoft Windows", "Microsoft Office",
                     "Computer Literacy", "Microsoft Excel", "Lifting Ability", "Microsoft Word")

skill_for_candidate <- filter(skill_for_candidate, ! skill %in% exclude_c_skill)

res_id_include <- unique(filter(res_skill_base, skill %in% skill_for_candidate$skill)$bgtresid)

###Finding the potential candidates, who have at least one of the hard skills
###included in (some threshhold) of jobs above

res_main <- filter(res_main_int, bgtresid %in% res_id_include)

###Finding all the skills related to potential candidates, which we are actually tracking for the match measure

res_skill <- filter(res_skill_base, bgtresid %in% res_id_include, skill %in% skill_for_eval$skill)

###Summary of match skills of potential candidates

res_skill_sum <- res_skill %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(count = n(),
                                         perc = round(n()/nrow(ad_main), 3)) %>% arrange(desc(perc))

#######################################################
################ MAKING CONTINGENCY TABLES ############
##### your input will be needed to make sure the ######
#################### tables match #####################
#######################################################

res_contingency <- as.data.frame.matrix(table(res_skill[,c("bgtresid", "skill")]))
colnames(res_contingency) <- make.names(colnames(res_contingency))
res_contingency[res_contingency > 1] <- 1

################################################
####### MANUAL DATA CHECK NEEDED HERE ##########
################################################

###Are there any skills in the ad contingency table that are NOT in the resume contingency table?
###If the following line returns "0," there are not, and you can continue to the measuring
###mismatch section:
sum(!colnames(ad_contingency) %in% colnames(res_contingency))

#if there ARE skills missing, what are they?
colnames(ad_contingency)[which(!colnames(ad_contingency) %in% colnames(res_contingency))]

###IF THERE IS ONLY ONE MISSING COLUMN, DO THIS:
#replace COLNAME_GOES_HERE with the name that the previous line returned
res_contingency$COLNAME_GOES_HERE <- 0
res_contingency <- select(res_contingency, colnames(ad_contingency))


###IF THERE ARE MULTIPLE MISSING COLUMNS, JUST RUN THROUGH THIS:
missing <- as.matrix(ad_contingency[1:nrow(res_contingency),which(!colnames(ad_contingency) %in% colnames(res_contingency))])
missing[missing > 0] <- 0
res_contingency<- cbind(res_contingency, missing)
res_contingency <- select(res_contingency, colnames(ad_contingency))

##Check to make sure that it worked: this should return a 0
sum(!colnames(ad_contingency) %in% colnames(res_contingency))
##This should return a one:
sum(colnames(ad_contingency) == colnames(res_contingency))/length(ad_contingency)

#######################################################
################ MISMATCH MATRIX AND SCORE ############
##### no manual input needed, but you should note #####
############# down the value of mis_score #############
#######################################################

###make mismatch matrix. this might take a while.

mismatch_matrix <- function(ad_skill, res_skill){
  apply(ad_skill, MARGIN = 1, function(y){apply(res_skill, MARGIN = 1,
  function(x){sum(x[x == y], na.rm = TRUE)/sum(y, na.rm = TRUE)})})
}

mis_matrix <- mismatch_matrix(ad_contingency, res_contingency)

###find mismatch scores. NOTE DOWN THE VALUE OF mis_score

mis_score <- sum(apply(mis_matrix, MARGIN = 1, mean))/nrow(ad_contingency)
mis_score

#descriptives for each resume

res_desc <- data.frame(bgtresid = rownames(mis_matrix), min = NA, mean = NA, median = NA, max = NA)
res_desc$min <- apply(mis_matrix, MARGIN = 1, min)
res_desc$mean <- apply(mis_matrix, MARGIN = 1, mean)
res_desc$median <- apply(mis_matrix, MARGIN = 1, median)
res_desc$max <- apply(mis_matrix, MARGIN = 1, max)
res_desc$matchcount <- apply(mis_matrix, MARGIN = 1, function(x){sum(x>0)})

#######################################################
#################### MAKING HISTOGRAMS ################
### please edit the titles to reflect your analysis ###
############### and save the histograms ###############
#######################################################

###histograms to look at distribution of matches:

hist(mis_matrix, main = "All match scores for possible candidates")
hist(res_desc$mean, main = "Average match score for possible candidates")

#######################################################
################### SKILL SUPPLY CHARTS ###############
### please edit the titles to reflect your analysis ###
################# and save the charts #################
#######################################################

###Summary table of skills for plausible candidates and ads

top_skill <- res_skill %>% group_by(skillclusterfamily, skillcluster, skill) %>%
  summarise(count = n()) %>% arrange(desc(count))

supply_skill <- unique(c(top_skill[1:10, ]$skill, skill_for_eval[1:10, ]$skill))

skill_table <- data.frame(skill = supply_skill, res_count = NA, ad_count = NA)
colnames(skill_table) <- c("skill", "res_count", "ad_count")
skill_table$res_count <- sapply(skill_table$skill, function(x){length(unique(res_skill[res_skill$skill == x,]$bgtresid))})
skill_table$ad_count <- sapply(skill_table$skill, function(x){length(unique(ad_skill[ad_skill$skill == x,]$bgtjobid))})
skill_table$res_perc <- skill_table$res_count/length(unique(res_skill$bgtresid))
skill_table$ad_perc <- skill_table$ad_count/length(unique(ad_skill$bgtjobid))

#Relative skill demand chart

rel_demand <- ggplot(data = skill_table) + geom_point(aes(x = ad_perc, y = res_perc - ad_perc)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .2) +
  geom_text_repel(aes(x = ad_perc, y = res_perc - ad_perc, label = skill)) +
  ylab("Over and under supply") + xlab("Demand")+ labs(title = "OCCUPATION in MSA: Relative Skills", subtitle = "Percentages of skills in job ads and resumes")+theme_bw()

rel_demand

#Absolute skill demand chart

abs_demand <- ggplot(data = skill_table) + geom_point(aes(x = ad_count, y = res_count - ad_count)) +
  geom_hline(yintercept = 0, alpha = .5, linetype = "dashed") +
  geom_text_repel(aes(x = ad_count, y = res_count - ad_count, label = skill)) +
  ylab("Over and under supply") + xlab("Demand")+ labs(title = "OCCUPATION in MSA: Absolute Skills", subtitle = "Counts of skills in job ads and resumes")+
  theme_bw()

abs_demand
