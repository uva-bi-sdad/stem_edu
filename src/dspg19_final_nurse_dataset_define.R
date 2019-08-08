####getting down to final datasets for critical care nurses in Richmond and Blacksburg

library(data.table)
library(tidyverse)
library(ggrepel)

loc <- "data/stem_edu/final/dspg19_analysis"


#ad main file (already filtered to specific MSA)
ad_main_base <- fread(file.path(loc, "blacksburg_top5_stw_jobs_main.csv"), drop = "V1")

#ad skill file (already filtered to specific MSA)
ad_skill_base <- fread(file.path(loc, "blacksburg_top5_stw_jobs_all_skills.csv"), drop = "V1")

#resume main file (already filtered to specific MSA, with bachelor's/not-bachelor's flag)
res_main_base <- fread(file.path(loc, "resume_with_bachelors_b_main.csv"), drop = "V1")
res_main_base <- select(res_main_base, colnames(res_main_base)[colnames(res_main_base) != "V1"])
colnames(res_main_base) <- c("bgtresid", "statename", "cityname", "msa", "zipcode", "gender", "noofschooldegrees",
                             "noofcertifications", "noofjobs", "bachelor")

#resume skill file (already filtered to specific MSA, with bachelor's/not-bachelor's flag)
res_skill_base <- fread(file.path(loc, "resume_with_bachelors_b_skill.csv"), drop = "V1")
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


###Filtering to only ads of interest (occupation name, requires bachelor +/does not require bachelor/all)
###ADJUST THIS CODE TO MEET YOUR PARTICULAR ANALYSIS

ad_main <- filter(ad_main_base, onetname == "Critical Care Nurses", edu <= 14 | is.na(edu) == TRUE)

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


#######################################################
################ PREPPING RESUME SKILL FILE ###########
##### please customize the education level of the #####
########## candidates you are intereseted in ##########
#######################################################

###Filtering only to candidates of interest (has bachelor+/does not have bachelor+/all)

res_main_int <- filter(res_main_base, bachelor %in% c(0,1) | is.na(bachelor == TRUE))

###Clean resume skills (rename near-duplicates)

res_skill_base_clean <- skill_clean(dupe_list, res_skill_base)

###Finding the "critical" resume skills that are included in at least 30% of job ads, and getting the unique
###resume IDs.

skill_for_candidate <- filter(ad_skill_sum, perc >= .25, hard_soft == "hard")

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

res_id_include <- unique(filter(res_skill_base_clean, skill %in% skill_for_candidate$skill)$bgtresid)

res_main <- filter(res_main_int, bgtresid %in% res_id_include)

res_skill <- filter(res_skill_base_clean, bgtresid %in% res_main$bgtresid)

res_skill_sum <- res_skill %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(count = length(unique(bgtresid)),
                                                                                               perc = round(length(unique(bgtresid))/length(unique(res_skill$bgtresid)), 3)) %>% arrange(desc(perc))

##################################################################################################
#Files for plots (change names as appropriate by MSA)
BLACKSBURG_RESUME<-merge(res_main, res_skill, by="bgtresid")

BLACKSBURG_RESUME_DEDUPE <- BLACKSBURG_RESUME %>% group_by(bgtresid,
    statename, cityname, msa, zipcode,  gender, noofschooldegrees,
    noofcertifications, noofjobs, bachelor.x, skillid, skill, skillcluster,
    skillclusterfamily, isbaseline, issoftware, isspecialized, bachelor.y) %>%
  summarise(count = n())

BLACKSBURG_RESUME_FINAL <- as.data.frame(BLACKSBURG_RESUME_DEDUPE[,1:18])

loc_write <- "data/stem_edu/final/dspg19_analysis"
write_csv(BLACKSBURG_RESUME_FINAL, file.path(loc_write, "FINAL_BLACKSBURG_NURSE_RESUME.csv"))


BLACKSBURG_JOBAD<-merge(ad_main, ad_skill, by="bgtjobid")

BLACKSBURG_JOBAD_DEDUPE <- BLACKSBURG_JOBAD %>% group_by(bgtjobid,
      jobid, jobdate.x, cleantitle, canontitle, occfam, occfamname, soc,
      socname, onet.x, onetname.x, bgtocc, bgtoccname, bgtoccgroupname, bgtoccgroupname2, bgtcareerareaname,
      bgtcareerareaname2, employer, sector, sectorname, naics3, naics4, naics5, naics6,
      city, state, county, fipsstate, fipscounty, fips, lat, lon,
      bestfitmsa, bestfitmsaname, bestfitmsatype, msa, msaname, edu, maxedu,
      degree, maxdegree, exp, maxexp, minsalary, maxsalary, minhrlysalary, maxhrlysalary,
      payfrequency, salarytype, jobhours, taxterm, internship, subocc,  jobdate.y,
      skill, skillcluster, skillclusterfamily, isspecialized, isbaseline, issoftware,
      salary, onet.y, onetname.y, hard_soft) %>% summarise(count = n())

BLACKSBURG_JOBAD_FINAL <- as.data.frame(BLACKSBURG_JOBAD_DEDUPE[,1:64])

write_csv(BLACKSBURG_JOBAD_FINAL, file.path(loc_write, "FINAL_BLACKSBURG_NURSE_AD.csv"))
##################################################################################################


