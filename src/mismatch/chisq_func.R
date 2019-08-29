library(data.table)

loc <- "data/stem_edu/working/burning_glass_ad_combine_16_17"
rich_skill <- fread(file.path(loc, "richmond_top5_stw_jobs_all_skills.csv"))
rich_job <- fread(file.path(loc, "richmond_top5_stw_jobs_main.csv"))

maint_job_id <- rich_job[onetname == "Maintenance and Repair Workers, General"]$bgtjobid
maint_skill <- rich_skill[bgtjobid %in% maint_job_id]

loc_res <- "data/stem_edu/working/MSA_Resumes"
richmond_resume <- read.table(file.path(loc_res, "richmond_skills.txt"), stringsAsFactors = FALSE)
colnames(richmond_resume) <- c("bgtresid", "skillid", "skill", "skillcluster", "skillclusterfamily", "isbaseline", "issoftware", "isspecialized")

mismatch_chisq <- function(candidate_skill, job_skill, all_job, top_count){
  ##first, get the top skills for maintenance and repair work

  top_jobskill <- job_skill %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))
  top_count_jobskill <- top_jobskill[1:top_count,]

  ##percentage of people who could be expected to have these skills might be the percent of ads
  ##for this job, times the percentage of ads that are this job?

  market_share <- nrow(filter(all_job, bgtjobid %in% job_skill$bgtjobid))/nrow(all_job)

  top_count_jobskill$expect <- (top_count_jobskill$count*market_share)/nrow(all_job)

  ##next, get the percentage of candidates who have these skills

  candidate_rel_skill <- candidate_skill %>% filter(skill %in% top_count_jobskill$skill) %>%
    group_by(skill) %>% summarise(observed = n())

  top_count_jobskill <- left_join(top_count_jobskill, candidate_rel_skill[,c("skill", "observed")])

  top_count_jobskill

  #chi <- chisq.test(top_count_jobskill$expect, top_count_jobskill$observed)

  #chi
}

