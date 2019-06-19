library(tidyverse)
library(data.table)

# We read in jobs and skills files from RDS to data frames.

skills <- readRDS("~/stem_edu/data/stem_edu/working/burning_glass/ads_skills_2017_51.RDS")
ads_main_2017_51 <- readRDS("~/stem_edu/data/stem_edu/working/burning_glass/ads_main_2017_51.RDS")

# We found everything from the main file with MSAs Richmond and Blacksburg

rich_ads <- filter(ads_main_2017_51, msaname == "Richmond, VA")
blacksburg_ads <- filter(ads_main_2017_51, msaname =="Blacksburg-Christiansburg-Radford, VA")

# We are finding skills with job IDs of ads in Richmond and Blacksburg

rich_job_id <- rich_ads$bgtjobid
rich_skills <- skills[bgtjobid %chin% rich_job_id]

blacksburg_job_id<-blacksburg_ads$bgtjobid
blacksburg_skills<-skills[bgtjobid %chin% blacksburg_job_id]

#writing out job ads/skills data (Sarah, the data files got really big, so I redirected them back
#to the data folder rather than having them in something that will sync to github. They're commented
#out so we won't write these giant files by accident. Delete the pound signs if you want to run them.)

write.csv(rich_ads, "~/stem_edu/data/stem_edu/working/Team_SA_job_skills_filter/rich_jobs.csv")
write.csv(blacksburg_ads, "~/stem_edu/data/stem_edu/working/Team_SA_job_skills_filter/blacksburg_jobs.csv")
write.csv(rich_skills, "~/stem_edu/data/stem_edu/working/Team_SA_job_skills_filter/rich_skills.csv")
write.csv(blacksburg_skills, "~/stem_edu/data/stem_edu/working/Team_SA_job_skills_filter/blacksburg_skills.csv")


