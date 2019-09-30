library(data.table)
library(tidyverse)
library(lubridate)

######Combining 2016 and 2017 ads/skills into master data files

loc <- "data/stem_edu/working/job_ads_2016"
main_2016 <- fread(file.path(loc, "job_ads_main_2016.csv"))
skills_2016 <- fread(file.path(loc, "job_ads_skills_2016.csv"))

str(main_2016)

loc <- "data/stem_edu/working/burning_glass"
main_2017<- readRDS(file.path(loc, "ads_main_2017_51.RDS"))
skills_2017<- readRDS(file.path(loc, "ads_skills_2017_51.RDS"))

##########SKILLS##########

####checking to make sure they have the same columns
ncol(skills_2017)
ncol(skills_2016)
sum(colnames(skills_2016) == colnames(skills_2017))/(ncol(skills_2016) + ncol(skills_2017))

#actually main 2016 has a useless V1, taking that out
skills_2016 <- skills_2016[,2:10]

#checking to make sure 2017 data is all from 2017/2016 data is all from 2016,
#and that all 12 months are represented in it

unique(year(skills_2017$jobdate))
unique(year(skills_2016$JobDate))

unique(month(skills_2017$jobdate))
unique(month(skills_2016$JobDate))

#checking that columns have the same class
col_compare <- data.frame(colnames(skills_2016), colnames(skills_2017))
col_compare$type_2016 <- unname(sapply(skills_2016, class))
col_compare$type_2017 <- unname(sapply(skills_2017, class))
col_compare$match <- col_compare$type_2016 == col_compare$type_2017

#two variables have different classes--going to try to get everything matched up before combining the two files
mismatch <- filter(col_compare, match == FALSE)

skills_2016$BGTJobId <- as.character(skills_2016$BGTJobId)
skills_2016$JobDate <- ymd(skills_2016$JobDate)

##rerun comparison to make sure everything matches
col_compare <- data.frame(colnames(skills_2016), colnames(skills_2017))
col_compare$type_2016 <- unname(sapply(skills_2016, class))
col_compare$type_2017 <- unname(sapply(skills_2017, class))
col_compare$match <- col_compare$type_2016 == col_compare$type_2017
sum(col_compare$match)/nrow(col_compare)

#putting everything together
colnames(skills_2016) <- colnames(skills_2017)
all_job_ad_skills <- rbind(skills_2017, skills_2016)
nrow(all_job_ad_skills) == nrow(skills_2016)+nrow(skills_2017)

##########MAIN JOB AD##########

####checking to make sure they have the same columns
ncol(main_2017)
ncol(main_2016)
sum(colnames(main_2016) == colnames(main_2017))/(ncol(main_2016) + ncol(main_2017))

#actually main 2016 has a useless V1, taking that out
main_2016 <- main_2016[,2:54]

#now 2017 has one more column than 2016--what's the difference?
colnames_compare <- data.frame(c(colnames(main_2016),NA), colnames(main_2017))
#mo column appears to be the month of the job posting--is it an exact match for the months in that dataset?
sum(main_2017$mo == month(main_2017$jobdate))/sum(!is.na(main_2017$jobdate))
#mo column is just a repeat of what's in the job posting date, going to pull it out.
main_2017 <- main_2017[,1:53]

#checking to make sure 2017 data is all from 2017/2016 data is all from 2016,
#and that all 12 months are represented in it

unique(year(main_2017$jobdate))
unique(year(main_2016$JobDate))

unique(month(main_2017$jobdate))
unique(month(main_2016$JobDate))

#checking that columns have the same class
col_compare <- data.frame(colnames(main_2016), colnames(main_2017))
col_compare$type_2016 <- unname(sapply(main_2016, class))
col_compare$type_2017 <- unname(sapply(main_2017, class))
col_compare$match <- col_compare$type_2016 == col_compare$type_2017

#three variables have different classes--going to try to get everything matched up before combining the two files
mismatch <- filter(col_compare, match == FALSE)

main_2016$BGTJobId <- as.character(main_2016$BGTJobId)
main_2016$JobDate <- ymd(main_2016$JobDate)
main_2017$occfam <- as.character(main_2017$occfam)

##rerun comparison to make sure everything matches
col_compare <- data.frame(colnames(main_2016), colnames(main_2017))
col_compare$type_2016 <- unname(sapply(main_2016, class))
col_compare$type_2017 <- unname(sapply(main_2017, class))
col_compare$match <- col_compare$type_2016 == col_compare$type_2017
sum(col_compare$match)/nrow(col_compare)

#putting everything together
colnames(main_2016) <- colnames(main_2017)
all_job_ad_main <- rbind(main_2017, main_2016)
nrow(all_job_ad_main) == nrow(main_2016)+nrow(main_2017)

####sanity check
unique(year(all_job_ad_main$jobdate))
unique(year(all_job_ad_skills$jobdate))


##write out both
loc <- "data/stem_edu/working/burning_glass_ad_combine_16_17"
write.csv(all_job_ad_main, file.path(loc, "combine_16_17_job_ad_main.csv"))
write.csv(all_job_ad_skills, file.path(loc, "combine_16_17_job_ad_skill.csv"))
