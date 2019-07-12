library(DataExplorer)
library(devtools)
library(dplyr)
library(tidyverse)
library(data.table)

main_vars <-c("BGTResId","StateName","cityName","msa","zipcode","NoofSchoolDegrees","Noofcertifications","NoOfJobs")
skills_vars <-c("BGTResId","skillId","skillName","SkillClusterName","SkillClusterFamilyName")
edu_vars <- c("BGTResID","degreeLevel","Instituition","InstitutionCity","InstitutionState","degreePosition","DegreeType","ipeds_unit_id","major","MajorCipCode","CompletionDateRaw")
job_vars <- c("BGTResId","consolidatedtitleonet","JobPosition","jobAddressState","jobAddressCity","jobAddressCounty","NAICS2","jobISOStartDate","jobISOEndDate")
certs_vars <- c("BGTResID","certificationName","certificationType","certificationposition")

b_main <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_personal.txt")
b_skill <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_skills.txt")
b_edu <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_edu.txt")
b_certs <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_certs.txt")
b_job <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_job.txt")

# Profiling the main table:
b_main <- b_main %>%
  select(main_vars)
b_main[b_main == "na"] <- NA
b_main[b_main == ""] <- NA

# Completeness of main table:
l <- length(b_main$BGTResId)
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(b_main)) {
  comp <- l - sum(is.na (b_main[[i]]))
  uni<-length(unique(b_main[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}


# Look at them specifically
a<- nchar(b_main$BGTResId) >= 1
b<- nchar(b_main$BGTResId) <= 20
