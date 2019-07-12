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

r_main <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_personal.txt")
r_skill <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_skills.txt")
r_edu <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_edu.txt")
r_certs <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_certs.txt")
r_job <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_job.txt")


# Profiling the main table:
b_main <- b_main %>%
  select(main_vars)
# replace "na" and "" with NA
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

# Completeness of main table:
r_main[r_main == "na"] <- NA
r_main[r_main == ""] <- NA
l <- length(r_main$BGTResId)
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(r_main)) {
  comp <- l - sum(is.na (r_main[[i]]))
  uni<-length(unique(r_main[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}


# Basic code to check that a column contains characters of specific length. I had to
# do it by hand because r is being weird.

m<- 0
nas<- 0
bad <- 0
strLengthValidation <- function(min, max, n, column) {
  m<- 0
  nas<- 0
  bad <- 0
  a<- nchar(column) >= min
  b<- nchar(column) <= max
  for (i in 1:n) {
    if (is.na(a[i])) {
      nas <- nas + 1
      next
    } else if (a[i] && b[i]) {
      m <- m + 1
    } else {
      bad <- bad + 1
      next
    }
  }
  print(paste("Good: ",m,"Na:",nas,"bad:",bad))
}
strLengthValidation(1,250,91916,r_main$cityName)


# Calculating state:
checkState <- b_main$StateName[b_main$StateName == "Virginia"]
na.rem


intLengthValidation <- function(min, max,n,column) {
  m<- 0
  nas<- 0
  bad <- 0
  a<- column >= min
  b<- column <= max
  for (i in 1:n) {
    if (is.na(a[i])) {
      nas <- nas + 1
      next
    } else if (a[i] && b[i]) {
      m <- m + 1
    } else {
      bad <- bad + 1
      next
    }
  }
  print(paste("Good: ",m,"Na:",nas,"bad:",bad))
}
intLengthValidation(1,10)






# Skills
b_skill[b_skill == "na"] <- NA
b_skill[b_skill == ""] <- NA
l <- length(b_skill$BGTResId)
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(b_skill)) {
  comp <- l - sum(is.na (b_skill[[i]]))
  uni<-length(unique(b_skill[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}


r_skill[r_skill == "na"] <- NA
r_skill[r_skill == ""] <- NA
l <- length(r_skill$BGTResId)
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(r_skill)) {
  comp <- l - sum(is.na (r_skill[[i]]))
  uni<-length(unique(r_skill[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}

# Education table
b_edu[b_edu == "na"] <- NA
b_edu[b_edu == ""] <- NA
l <- length(b_edu$BGTResID)
l
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(b_edu)) {
  comp <- l - sum(is.na (b_edu[[i]]))
  uni<-length(unique(b_edu[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}


# Education table
r_edu[r_edu == "na"] <- NA
r_edu[r_edu == ""] <- NA
l <- length(r_edu$BGTResID)
l
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(r_edu)) {
  comp <- l - sum(is.na (r_edu[[i]]))
  uni<-length(unique(r_edu[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}



# Jobs table
b_job[b_job == "na"] <- NA
b_job[b_job == ""] <- NA
l <- length(b_job$BGTResId)
l
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(b_job)) {
  comp <- l - sum(is.na (b_job[[i]]))
  uni<-length(unique(b_job[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}


r_job[r_job == "na"] <- NA
r_job[r_job == ""] <- NA
l <- length(r_job$BGTResId)
l
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(r_job)) {
  comp <- l - sum(is.na (r_job[[i]]))
  uni<-length(unique(r_job[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}

# Certs
b_certs[b_certs == "na"] <- NA
b_certs[b_certs == ""] <- NA
l <- length(b_certs$BGTResID)
l
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(b_certs)) {
  comp <- l - sum(is.na (b_certs[[i]]))
  uni<-length(unique(b_certs[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}


r_certs[r_certs == "na"] <- NA
r_certs[r_certs == ""] <- NA
l <- length(r_certs$BGTResID)
l
print("Completeness for each MSA: ")
print("Completeness for Blacksburg: ")
for (i in names(r_certs)) {
  comp <- l - sum(is.na (r_certs[[i]]))
  uni<-length(unique(r_certs[[i]]))
  print(paste(i, comp,round(comp / l * 100,2),"Still figuring out validity",uni))
}


# Validating skills:
strLengthValidation(1,20,1754999,r_skill$BGTResId)
strLengthValidation(1,20,207277,b_skill$BGTResId)
intLengthValidation(0,19000,1754999,r_skill$skillId)
intLengthValidation(0,19000,207277,b_skill$skillId)
strLengthValidation(1,100,1754999,r_skill$skillName)
strLengthValidation(1,100,207277,b_skill$skillName)
strLengthValidation(1,100,1754999,r_skill$SkillClusterName)
strLengthValidation(1,100,207277,b_skill$SkillClusterName)
strLengthValidation(1,100,1754999,r_skill$SkillClusterFamilyName)
strLengthValidation(1,100,207277,b_skill$SkillClusterFamilyName)

howManyChars <- function(column,symbol) {
  m <- 0
  ug <- grepl(symbol,column)
  for (i in ug){
    if (i){
          m <- m+1
      }
  }
  print(m)
}

# Validate Educational information
strLengthValidation(1,20,149975,r_edu$BGTResID)
strLengthValidation(1,20,17515,b_edu$BGTResID)
strLengthValidation(1,150,149975,r_edu$InstitutionCity)
strLengthValidation(1,250,17515,b_edu$InstitutionCity)
howManyChars(b_edu$InstitutionCity,"#")
howManyChars(r_edu$InstitutionCity,"#")
strLengthValidation(2,15,149975,r_edu$InstitutionState)
strLengthValidation(2,15,17515,b_edu$InstitutionState)
howManyChars(b_edu$InstitutionState,"#")
howManyChars(r_edu$InstitutionCity,"#")
intLengthValidation(1,50,149975,r_edu$degreePosition)
intLengthValidation(1,50,17515,b_edu$degreePosition)
intLengthValidation(0,24,149975,r_edu$degreeLevel)
intLengthValidation(0,24,17515,b_edu$degreeLevel)
strLengthValidation(1,40,149975,r_edu$DegreeType)
strLengthValidation(1,40,17515,b_edu$DegreeType)
howManyChars(r_edu$DegreeType,"#")
howManyChars(b_edu$DegreeType,"#")
strLengthValidation(1,40,149975,r_edu$MajorCipCode)
strLengthValidation(1,40,17515,b_edu$MajorCipCode)
howManyChars(r_edu$MajorCipCode,"#")
howManyChars(b_edu$MajorCipCode,"#")
strLengthValidation(1,140,149975,r_edu$Instituition)
strLengthValidation(1,140,17515,b_edu$Instituition)
howManyChars(r_edu$Instituition,"#")
howManyChars(b_edu$Instituition,"#")
strLengthValidation(1,140,149975,r_edu$CompletionDateRaw)
strLengthValidation(1,140,17515,b_edu$CompletionDateRaw)
howManyChars(r_edu$CompletionDateRaw,"#")
howManyChars(b_edu$CompletionDateRaw,"#")

# Validate Jobs information
strLengthValidation(1,20,528275,r_job$BGTResId)
strLengthValidation(1,20,50791,b_job$BGTResId)
howManyChars(r_job$BGTResId,"#")
howManyChars(b_job$BGTResId,"#")
strLengthValidation(1,20,528275,r_job$JobPosition)
strLengthValidation(1,20,50791,b_job$JobPosition)
howManyChars(r_job$JobPosition,"#")
howManyChars(b_job$JobPosition,"#")
strLengthValidation(1,20,528275,r_job$consolidatedtitleonet)
strLengthValidation(1,20,50791,b_job$consolidatedtitleonet)
howManyChars(r_job$consolidatedtitleonet,"#")
howManyChars(b_job$consolidatedtitleonet,"#")
strLengthValidation(1,20,528275,r_job$NAICS2)
strLengthValidation(1,20,50791,b_job$NAICS2)
howManyChars(r_job$NAICS2,"-")
howManyChars(b_job$NAICS2,"-")
strLengthValidation(1,20,528275,r_job$jobISOStartDate)
strLengthValidation(1,20,50791,b_job$jobISOStartDate)
howManyChars(r_job$jobISOStartDate,"#")
howManyChars(b_job$jobISOStartDate,"#")
strLengthValidation(1,20,528275,r_job$jobISOEndDate)
strLengthValidation(1,20,50791,b_job$jobISOEndDate)
howManyChars(r_job$jobISOEndDate,"#")
howManyChars(b_job$jobISOEndDate,"#")


# Validate Certification information
strLengthValidation(1,20,46174,r_certs$BGTResID)
strLengthValidation(1,20,3478,b_certs$BGTResID)
howManyChars(r_certs$BGTResID,"#")
howManyChars(b_certs$BGTResID,"#")
strLengthValidation(1,100,46174,r_certs$certificationName)
strLengthValidation(1,100,3478,b_certs$certificationName)
howManyChars(r_certs$certificationName,"#")
howManyChars(b_certs$certificationName,"#")
strLengthValidation(1,100,46174,r_certs$certificationType)
strLengthValidation(1,100,3478,b_certs$certificationType)
howManyChars(r_certs$certificationType,"#")
howManyChars(b_certs$certificationType,"#")
strLengthValidation(1,100,46174,r_certs$certificationposition)
strLengthValidation(1,100,3478,b_certs$certificationposition)
howManyChars(r_certs$certificationposition,"#")
howManyChars(b_certs$certificationposition,"#")
