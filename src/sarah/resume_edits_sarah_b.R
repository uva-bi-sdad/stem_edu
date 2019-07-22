library(devtools)
library(dplyr)
library(tidyverse)
library(data.table)

edu <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_edu.txt")
main <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_personal.txt")
skill <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_skills.txt")

#This function determines bachelors designation by degreeLevel variable
ids <- main$BGTResId
maxLevel <- c()
for (i in 1:length(ids)) {
  a <- max(edu[edu$BGTResID == ids[i]]$degreeLevel)

  # Tries to fill in some of the NAs
  if (is.na(a)){
    maxLevel <- append(maxLevel,NA)
    next
  } else {
    maxLevel <- append(maxLevel, a)
  }
}

bachelor <- c()
for (i in 1:length(maxLevel)){
  if (is.na(maxLevel[i])) {
    bachelor <- append(bachelor,NA)
  } else if (maxLevel[i] > 14) {
    bachelor<- append(bachelor,1)
  } else if (maxLevel[i] == "") {
    bachelor <- append(bachelor,NA)
  } else {
    bachelor <- append(bachelor,0)
  }
}
main$bachelor <- bachelor

before <- main %>% group_by(bachelor) %>% summarise (count = n())

#now I separate the bachelor NA
main_na <- main %>% filter(is.na(bachelor)==TRUE)
edu_na <- edu %>% filter(BGTResID %in% main_na$BGTResId)


#check for VCCS
vccs <- read.csv("data/stem_edu/working/ipeds_vccs/ipeds_vccs_cred.csv")
vccs <-unique(vccs$College)
vccs <- tolower(vccs)
edu_na$inst_low <- tolower(edu_na$Instituition)

isVCCS <-c()
for (i in edu_na$inst_low[1:length(edu_na$BGTResID)]) {
  if (i %in% vccs) {
    isVCCS <- c(isVCCS,1)
    next
  } else {
    isVCCS <- c(isVCCS,0)
  }
}
edu_na$vccs <- isVCCS

#check for other (technical, beauty, etc)
isOther <- c()
for (i in edu_na$inst_low[1:length(edu_na$BGTResID)]) {
  if (grepl("technical school",i)) {
    isOther <- c(isOther,1)
    next
  } else if (grepl("technical institute",i)) {
    isOther <- c(isOther,1)
    next
  } else if (grepl("technical college",i)) {
    isOther <- c(isOther,1)
    next
  } else if (grepl("cosmetology",i)) {
    isOther <- c(isOther,1)
    next
  } else if (grepl("beauty",i)) {
    isOther <- c(isOther,1)
    next
  }else if (grepl("vocational",i)) {
    isOther <- c(isOther,1)
    next
  }else if (grepl("career",i)) {
    isOther<-c(isOther,1)
    next
  } else if (grepl("culinary",i)) {
    isOther<-c(isOther,1)
    next
  } else {
    isOther <- c(isOther,0)
  }
}
edu_na$other <- isOther

#check for high school
isHigh <-c()
for (i in 1:length(edu_na$BGTResID)) {
  if (grepl("high school", edu_na$inst_low[i])) {
    isHigh <- c(isHigh,1)
    next
  } else if (grepl("highschool", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("secondary school teachers", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 0)
    next
  } else if (grepl("secondary school", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("fork union military academy", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("frederick military academy", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("hargrave military academy", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("fishburne military school", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("franklin military academy", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("massanutten military academy", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("randolph-macon academy", edu_na$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("GED", edu_na$degreeLevel[i], ignore.case = TRUE)){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("GED", edu_na$major[i], ignore.case = TRUE)){
    isHigh <- c(isHigh, 1)
    next
  } else {
    isHigh <- c(isHigh,0)
  }
}
edu_na$high <- isHigh


#check for partial college
isPartial <-c()
for (i in 1:length(edu_na$BGTResID)) {
  if (grepl("some college", edu_na$inst_low[i])) {
    isPartial <- c(isPartial,1)
    next
  } else if (grepl("college courses", edu_na$inst_low[i])) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("progress", edu_na$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("incomplete", edu_na$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("unearned degree", edu_na$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("no degree", edu_na$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("non degree", edu_na$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("non-degree", edu_na$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else {
    isPartial <- c(isPartial,0)
  }
}
edu_na$partial <- isPartial

#check for associates designation
isAssociates <- c()
for(i in 1:length(edu_na$BGTResID)) {
  if(grepl("associates", edu_na$DegreeType[i], ignore.case = TRUE)) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("assoc.", edu_na$DegreeType[i], ignore.case = TRUE)) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("ass", edu_na$DegreeType[i], ignore.case = TRUE)) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AS$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AA$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AE$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AN$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AF$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AT$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AAA$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AAB$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AS$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^ABA$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AAT$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AAS$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^ABS$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^ADN$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AES$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AET$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AFA$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AGS$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AIT$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AOS$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^APE$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^APS$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^ASPT$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^APT$", edu_na$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else {
    isAssociates <- c(isAssociates, 0)
  }
}
edu_na$associates <- isAssociates

#if VCCS, other, high school, partial, or associates is found, bach equals 0
isbach <-c()
for (i in 1:length(edu_na$BGTResID)) {
  if (edu_na$vccs[i] == 1) {
    isbach <- c(isbach, 0)
    next
  } else if (edu_na$other[i] == 1) {
    isbach <- c(isbach, 0)
    next
  } else if (edu_na$high[i] == 1) {
    isbach <- c(isbach, 0)
    next
  } else if (edu_na$partial[i] == 1) {
    isbach <- c(isbach, 0)
    next
  } else if (edu_na$associates[i] == 1) {
    isbach <- c(isbach, 0)
    next
  } else {
    isbach <- c(isbach, NA)
  }
}
edu_na$bach <- isbach

#change NA to "" so that taking the maximum works
edu_na$bach[is.na(edu_na$bach) == TRUE] <- ""

#take the maximum for each ID
maxLevel2 <- edu_na %>% group_by(BGTResID) %>% summarise(max = max(bach))
colnames(maxLevel2)[colnames(maxLevel2)=="BGTResID"] <- "BGTResId"


#take this back to main_na table
main_na <- merge(main_na, maxLevel2, by="BGTResId", all= TRUE)

#get rid of old bachelor column, and make max the new bachelor column
main_na <- main_na[ , -which(names(main_na) %in% c("bachelor"))]
colnames(main_na)[colnames(main_na)=="max"] <- "bachelor"

#drop rows where bachelor= NA in original main table
main  <- main %>% drop_na(bachelor)

main <- rbind(main, main_na)
main$bachelor[main$bachelor == ""] <- NA

after <- main %>% group_by(bachelor) %>% summarise(count = n())

#write.csv(main, "data/stem_edu/working/resume_with_bachelors/resume_with_bachelors_b_main")

skill$bachelor = main$bachelor[match(skill$BGTResId, main$BGTResId)]

#write.csv(skill, "data/stem_edu/working/resume_with_bachelors/resume_with_bachelors_b_skill")

