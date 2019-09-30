# The resume data has really messy education information
# much is missing, and many things have #s in fields representing multiple entries
# The goal of this document is to determine whether an individual has a college degree

library(devtools)
library(dplyr)
library(tidyverse)
library(data.table)

edu <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_edu.txt")
main <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_personal.txt")

####################

edu$inst_low <- tolower(edu$Instituition)

isCommunity <-c()
for (i in edu$inst_low[1:length(edu$BGTResID)]) {
  if (grepl("community",i)) {
    isCommunity <- c(isCommunity,1)
    next
  } else {
    isCommunity <- c(isCommunity,0)
  }
}

edu$community <- isCommunity

isOther <- c()
for (i in edu$inst_low[1:length(edu$BGTResID)]) {
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

edu$other <- isOther

isHigh <-c()
for (i in 1:length(edu$BGTResID)) {
  if (grepl("high school", edu$inst_low[i])) {
    isHigh <- c(isHigh,1)
    next
  } else if (grepl("highschool", edu$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("secondary school teachers", edu$inst_low[i])){
    isHigh <- c(isHigh, 0)
    next
  } else if (grepl("secondary school", edu$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("fork union military academy", edu$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("frederick military academy", edu$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("hargrave military academy", edu$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("fishburne military school", edu$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("franklin military academy", edu$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("massanutten military academy", edu$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("randolph-macon academy", edu$inst_low[i])){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("GED", edu$degreeLevel[i], ignore.case = TRUE)){
    isHigh <- c(isHigh, 1)
    next
  } else if (grepl("GED", edu$major[i], ignore.case = TRUE)){
    isHigh <- c(isHigh, 1)
    next
  }else {
    isHigh <- c(isHigh,0)
  }
}

edu$high <- isHigh

vccs <- read.csv("data/stem_edu/working/ipeds_vccs/ipeds_vccs_cred.csv")
vccs <-unique(vccs$College)
vccs <- tolower(vccs)

isVCCS <-c()
for (i in edu$inst_low[1:length(edu$BGTResID)]) {
  if (i %in% vccs) {
    isVCCS <- c(isVCCS,1)
    next
  } else {
    isVCCS <- c(isVCCS,0)
  }
}

edu$vccs <- isVCCS

isPartial <-c()
for (i in 1:length(edu$BGTResID)) {
  if (grepl("some college", edu$inst_low[i])) {
    isPartial <- c(isPartial,1)
    next
  } else if (grepl("college courses", edu$inst_low[i])) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("progress", edu$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("incomplete", edu$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("unearned degree", edu$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("no degree", edu$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("non degree", edu$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else if (grepl("non-degree", edu$major[i], ignore.case = TRUE)) {
    isPartial <- c(isPartial, 1)
    next
  } else {
    isPartial <- c(isPartial,0)
  }
}

edu$partial <- isPartial

isAssociates <- c()
for(i in 1:length(edu$BGTResID)) {
  if(grepl("associates", edu$DegreeType[i], ignore.case = TRUE)) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("assoc.", edu$DegreeType[i], ignore.case = TRUE)) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("ass", edu$DegreeType[i], ignore.case = TRUE)) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AS$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AA$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AE$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AN$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AF$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AT$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AAA$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AAB$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AS$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^ABA$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AAT$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AAS$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^ABS$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^ADN$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AES$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AET$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AFA$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^AGS$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AIT$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^AOS$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else if (grepl("^APE$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^APS$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^ASPT$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  }else if (grepl("^APT$", edu$DegreeType[i])) {
    isAssociates <- c(isAssociates, 1)
    next
  } else {
    isAssociates <- c(isAssociates, 0)
  }
}

edu$associates <- isAssociates

isUniversity <-c()
for (i in 1:length(edu$BGTResID)) {
  if (grepl("university", edu$inst_low[i])) {
    isUniversity <- c(isUniversity,1)
    next
  } else if (grepl("virginia tech", edu$inst_low[i]) & (edu$community[i] == 0)){
    isUniversity <- c(isUniversity, 1)
    next
  } else if (grepl("college", edu$inst_low[i]) & (edu$community[i] == 0)){
    isUniversity <- c(isUniversity, 1)
    next
  } else if (edu$degreeLevel[i]> 14){
    isUniversity <- c(isUniversity, 1) #does not separate by '#'
  } else {
    isUniversity <- c(isUniversity, 0)
  }
}

edu$university <- isUniversity



isBachelors <-c()
for (i in 1:length(edu$BGTResID)) {
  if ((edu$partial[i] == 1 | edu$other[i] == 1 |edu$high[i] == 1 | edu$vccs[i] == 1 | isAssociates == 1) & (edu$university[i]== 0)) {
    isBachelors <- c(isBachelors, 0)
    next
  } else if (edu$degreeLevel[i] > 14) {
    isBachelors <- c(isBachelors, 1)
    next
  } else {
    isBachelors <-c(isBachelors, NA)
  }
}

edu$bachelors <- isBachelors


###############################
ids <- main$BGTResId

maxLevel <- c()
for (i in 1:length(ids)) {
  a <- max(edu[edu$BGTResID == ids[i]]$bachelors, na.rm=TRUE)

  # Tries to fill in some of the NAs
  if (is.na(a)){
    maxLevel <- append(maxLevel,NA)
    next
  } else {
    maxLevel <- append(maxLevel,a)
  }
}
main$bachelors <- maxLevel
# NAs become -Inf
#

main$bachelors[main$bachelors== -Inf] <- NA

write.csv(main, "data/stem_edu/working/resume_with_bachelors/resume_with_bachelors_r")
