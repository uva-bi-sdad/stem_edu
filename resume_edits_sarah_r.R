library(devtools)
library(dplyr)
library(tidyverse)
library(data.table)

edu <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_edu.txt")
main <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_personal.txt")
skill <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/richmond_skills.txt")

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
  } else if (grepl("j sargeant reynolds community college",i), ignore.cases= TRUE) {
    isOther <- c(isOther,1)
    next
  } else if (grepl("j.sargeant reynolds community college",i), ignore.cases= TRUE) {
    isOther <- c(isOther,1)
    next
  } else {
    isVCCS <- c(isVCCS,0)
  }
}
edu_na$vccs <- isVCCS

