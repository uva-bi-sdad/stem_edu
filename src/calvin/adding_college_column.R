# The resume data has really messy education information
# much is missing, and many things have #s in fields representing multiple entries
# The goal of this document is to determine whether an individual has a college degree

library(devtools)
library(dplyr)
library(tidyverse)
library(data.table)

edu <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_edu.txt")
main <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_personal.txt")

ids <- main$BGTResId

maxLevel <- c()
for (i in 1:length(ids)) {
  a <- max(edu[edu$BGTResID == ids[i]]$degreeLevel)

  # Tries to fill in some of the NAs
  if (is.na(a)){
    maxLevel <- append(maxLevel,NA)
    next
  } else if (a == "") {
    college <- FALSE
    high <- FALSE
    for (m in grepl("igh", edu[edu$BGTResID == ids[i]]$Instituition)) {
      if (m){
        high <- TRUE
      }
    }
    for (m in grepl("IGH", edu[edu$BGTResID == ids[i]]$Instituition)) {
      if (m){
        high <- TRUE
      }
    }
    for (m in grepl("niversity", edu[edu$BGTResID == ids[i]]$Instituition)) {
      if (m){
        college <- TRUE
      }
    }
    for (m in grepl("ollege", edu[edu$BGTResID == ids[i]]$Instituition)) {
      if (m){
        college <- TRUE
      }
    }
    if (high && !college) {
      maxLevel <- append(maxLevel,"12")
      next
    }
  }

  maxLevel <- append(maxLevel,a)
}

college <- c()
for (i in 1:length(maxLevel)){
  if (is.na(maxLevel[i])) {
    college <- append(college,"NOT IN EDUCATION")
  } else if (maxLevel[i] > "14") {
    college<- append(college,1)
  } else if (maxLevel[i] == "") {
    college <- append(college,NA)
  } else {
    college <- append(college,0)
  }
}


main$college <- college
