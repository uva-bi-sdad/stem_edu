# investigating nces data
# makes dictionary of variable names and definitions

load("data/stem_edu/original/NCES/els_02_12_byf1sch_v1_0.rdata") # byf1sch (school)
load("data/stem_edu/original/NCES/els_02_12_byf3stu_v1_0.rdata") # byf3stu (student)
load("data/stem_edu/original/NCES/els_02_12_f2inst_v1_0.rdata") # f2inst (institution)
load("data/stem_edu/original/NCES/els_02_12_f3inst_v1_0.rdata") # f3inst (institution)

f1sch <- els_02_12_byf1sch_v1_0
f3stu <- els_02_12_byf3stu_v1_0
f2inst <- els_02_12_f2inst_v1_0
f3inst <- els_02_12_f3inst_v1_0

rm("els_02_12_byf1sch_v1_0","els_02_12_byf3stu_v1_0","els_02_12_f2inst_v1_0","els_02_12_f3inst_v1_0")

dim(f3stu)
unique(f3stu$G10COHRT)
unique(f3stu$F1RPHY_C) # suppressed variables contain -5

# remove suppressed variables:
f3stu[f3stu == -5] <- NA
f3stu <- f3stu[, colSums(is.na(f3stu)) != nrow(f3stu)]

completeness <- function(x){
  na <- sum(is.na(x))
  total <- length(x)
  percent <- (total - na)/total
  print(round(percent*100, 4))
}

library(stringr)

# TABLE PROFILES
byf3stu_profile <- as.data.frame(names(f3stu))
dict <- read.csv("data/stem_edu/original/NCES/byf3_dictionary.csv", header = FALSE)
byf3stu_profile <- merge(byf3stu_profile, dict, by.x = "names(f3stu)", by.y = "V1", all.x = TRUE)



