library(data.table)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(tidyverse)
library(DescTools)

b_certs <- fread("data/stem_edu/working/MSA_Resumes/blacksburg_certs.txt")
b_edu <- fread("data/stem_edu/working/MSA_Resumes/blacksburg_edu.txt")
b_job <- fread("data/stem_edu/working/MSA_Resumes/blacksburg_job.txt")
b_personal <- fread("data/stem_edu/working/MSA_Resumes/blacksburg_personal.txt")
b_skills <- fread("data/stem_edu/working/MSA_Resumes/blacksburg_skills.txt")

#count NA values for degreeLevel (it looks like all missing values are blank rather than NA, but I wanted to check)
sum(is.na(b_edu$degreeLevel))

#split observations that list multiple degree levels
b_edu <-cSplit(b_edu, "degreeLevel", "#")

#I just want the ID and degree levels
b_edu_2 <- b_edu %>%
  select(BGTResID, degreeLevel_1, degreeLevel_2, degreeLevel_3, degreeLevel_4, degreeLevel_5, degreeLevel_6, degreeLevel_7)

#aggregate. if there are multiple values collapsing on each other, take the max (this is common for people who attained multiple Bachelor's degrees)
b_edu_2 = aggregate(b_edu_2,
  by = list(b_edu_2$BGTResID),
  FUN=max,
  na.rm=TRUE)
b_edu_2 <- b_edu_2[-1]

#take max degree level
b_edu_2$max = apply(b_edu_2[2:8], 1, max, na.rm=TRUE)

#if max equals -Inf, change to blank (as before)
b_edu_2$max <- ifelse(b_edu_2$max == -Inf, "", b_edu_2$max)
b_edu_2<- b_edu_2[c(1,9)]

#rename b_personal to match b_edu_2
colnames(b_personal)[colnames(b_personal)=="BGTResId"] <- "BGTResID"

#merge b_edu_2 and b_personal
b_personal <- merge(b_personal, b_edu_2, by="BGTResID", all.x=TRUE)

#personal has three additional unqiue job ids than edu, edu_2
length(unique(b_personal$BGTResId))
length(unique(b_edu$BGTResID))

#remove duplicates
filter(b_personal, duplicated(b_personal$BGTResID)==TRUE)
b_personal <- b_personal[-c(10112, 10121, 10123, 9952, 8489),]
filter(b_personal, duplicated(b_personal$BGTResID)==TRUE)

b_personal$max = b_personal$max %>%
  as.numeric()
b_personal$hasbach = ifelse(b_personal$max > 14, TRUE, FALSE)

b_personal %>% group_by(hasbach)%>% summarise(count= n())
#there are 2567 "NA"

na <- filter(b_personal, is.na(b_personal$hasbach)==TRUE)

#back to edu table
na_edu <- b_edu %>% filter(BGTResID %in% na$BGTResID)

na_comm <- na_edu %>% filter(Instituition %like any% c("%ommunity%", "%OMMUNITY%"))
na_comm <- na_comm %>% cSplit("Instituition", "#")

na_hs <- na_edu %>% filter(Instituition %like any% c("%igh school%", "%igh School%", "%HIGH SCHOOL%", "%Highschool%"))
na_hs <- na_hs %>% cSplit("Instituition", "#")

na_col <- na_edu %>% filter(!(V1 %in% na_comm$V1) & !(V1 %in% na_hs$V1))
na_col <- na_col %>% cSplit("Instituition", "#")




#VCCS
vccs <- read.csv("data/stem_edu/working/ipeds_vccs/ipeds_vccs_cred.csv")
vccs<-vccs[2]
vccs<-unique(vccs)

