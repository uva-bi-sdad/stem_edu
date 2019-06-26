library(tidyverse)
library(plyr)

dt <-read.csv("data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2017.csv")

ipeds_va_2017 <- filter(dt, STABBR == "VA")
ipeds_va_2017 <-ipeds_va_2017[ -c(3, 10:65) ]
colnames(ipeds_va_2017)[colnames(ipeds_va_2017)=="INSTNM"] <- "institution"

#ipeds_va_2017$institution <- as.character(ipeds_va_2017$institution)

#Naming Consistency
levels(ipeds_va_2017$institution)[levels(ipeds_va_2017$institution)=="Virginia Polytechnic Institute and State University"] <- "Virginia Tech"

levels(ipeds_va_2017$institution)[levels(ipeds_va_2017$institution)=="University of Virginia-Main Campus"] <- "University of Virginia"

levels(ipeds_va_2017$institution)[levels(ipeds_va_2017$institution)=="Dabney S Lancaster Community College"] <- "Dabney S. Lancaster Community College"

levels(ipeds_va_2017$institution)[levels(ipeds_va_2017$institution)=="Richard Bland College of William and Mary"] <- "Richard Bland College"

levels(ipeds_va_2017$institution)[levels(ipeds_va_2017$institution)=="Averett University-Non-Traditional Programs"] <- "Averett University Non-Traditional"

###schev_stw_subbach_combine

schev_stw_subbach_combine <- read.csv("data/stem_edu/working/schevData/schev_stw_subbach_combine.csv")

ipeds_schev_stw_subbach_combine <- merge(ipeds_va_2017,schev_stw_subbach_combine,by=c("institution"))

#check to ensure names match
unique(schev_stw_subbach_combine$institution[!(schev_stw_subbach_combine$institution %in% ipeds_schev_stw_subbach_combine$institution)])

#write.csv(ipeds_schev_stw_subbach_combine, "data/stem_edu/working/ipeds_schev/ipeds_schev_stw_subbach_combine.csv")


###schev_stw_schools

schev_stw_schools <- read.csv("data/stem_edu/working/schevData/schev_stw_schools.csv")

ipeds_schev_stw_schools <- merge(ipeds_va_2017,schev_stw_schools,by=c("institution"))

#check to ensure names match
unique(schev_stw_schools$institution[!(schev_stw_schools$institution %in% ipeds_schev_stw_schools$institution)])

#write.csv(ipeds_schev_stw_schools, "data/stem_edu/working/ipeds_schev/ipeds_schev_stw_schools.csv")

###schev_subbach_combine

schev_subbach_combine <- read.csv("data/stem_edu/working/schevData/schev_subbach_combine.csv")

levels(ipeds_va_2017$institution)[levels(ipeds_va_2017$institution)=="Lynchburg College"] <- "University of Lynchburg"

ipeds_schev_subbach_combine <- merge(ipeds_va_2017,schev_subbach_combine,by=c("institution"))

#check to ensure names match
unique(schev_subbach_combine$institution[!(schev_subbach_combine$institution %in% ipeds_schev_subbach_combine$institution)])

#Virginia Intermont College Closed in 2014, so it is not relevant to 2017
#Christendom College is not in the IPEDS data
#
christendom <- c("Christendom College", NA, "134 Christendom Dr", "Front Royal", "VA", "22630-6534", "51", "5", )

#write.csv(ipeds_schev_stw_schools, "data/stem_edu/working/ipeds_schev/ipeds_schev_stw_schools.csv")
