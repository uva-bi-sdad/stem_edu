library(tidyverse)
library(plyr)
library(data.table)

dt <-read.csv("data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2017.csv")

ipeds_va_2017 <- filter(dt, STABBR == "VA")
ipeds_va_2017 <-ipeds_va_2017[ -c(3, 10:65) ]
colnames(ipeds_va_2017)[colnames(ipeds_va_2017)=="INSTNM"] <- "College"

vccs_cred <- read.csv("data/stem_edu/original/VCCS/vccs_cred.csv")

comm_col <- c("Community College")
vccs_cred$new.col <- comm_col

vccs_cred$College <- paste(vccs_cred$College,vccs_cred$new.col)

vccs_cred <- vccs_cred[,-7]

levels(ipeds_va_2017$College)[levels(ipeds_va_2017$College)=="Dabney S Lancaster Community College"] <- "Dabney S. Lancaster Community College"
levels(ipeds_va_2017$College)[levels(ipeds_va_2017$College)=="J Sargeant Reynolds Community College"] <- "J. Sargeant Reynolds Community College"
levels(ipeds_va_2017$College)[levels(ipeds_va_2017$College)=="Paul D Camp Community College"] <- "Paul D. Camp Community College"


ipeds_vccs_cred <- merge(ipeds_va_2017,vccs_cred,by=c("College"))

unique(vccs_cred$College[!(vccs_cred$College %in% ipeds_vccs_cred$College)])

#write.csv(ipeds_vccs_cred, "data/stem_edu/working/ipeds_vccs/ipeds_vccs_cred.csv")

