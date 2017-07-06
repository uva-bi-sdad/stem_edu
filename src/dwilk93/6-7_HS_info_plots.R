library(plyr)
library(stringr)
library(reshape2)
library(ggplot2)


###School Demographics

#Uploading File
SchoolDemographics<-read.csv("~/git/stem_edu/data/stem_edu/original/HS_info/2015-2016 School Demographics.csv",stringsAsFactors = F)

#DataFrame manipulation
SchoolDemographics <- rename(SchoolDemographics, c("school_name"="School Name", "BEG_SCH_YR"="School Year", "F"="Female", "M"="Male",
                                                   "FALL_MEMBERSHIP_CNT" = "Total Enrollment", "BlackOrAfricanAmerican"="Black or African-American",
                                                   "AmericanIndianAlaskaNative"="American Indian or Alaskan Native", "TwoOrMoreRaces" = "Two+ Races",
                                                   "NativeHawaiianOtherPacificIslander"="Native Hawaiian and other Pacific Islander",
                                                   "isDisadv"="Disadvantaged", "inLEP"="Limited English Proficiency"))
SchoolDemographics <- SchoolDemographics[,c(1,2,14,11,10,7,5,4,6,3,8,9,12,13)]

#Saving manipulated Dataframes
write.csv(SchoolDemographics,"~/git/stem_edu/data/stem_edu/original/HS_info/15-16_School_Demographics.csv")



###Accreditation Reports
AccreditationReport<-read.csv("~/git/stem_edu/data/stem_edu/original/HS_info/2015-2016 Accreditation Report.csv",stringsAsFactors = F)
#Dataframe manipulation
AccreditationReport <- AccreditationReport[-c(1:3,6,7,10,11,14,15)]
AccreditationReport <- AccreditationReport[-c(3:1852),]
AccreditationReport <- rename(AccreditationReport, c("School.Name"="School","School.Accreditation.Rating"="Accreditation Rating",
                                                     "Mathematics"="Math Pass Rate", "Science"="Science Pass Rate",
                                                     "Met.Mathematics"= "Met Math Accreditation", "Met.Science" = "Met Science Accreditation"))
#Saving new dataframe
write.csv(AccreditationReport,"~/git/stem_edu/data/stem_edu/original/HS_info/15-16_Accreditation_Report.csv")



###SOL Scores
SchoolSOLPassRate<-read.csv("~/git/stem_edu/data/stem_edu/original/HS_info/2015-2016 SOL Pass Rate.csv",stringsAsFactors = F)
StateSOLPassRate<-read.csv("~/git/stem_edu/data/stem_edu/original/HS_info/2015-2016 State SOL Pass Rate.csv",stringsAsFactors = F)


#Manipulating School Dataframes
SchoolSOLPassRate <- SchoolSOLPassRate[-c(1:4,6:8,14:16)]
SchoolSOLPassRate <- SchoolSOLPassRate[-c(2:7,14:20),]
SchoolSOLPassRate <- SchoolSOLPassRate[c(1,2,4,3,5,6,7,9,8,10,11,12,13),]


#Manipulating State Dataframe
StateSOLPassRate <- StateSOLPassRate[-c(1,7:9)]
StateSOLPassRate <- StateSOLPassRate[-c(2:28,32:34),]
StateSOLPassRate <- StateSOLPassRate[c(1,3,4,2,6,7,5),]







