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
SchoolSOLPassRate <- SchoolSOLPassRate[-c(1:4,6:8,14:16)] #delete columns
SchoolSOLPassRate <- SchoolSOLPassRate[-c(1:6,13:19),] #delete rows
SchoolSOLPassRate <- SchoolSOLPassRate[c(1,3,2,4:6,8,7,9,10:12),] #reorder columns
SchoolSOLPassRate <- rename(SchoolSOLPassRate, c("School.Name"="School", "X2013.14.Pass.Rate"="Pass_Rate_13_14", "X2014.15.Pass.Rate"="Pass_Rate_14_15","X2015.16.Pass.Rate"="Pass_Rate_15_16"))


#Manipulating State Dataframe
StateSOLPassRate <- StateSOLPassRate[-c(1,7:9)] #delete columns
StateSOLPassRate <- cbind("School" = "State", StateSOLPassRate)
StateSOLPassRate <- StateSOLPassRate[-c(1:27,31:33),] #delete rows
StateSOLPassRate <- StateSOLPassRate[c(2,3,1,5,6,4),] #reorder columns
StateSOLPassRate <- rename(StateSOLPassRate, c("X2013.14.Pass.Rate"="Pass_Rate_13_14", "X2014.15.Pass.Rate"="Pass_Rate_14_15","X2015.16.Pass.Rate"="Pass_Rate_15_16"))

#Save State and School SOL Score Dataframes
write.csv(SchoolSOLPassRate,"~/git/stem_edu/data/stem_edu/original/HS_info/15-16_School_SOL_Pass_Rate.csv")
write.csv(StateSOLPassRate,"~/git/stem_edu/data/stem_edu/original/HS_info/15-16_State_SOL_Pass_Rate.csv")

#binding School & State
SchoolandStatebind <-rbind(SchoolSOLPassRate,StateSOLPassRate)

###Plotting

##SOL SCORES

#Algebra 1 (15-16)
AlgebraI <- SchoolandStatebind[-c(2:6,8:12,14:19),]
ggplot(data = AlgebraI, aes(x = School, y = Pass_Rate_15_16)) + geom_bar(stat = "identity") + ylab("15-16 Pass Rate (%)") + ggtitle("Algebra 1 Pass Rates")

#Biology (15-16)




