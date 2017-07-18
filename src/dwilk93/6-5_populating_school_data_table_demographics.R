library(dplyr)
library(stringr)
library(reshape2)

## get Virginia school data
setwd("~/Google Drive/2017 DSPG Program - shared folder/DSPG 2017 Projects/NSF NCSES STEM Education/Data for Team Education/")

data1<-read.csv("VDOE 2015_fm_divsch_1_69.csv",stringsAsFactors = F)
data2<-read.csv("VDOE 2015_fm_divsch_70_207.csv",stringsAsFactors = F)

data<-rbind(data1,data2)

## look at schools in Alexandria City, Fairfax County, Arlington County, Falls Church City, Fairfax City
data<-filter(data, DIV_NAME=="Alexandria City" | DIV_NAME=="Fairfax County" | DIV_NAME=="Arlington County" | DIV_NAME=="Falls Church City" |
               DIV_NAME=="Fairfax City")
## subset for high schools
hs<-unique(ifelse(str_detect(data$SCH_NAME,"High")==T,data$SCH_NAME,""))
grade9<-unique(ifelse(data$GRADE_CODE=="09",data$SCH_NAME,""))
all<-unique(append(hs,grade9))
all<-as.data.frame(all)

dataHS<-left_join(all,data,by=c("all"="SCH_NAME"))
dataHS<-rename(dataHS,school_name=all)
dataHS<-filter(dataHS,school_name!="")

## total students
total <- dataHS %>%
  filter(GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG=="" & is.na(FEDERAL_RACE_CODE) & GRADE_CODE=="")
total<-dplyr::select(total,school_name,FALL_MEMBERSHIP_CNT)

### descriptive statistics of virginia fall membership data
## count students by year, school, and race
race <- dataHS %>%
  filter(GENDER=="" & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG=="" & !is.na(FEDERAL_RACE_CODE) & GRADE_CODE!="")

race_counts <- group_by(race,BEG_SCH_YR,school_name,FEDERAL_RACE_CODE) %>%
  summarise(count=sum(FALL_MEMBERSHIP_CNT))

drace<-dcast(race_counts,BEG_SCH_YR+school_name~FEDERAL_RACE_CODE,value.var ="count")

## count students by year, school, and gender
gender <- dataHS %>%
  filter(is.na(FEDERAL_RACE_CODE) & DISABILITY_FLAG=="" & LEP_FLAG=="" & DISADVANTAGED_FLAG=="" & GENDER!=""  & GRADE_CODE!="")

gender_counts <- group_by(gender,BEG_SCH_YR,school_name,GENDER) %>%
  summarise(count=sum(FALL_MEMBERSHIP_CNT))

dgender<-dcast(gender_counts,BEG_SCH_YR+school_name~GENDER,value.var ="count")

## count students by year, school, and disadvantaged flag
disadv <- dataHS %>%
  filter(is.na(FEDERAL_RACE_CODE) & DISABILITY_FLAG=="" & LEP_FLAG=="" & GENDER=="" & DISADVANTAGED_FLAG!=""  & GRADE_CODE!="")

disadv_counts <- group_by(disadv,BEG_SCH_YR,school_name,DISADVANTAGED_FLAG) %>%
  summarise(count=sum(FALL_MEMBERSHIP_CNT))
disadv_counts<-filter(disadv_counts,DISADVANTAGED_FLAG=="Y")
disadv_counts<-rename(disadv_counts,isDisadv=count)
disadv_counts<-disadv_counts[-3]

## count students by year, school, and lep flag
lep <- dataHS %>%
  filter(is.na(FEDERAL_RACE_CODE) & DISABILITY_FLAG=="" & LEP_FLAG!="" & GENDER=="" & DISADVANTAGED_FLAG==""  & GRADE_CODE!="")

lep_counts <- group_by(lep,BEG_SCH_YR,school_name,LEP_FLAG) %>%
  summarise(count=sum(FALL_MEMBERSHIP_CNT))
lep_counts<-filter(lep_counts,LEP_FLAG=="Y")
lep_counts<-rename(lep_counts,inLEP=count)
lep_counts<-lep_counts[-3]

## combine
df<-left_join(drace,dgender,by=c("school_name","BEG_SCH_YR"))
df<-left_join(df,disadv_counts,by=c("school_name","BEG_SCH_YR"))
df<-left_join(df,lep_counts,by=c("school_name","BEG_SCH_YR"))
df<-left_join(df,total,by="school_name")

# all NAs to 0s
df[is.na(df)] <- 0

# race_code
# row.names federal_race_code                      federal_race_desc
# 1         1                 0                            Unspecified
# 2         2                 1          American Indian/Alaska Native
# 3         3                 2                                  Asian
# 4         4                 3              Black or African/American
# 5         5                 4                   Hispanic of any race
# 6         6                 5                                  White
# 7         7                 6 Native Hawaiian/Other Pacific Islander
# 8         8                99        Tow or more races, non-Hispanic

dataHSs<-rename(df,AmericanIndianAlaskaNative=`1`,Asian=`2`,BlackOrAfricanAmerican=`3`,Hispanic=`4`,White=`5`,NativeHawaiianOtherPacificIslander=`6`,
                TwoOrMoreRaces=`99`)
write.csv(dataHSs,"DataBySchool.csv")
