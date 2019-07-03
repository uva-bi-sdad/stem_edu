# Getting Access to the Data

# install.packages("RPostgreSQL")
require("RPostgreSQL")
library("RPostgreSQL")
library(DataExplorer)
library(devtools)
library(dplyr)
library(tidyverse)
library(data.table)


# Attempting the more direct method:
# DONT UNZIP unzip("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-01.zip")

what <- read.table(unz("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-01.zip", "Main_2016-01.txt"), fill = TRUE)
first16 <- what[what$V26 == "Virginia" | what$V26 == "State", ]

for (i in seq(1:8)+1){
  what <- read.table(unz(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-0",i,".zip", sep = ""), paste("Main_2016-0",i,".txt",sep="")), fill = TRUE)
  more16 <-  what[what$V26 == "Virginia" | what$V26 == "State", ]
  print(i)
  first16 <- merge(first16,more16, all=TRUE)
}
write.csv(first16, file = "firstnine2016.csv")

for (i in seq(1:3)+9){
  what <- read.table(unz(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-",i,".zip", sep = ""), paste("Main_2016-",i,".txt",sep="")), fill = TRUE)
  more16 <-  what[what$V26 == "Virginia" | what$V26 == "State", ]
  print(i)
  first16 <- merge(first16,more16, all=TRUE)
}
write.csv(first16, file = "firs_ten_2016.csv")
first_16 <- read.csv("../stem_edu/firstnine2016.csv")

what <- read.table(unz("stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-10.zip", "Main_2016-10.txt"), fill = TRUE)
first162 <- what[what$V26 == "Virginia" | what$V26 == "State", ]
what <- read.table(unz("stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-11.zip", "Main_2016-11.txt"), fill = TRUE)
more1621 <- what[what$V26 == "Virginia" | what$V26 == "State", ]
first162 <- merge(first162,more1621, all=TRUE)
what <- read.table(unz("stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-12.zip", "Main_2016-12.txt"), fill = TRUE)
more1622 <- what[what$V26 == "Virginia" | what$V26 == "State", ]
first162 <- merge(first162,more1622, all=TRUE)

first_nine <- read.csv("stem_edu/firstnine2016.csv")
main_2016 <- merge(first_nine,first162, all=TRUE)

write.csv(main_2016, file = "job_ads_main_2016.csv")

main <- read.csv("stem_edu/data/stem_edu/working/job_ads_2016/job_ads_main_2016.csv")
virginia_bgtjobid <- main$V1
virginia_bgtjobid <- as.character(virginia_bgtjobid)




# NEXT WRITE A LOOP TO GET THE SKILLS AND WRITE IT TO A FILE
all_skills <- read.table(unz("stem_edu/data/stem_edu/original/Burning_Glass_Data/Skill/2016/Skills_2016-01.zip", "Skills_2016-01.txt"), fill = TRUE)
all_skills$V1 <- as.character(all_skills$V1)
all_skills <- as.data.table(all_skills)
virginia_skills <- all_skills[all_skills$V1 %chin% virginia_bgtjobid]

#virginia_skills <- what[what$V26 == "Virginia" | what$V26 == "State", ]
for (i in seq(1:8)+1){
  all_skills <- read.table(unz(paste("stem_edu/data/stem_edu/original/Burning_Glass_Data/Skill/2016/Skills_2016-0",i,".zip", sep = ""), paste("Skills_2016-0",i,".txt",sep="")), fill = TRUE)
  all_skills$V1 <- as.character(all_skills$V1)
  all_skills <- as.data.table(all_skills)
  print(i)
  more16 <- all_skills[all_skills$V1 %chin% virginia_bgtjobid]
  virginia_skills <- merge(virginia_skills,more16, all=TRUE)
}
for (i in seq(1:3)+9){
  all_skills <- read.table(unz(paste("stem_edu/data/stem_edu/original/Burning_Glass_Data/Skill/2016/Skills_2016-",i,".zip", sep = ""), paste("Skills_2016-",i,".txt",sep="")), fill = TRUE)
  all_skills$V1 <- as.character(all_skills$V1)
  all_skills <- as.data.table(all_skills)
  print(i)
  more16 <- all_skills[all_skills$V1 %chin% virginia_bgtjobid]
  print(i)
  virginia_skills <- merge(virginia_skills,more16, all=TRUE)
}
write.csv(virginia_skills, file = "job_ads_skills_2016.csv")








#===================== OLD THINGS I TRIED =====================#

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it. Everyone should have a DB username and password, so just use yours here
pw <- "This is my Data UVA password."


# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
library(DBI)


library("RPostgreSQL")



con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "burning_glass",
                      host = "postgis",
                      port = "5432",
                      user = "",#Your username here
                      password ="")#Your password here


# check for the cartable
dbExistsTable(con, "ads_main_2016")

# check for the cartable
dbExistsTable(con, "ads_main_2016")
dbExistsTable(con, "ads_skills_2016")
# should give you TRUE

job_ads_2016_main <- dbGetQuery(con, "select * from ads_main_2016")
job_ads_2016_skill <- dbGetQuery(con, "select * from ads_skills_2016")

write.csv(job_ads_2016_main, file = "job_ads_2016_main.csv")
write.csv(job_ads_2016_skill, file = "job_ads_2016_skill.csv")

# Whenever you're done
dbDisconnect(con)

getData <- function(x) {
  peep <- fread("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-01.zip", data.table = TRUE)
  for (i in (seq(2:9)+1)){
    peep<- merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-0",i,".zip",sep=""), data.table = TRUE), all=TRUE)
  }
  for (i in (seq(10:x)+9)){
    peep<- merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-",i,".zip",sep=""), data.table = TRUE), all=TRUE)
  }
  return(peep)
}

main <- getData(12)
