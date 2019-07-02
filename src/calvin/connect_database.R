# Getting Access to the Data

# install.packages("RPostgreSQL")
require("RPostgreSQL")
library("RPostgreSQL")

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






# Attempting the more direct method:
# DONT UNZIP unzip("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-01.zip")

what <- read.table(unz("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-01.zip", "Main_2016-01.txt"), fill = TRUE)
first16 <- what[what$V26 == "Virginia" | what$V26 == "State", ]



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
