library(readxl)
library(data.table)
library(tidyverse)

stw_list <- read.csv("src/ONET_define/Rothwell_STW_list.csv")
schev <- fread("data/stem_edu/original/schev_degree_inventory.csv")
crosswalk <- read_xlsx("src/ONET_define/Education_CIP_to_ONET_SOC.xlsx")
colnames(schev) <- c("V1","institution","degreeLevel","degree","programName","cipCode")
colnames(crosswalk) <- c("cipCode","cipTitle","onetCode","onetTitle")

str(schev)

cip_stw <- crosswalk %>% filter(onetCode %in% stw_list$onet)

schev_stw <- schev %>% filter(cipCode %in% cip_stw$cipCode)

degreeTypes <- schev %>% group_by(degree) %>% summarise(count=n())

#the above is so messy that it's probably better to download separate files for the 'levels' we're interested in and combine them, rather
#than trying to decode hundreds of degree types

loc <- file.path("~/stem_edu/data/stem_edu/original/schev-data/program-by-degree-level")

files <- list.files(loc)

#quick function to read in each file, delete the top row, and group them together

read_combine <- function(x){
  #where x is a vector of file names
  for(i in 1:length(x)){
    if(i == 1){
      first <- read_delim(file.path(loc, files)[i], delim = ",", col_names = TRUE, skip = 1)
      combine <- first
    } else {
      next.file <- read_delim(file.path(loc, files)[i], delim = ",", col_names = TRUE, skip = 1)
      combine <- rbind(combine, next.file)
    }
  }
}

schev_subbach <- read_combine(files)
colnames(schev_subbach) <- c("institution", "level", "degree", "programName", "cipCode")
unique(schev_subbach$Level)

#which of these are STW-related (based on Rothwell's O*NET definition)
schev_stw <- schev_subbach %>% filter(cipCode %in% cip_stw$cipCode)
schev_stw %>% group_by(level) %>% summarise(count = n())
schev_stw %>% group_by(programName) %>% summarise(count = n()) %>% arrange(desc(count))

