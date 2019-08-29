library(readxl)
library(data.table)
library(tidyverse)

loc <- file.path("src/ONET_define")
stw_list <- read.csv(file.path(loc, "Rothwell_STW_list.csv"))
crosswalk <- read_xlsx(file.path(loc, "Education_CIP_to_ONET_SOC.xlsx"))
colnames(crosswalk) <- c("cipCode","cipTitle","onetCode","onetTitle")

cip_stw <- crosswalk %>% filter(onetCode %in% stw_list$onet)

#schev_stw <- schev %>% filter(cipCode %in% cip_stw$cipCode)

#degreeTypes <- schev %>% group_by(degree) %>% summarise(count=n())

#the above is so messy that it's probably better to download separate files for the 'levels' we're interested in and combine them, rather
#than trying to decode hundreds of degree types

loc2 <- file.path("~/stem_edu/data/stem_edu/original/schev-data/program-by-degree-level")

files <- list.files(loc2)

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
  combine
}

schev_subbach <- read_combine(files)
colnames(schev_subbach) <- c("institution", "level", "degree", "programName", "cipCode")
unique(schev_subbach$level)

#which of these are STW-related (based on Rothwell's O*NET definition)
schev_stw <- schev_subbach %>% filter(cipCode %in% cip_stw$cipCode)
schev_stw %>% group_by(level) %>% summarise(count = n())
schev_stw %>% group_by(programName) %>% summarise(count = n()) %>% arrange(desc(count))

#what are the unique schools and how many STW-related programs does each have?

stw_school <- dcast(schev_stw, institution ~level)
colnames(stw_school) <- c("institution", "assocDegreeBachelorCredit", "assocDegreeOccTech",
                          "atLeast1lessThan2yrs", "atLeast2lessThan4yrs", "lessThan1yr")
stw_school <- stw_school %>% mutate(total = assocDegreeBachelorCredit + assocDegreeOccTech +
                       atLeast1lessThan2yrs + atLeast2lessThan4yrs + lessThan1yr) %>% select(institution, total, lessThan1yr, atLeast1lessThan2yrs, assocDegreeOccTech,
                     assocDegreeBachelorCredit, atLeast2lessThan4yrs)

loc3 <- file.path("data/stem_edu/working/schevData")

#writing out useful results
write.csv(schev_subbach, file.path(loc3, "schev_subbach_combine.csv"))
write.csv(schev_stw, file.path(loc3, "schev_stw_subbach_combine.csv"))
write.csv(stw_school, file.path(loc3, "schev_stw_schools.csv"))
