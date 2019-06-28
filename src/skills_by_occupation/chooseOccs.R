library(tidyverse)
library(data.table)

####Finding most common STW positions in Richmond and Blacksburg

#reading in STW jobs in Richmond/Blacksburg

loc <- file.path("data/stem_edu/working/Team_SA_job_skills_filter")
b_stw <- fread(file.path(loc, "rothwell_blacksburg_stw_job.csv"))

r_stw <- fread(file.path(loc, "rothwell_richmond_stw_job.csv"))

#combining into one dataframe

all_stw <- rbind(cbind(b_stw, place = "Blacksburg"), cbind(r_stw, place = "Richmond"))
all_stw <- all_stw[,3:57]

all_stw %>% group_by(place) %>% summarise(count = n())

#most common STW job postings

stw_desc <- all_stw %>% group_by(onet, onetname) %>% summarise(all_count = n(),
              all_perc = n()/length(unique(all_stw$onet))) %>% arrange(desc(all_count))

b_count <- function(x){
  length(b_stw[])
}
