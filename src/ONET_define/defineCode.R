
library(tidyverse)
library(data.table)
library(readxl)

setwd("/home/af5ug/stem_edu")

knowledge <- read_excel("data/stem_edu/original/ONET_survey/Knowledge.xlsx")
education <- read_excel("data/stem_edu/original/ONET_survey/Education, Training, and Experience.xlsx")
titles <- read_excel("data/stem_edu/original/ONET_survey/Occupation Data.xlsx")

###Trimming down knowledge data to what we actually need

colnames(knowledge) <- c("soc_code","title","element_id","element_name","scale_id",
                      "scale_name","data_value","n","se","lower_ci","upper_ci",
                      "recommend_suppress", "not_relevant", "date",
                      "domain_source")

knowledge <- filter(knowledge, scale_name == "Level", element_name %in%
                      c("Biology", "Building and Construction", "Chemistry",
                        "Computers and Electronics", "Design", "Economics and Accounting",
                        "Engineering and Technology", "Food Production", "Mathematics",
                        "Mechanical", "Medicine and Dentistry", "Physics",
                        "Production and Processing", "Telecommunications"), data_value >= 4.5)

###O*NET codes for occupations that require high levels of technical knowledge

know_occ <- unique(knowledge$soc_code)


