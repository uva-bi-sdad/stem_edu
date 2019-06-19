
library(tidyverse)
library(data.table)
library(readxl)
library(reshape2)

setwd("/home/af5ug/stem_edu")

knowledge <- read_excel("data/stem_edu/original/ONET_survey/Knowledge.xlsx")
education <- read_excel("data/stem_edu/original/ONET_survey/Education, Training, and Experience.xlsx")
titles <- read_excel("data/stem_edu/original/ONET_survey/Occupation Data.xlsx")
edu_cat <- read_excel("data/stem_edu/original/ONET_survey/Education, Training, and Experience Categories.xlsx")

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

##O*NET codes for occupations that require high levels of technical knowledge

know_occ <- unique(knowledge$soc_code)

####Filtering down to the education data we actually want

colnames(education) <- c("soc_code","title","element_id","element_name","scale_id",
                         "scale_name", "category", "data_value", "n","se","lower_ci",
                         "upper_ci", "recommend_suppress", "date", "domain_source")
#education %>% filter(soc_code == "11-1011.00", scale_id == "RL") %>% group_by(soc_code) %>% summarise(sum = sum(data_value))

education <- filter(education, scale_id == "RL")

edu_cast <- dcast(education, formula = soc_code ~ category, value.var = "data_value")

edu_cast$sub_bach <- 0

for(i in 1:nrow(edu_cast)){
  edu_cast[i,"sub_bach"] <- sum(edu_cast[i,2:6])
}

edu_occ <- unique(filter(edu_cast, sub_bach >= 50)$soc_code)
