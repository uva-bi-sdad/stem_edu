require(tm)
require(NLP)
library(tokenizers)
library(tidyverse)
library(data.table)
library(forcats)
library(SnowballC)
data.dir = "./data/stem_edu/working/"

# Load in the file containing the locations and onet codes of all job postings

parsedJobs = na.omit(fread(paste0(data.dir, "allOpenjobsParsed.csv")))
crosswalkFull = read.csv("./data/stem_edu/original/occupation_cw.csv", header = T, stringsAsFactors = F)[,-1]


# Now we can find SOC codes from the crosswalk and extract whatever we want
# For now we're looking at one profession: nuclear medicine tech. let's find their postings

nmtSocCode = crosswalkFull[grep("Nuclear Medicine Technologists", crosswalkFull$onet_soc_title, ignore.case = T), 1]

nmtJobs = parsedJobs[normalizedTitle_onetCode == nmtSocCode]

tokenizeJobs = function(jobData, feature){

  text = jobData[,feature, with = FALSE]
  text = text[as.vector(text != "")]

  return(unlist(sapply(text, tokenize_sentences, lowercase = TRUE)))
}

findSentencesWithMatch = function(tokenizedColumn, searchVec){

  tokenizedColumn %>%
    sapply(grep, pattern = searchVec, value = TRUE, USE.NAMES = FALSE) %>%
    unlist
}

makeWordTable = function(tokenizedColumn, searchVec){
  tokenizedColumn %>%
    sapply(grep, pattern = searchVec, value = TRUE, USE.NAMES = FALSE) %>%
    unlist %>%
    tokenize_words(stopwords = tm::stopwords("SMART")) %>%
    unlist %>%
    table %>% sort
}

nmtResponsibilities = tokenizeJobs(nmtJobs, "responsibilities")
nmtExperience = tokenizeJobs(nmtJobs, "experienceRequirements")
nmtDescription = tokenizeJobs(nmtJobs, "jobDescription")

# Each job has a set of technical skills which we want to search for. This function extracts the tech skills for a given profession

getTechSkills = function(code, collapseWords = F){
  techSkills = fread("./data/stem_edu/original/OPED_Database_22/Tools and Technology.txt", na.strings = "n/a")
  colnames(techSkills)[1] = "onetCode"
  colnames(techSkills) = gsub(" ", "", colnames(techSkills))
  out = techSkills[onetCode == code & T2Type == "Technology"]
  if(!collapseWords) return(out)
  out$T2Example = gsub(" ", "|", out$T2Example)
  out$CommodityTitle = gsub(" ", "|", out$CommodityTitle)
  return(out)
}

techs = tolower(paste0(unique(getTechSkills(nmtSocCode, TRUE)$CommodityTitle), collapse = "|"))

findSentencesWithMatch(nmtResponsibilities, techs)
findSentencesWithMatch(nmtExperience, techs)
findSentencesWithMatch(nmtDescription, techs)

# Software Dev

devSocCode = "15-1132.00"
devJobs = parsedJobs[normalizedTitle_onetCode == devSocCode]

devResponsibilities = tokenizeJobs(devJobs, "responsibilities")
devExperience = tokenizeJobs(devJobs, "experienceRequirements")
devDescription = tokenizeJobs(devJobs, "jobDescription")

techs = tolower(paste0(getTechSkills(devSocCode)$CommodityTitle, collapse = "|"))

findSentencesWithMatch(devResponsibilities, techs)
findSentencesWithMatch(devExperience, techs)
findSentencesWithMatch(devDescription, techs)
