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

# Words: knowledge, skill, abilit, require, qualif, respons

#searchVec = paste0(c("knowledge", "skill", "abilit", "require", "qualif", "respons"), collapse = "|")
searchVec = paste0(c("knowledge", "skill", "abilit", "qualif", "respons", "exper"), collapse = "|")

findSentencesWithMatch(nmtResponsibilities, searchVec)
makeWordTable(nmtResponsibilities, searchVec)

findSentencesWithMatch(nmtExperience, searchVec)
makeWordTable(nmtExperience, searchVec)

findSentencesWithMatch(nmtDescription, searchVec)
makeWordTable(nmtDescription, searchVec)











