library(data.table)
library(stringr)
library(jsonlite)

jobsUrls = c("http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/9a810771-d6c9-43a8-93bd-144678cbdd4a/download/openjobs-jobpostings.mar-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/8e41ef3b-8b7b-4e2b-86aa-f256b754ef1b/download/openjobs-jobpostings.apr-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/074f7e44-9275-4bba-874e-4795e8f6830c/download/openjobs-jobpostings.may-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/d0421684-a088-40af-b9a1-11aa5bba4e60/download/openjobs-jobpostings.jun-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/984207df-9a8d-4de4-83cf-94703dd2fb6f/download/openjobs-jobpostings.jul-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/423d14c0-7646-4679-afb5-247e36904e07/download/openjobs-jobpostings.aug-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/aa577c2f-ce84-447d-97a8-d82c58fb271e/download/openjobs-jobpostings.sep-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/007fd197-e9ac-4148-b14d-de101ac19f72/download/openjobs-jobpostings.oct-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/07f141b6-7604-424b-88f3-da699dc37921/download/openjobs-jobpostings.nov-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/e492586c-e2c8-410c-9363-91782ea28f00/download/openjobs-jobpostings.dec-2016.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/f8317b24-e0db-4445-b9fc-666783c0f7c2/download/openjobs-jobpostings.jan-2017.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/d5523054-87f1-4914-b26c-f6226205d97b/download/openjobs-jobpostings.feb-2017.json",
             "http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/229aa3cc-573f-4d16-82b3-805239df9310/download/openjobs-jobpostings.mar-2017.json",
             'http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/29aab2f0-2fe8-4a33-ac95-6ee7d4a19484/download/openjobs-jobpostings.apr-2017.json',
             'http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/248fe6b8-f342-400c-9e60-0fa3e152b017/download/openjobs-jobpostings.may-2017.json',
             'http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/ee36aa59-75e0-4246-9f0a-12e1dcf32414/download/openjobs-jobpostings.jun-2017.json',
             'http://opendata.cs.vt.edu/dataset/ab0abac3-2293-4c9d-8d80-22d450254389/resource/27d4c33b-6ef2-43bf-9ac3-8c4def1e4520/download/openjobs-jobpostings.jul-2017.json')


# Scraping functions


{
  clean_json_parse <- function(json_dat) {
    return(unlist(unname(json_dat)))
  }

  jobLocation_address_fullText <- function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    return(clean_json_parse(json_dat$jobLocation$address$fullText))
  }

  jobLocation_address_region <- function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    return(clean_json_parse(json_dat$jobLocation$address$region))
  }

  jobLocation_address_region <- function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    return(clean_json_parse(json_dat$jobLocation$address$region))
  }

  jobLocation_geo_latitude <- function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    cleanedOut = clean_json_parse(json_dat$jobLocation$geo$latitude)
    if(is.null(cleanedOut)) cleanedOut = NA
    return(cleanedOut)
  }

  jobLocation_geo_longitude <- function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    cleanedOut = clean_json_parse(json_dat$jobLocation$geo$longitude)
    if(is.null(cleanedOut)) cleanedOut = NA
    return(cleanedOut)
  }

  normalizedTitle_onetCode <- function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    return(clean_json_parse(json_dat$normalizedTitle$onetCode))
  }

  normalizedTitle_onetName <- function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    return(clean_json_parse(json_dat$normalizedTitle$onetName))
  }

  rawdata_id <- function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    return(json_dat$id)
  }

  datePosted = function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    return(json_dat$datePosted)
  }

  responsibilities = function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)$responsibilities
    return(paste0(json_dat, collapse = ', '))
  }

  experienceRequirements = function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)$experienceRequirements
    return(paste0(json_dat, collapse = ', '))
  }

  jobDescription = function(json_dat) {
    json_dat <- jsonlite::fromJSON(json_dat)
    return(json_dat$jobDescription)
  }
}

i = 1
raw_data <- readLines(jobsUrls[i], warn = F)
parsedJobs = data.frame(rawdata_id = sapply(raw_data, rawdata_id, USE.NAMES = FALSE), stringsAsFactors = F)

parsedJobs$jobLocation_geo_latitude <- sapply(raw_data, jobLocation_geo_latitude, USE.NAMES = FALSE)
parsedJobs$jobLocation_geo_longitude <- sapply(raw_data, jobLocation_geo_longitude, USE.NAMES = FALSE)
parsedJobs$normalizedTitle_onetCode <- sapply(raw_data, normalizedTitle_onetCode, USE.NAMES = FALSE)
parsedJobs$normalizedTitle_onetName <- sapply(raw_data, normalizedTitle_onetName, USE.NAMES = FALSE)
parsedJobs$datePosted = sapply(raw_data, datePosted, USE.NAMES = FALSE)
parsedJobs$responsibilities = sapply(raw_data, responsibilities, USE.NAMES = FALSE)
parsedJobs$experienceRequirements = sapply(raw_data, experienceRequirements, USE.NAMES = FALSE)
parsedJobs$jobDescription = sapply(raw_data, jobDescription, USE.NAMES = FALSE)

allParsedJobs = parsedJobs

for(i in 2:17){
  raw_data <- readLines(jobsUrls[i], warn = F)
  parsedJobs = data.frame(rawdata_id = sapply(raw_data, rawdata_id, USE.NAMES = FALSE), stringsAsFactors = F)

  parsedJobs$jobLocation_geo_latitude <- sapply(raw_data, jobLocation_geo_latitude, USE.NAMES = FALSE)
  parsedJobs$jobLocation_geo_longitude <- sapply(raw_data, jobLocation_geo_longitude, USE.NAMES = FALSE)
  parsedJobs$normalizedTitle_onetCode <- sapply(raw_data, normalizedTitle_onetCode, USE.NAMES = FALSE)
  parsedJobs$normalizedTitle_onetName <- sapply(raw_data, normalizedTitle_onetName, USE.NAMES = FALSE)
  parsedJobs$datePosted = sapply(raw_data, datePosted, USE.NAMES = FALSE)
  parsedJobs$responsibilities = sapply(raw_data, responsibilities, USE.NAMES = FALSE)
  parsedJobs$experienceRequirements = sapply(raw_data, experienceRequirements, USE.NAMES = FALSE)
  parsedJobs$jobDescription = sapply(raw_data, jobDescription, USE.NAMES = FALSE)
  allParsedJobs = rbind(allParsedJobs, parsedJobs)
  print(i)
}

# Get this thing away from me.

fwrite(allParsedJobs, "./data/stem_edu/working/allOpenjobsParsed.csv")



