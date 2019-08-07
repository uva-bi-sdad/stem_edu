#+++++++++++++++++++++++++++++++++ READING IN RESUMES +++++++++++++++++++++++++++++++++++#
# The original code for this is in calvin_profiling.R at the bottom.
# I moved it over so it isn't a complete mess.
# Just run in the order of the lines to get all of the resumes from Virginia.

# I did not get a chance to look through all of the files. Some of them might be unzipped, so
# if the file is unzipped, just gzip it back up and run again

library(dplyr)
library(stringr)
library(data.table)

# Read in the first file:
currentSeries <- 1
resume <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_personal_info.csv.gz",sep=""))
va_Resumes <- resume[grepl("Virginia", resume$StateName),]

# Read in the rest of the files and combine them
for (currentSeries in seq(2:454)+1){
  filePath <- paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_personal_info.csv.gz",sep="")
  print(paste("Currently reading in file number ",currentSeries,".",sep = ""))
  currentResumes <- fread(filePath)
  va_Current <- currentResumes[grepl("Virginia", currentResumes$StateName),]
  va_Resumes <- rbindlist(list(va_Resumes,va_Current))
  print(paste("We now have a total of ", length(va_Resumes$BGTResId)," resumes in Virginia.",sep = ""))
}

# You should change this to whatever you want to name it
write.csv(va_Resumes,"resume_personal_info_va.csv")


# Next select only the resume IDs for the VA people
allIds <- va_Resumes$BGTResId

# Function to read in the other file types
getOtherInfo <- function(fileTypes){

  currentSeries <- 1
  resume <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_",fileTypes,".csv.gz",sep=""))
  otherInfo <- resume[as.character(resume$BGTResId) %chin% as.character(allIds),]

  for (currentSeries in seq(2:454)+1){
    filePath <- paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_",fileTypes,".csv.gz",sep="")
    print(paste("Currently reading in file number ",currentSeries,".",sep = ""))
    currentResumes <- fread(filePath)
    otherCurrent <- resume[as.character(resume$BGTResId) %chin% as.character(allIds),]
    otherInfo <- rbindlist(list(otherInfo,otherCurrent))
    print(paste("We now have a total of ", length(otherInfo$BGTResId)," ",fileTypes," rows in Virginia.",sep = ""))
  }
  return(otherInfo)
}

# Then get the other four file types
va_skills <- getOtherInfo("skill_info")
write.csv(va_skills,"resume_skill_info_va.csv")

va_skills <- getOtherInfo("education_info")
write.csv(va_skills,"resume_education_info_va.csv")

va_skills <- getOtherInfo("certification_info")
write.csv(va_skills,"resume_certification_info_va.csv")

va_skills <- getOtherInfo("job_info")
write.csv(va_skills,"resume_job_info_va.csv")
