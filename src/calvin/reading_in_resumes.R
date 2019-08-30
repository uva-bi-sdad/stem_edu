#+++++++++++++++++++++++++++++++++ READING IN RESUMES +++++++++++++++++++++++++++++++++++#
# Calvin Isch
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

# Remove the West Virginia peeps:
va_Resumes <- va_Resumes[!(va_Resumes$StateName == "West Virginia"), ]

# You should change this to whatever you want to name it
write.csv(va_Resumes,"resume_personal_info_va.csv")


# Next select only the resume IDs for the VA people
allIds <- va_Resumes$BGTResId
# Clear it from memory so R is less likely to crash
remove(va_Resumes)


# Function to read in the other file types
# You need to change the variable BGTResId for education_info and skill_info to BGTResID
getOtherInfo <- function(fileTypes) {
  folder <- "../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/"
  file_paths <- list.files(folder)
  info_paths <- file_paths[str_detect(string = file_paths, pattern = fileTypes) == TRUE]
  info_all <- data.table()

  for (i in seq(1:length(info_paths))) {
    filepath <- paste0(folder, info_paths[i])
    print(paste("Currently reading in file number ", i, ".", sep = ""))
    print(filepath)
    currentInfo <- fread(filepath)
    colnames(currentInfo)[1] <- "BGTResID"
    currentInfo <- currentInfo[as.character(currentInfo$BGTResID) %chin% as.character(allIds),]
    print(nrow(currentInfo))
    info_all <- rbindlist(list(info_all, currentInfo))
  }
  info_all
}

# Then get the other four file types
va_skills <- getOtherInfo("skill_info")
write.csv(va_skills,"resume_skill_info_va.csv")
remove(va_skills)

va_education <- getOtherInfo("education_info")
write.csv(va_education,"resume_education_info_va.csv")
remove(va_education)

va_certifications <- getOtherInfo("certification_info")
write.csv(va_certifications,"resume_certification_info_va.csv")
remove(va_certifications)

va_jobs <- getOtherInfo("job_info")
write.csv(va_jobs,"resume_job_info_va.csv")
remove(va_jobs)
