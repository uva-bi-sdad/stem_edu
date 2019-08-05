#+++++++++++++++++++++++++++++++++ READING IN RESUMES +++++++++++++++++++++++++++++++++++#
# The original code for this is in calvin_profiling.R at the bottom.
# I moved it over so it isn't a complete mess.

library(stringr)

# Read's the zipped file without unzipping them
job <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",currentSeries,"_job_info.csv.gz",sep=""))






# Create for loop 454 files, keep only relevant resumes each step of the way and combine to one table
bringTogether <- function(fileType) {
  peep <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/1","_",fileType,".csv.gz",sep=""), data.table = TRUE)
  for (i in (seq(2:5)+1)){
    peep<- merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",fileType,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
  }
  peep$msa <- sub("^(\\d{5}).*$", "\\1", peep$msa)
  either_resumes <- peep[is.na(str_extract(peep$msa, "40060")) | is.na(str_extract(peep$msa, "13980")),]
  # peep$rich_blacksburg <- ifelse((is.na(str_extract(peep$msa, "40060"))|is.na(str_extract(peep$msa, "13980"))), TRUE, FALSE)
  # if (is.na(str_extract(peep$msa, "40060"))) {
  #   peep$msa <- "40060"
  # } else if (is.na(str_extract(peep$msa, "13980"))) {
  #   peep$msa <- "13980"
  # } else {
  #   peep$msa <- peep$msa
  # }
  either_resumes <- peep[peep$rich_blacksburg,]
  eitherresid <- either_resumes$BGTResId
  peep <- peep[BGTResId %chin% eitherresid]
  for (j in (seq(1:89))) {
    x <- j * 5 + 1
    peep1 <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",x,"_",fileType,".csv.gz",sep=""), data.table = TRUE)
    for (i in (seq(2:5)+x)){
      peep1<- merge(peep1,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",fileType,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
    }
    peep1$msa <- sub("^(\\d{5}).*$", "\\1", peep1$msa)
    either_resumes <- peep1[peep1$msa == "40060" | peep1$msa == "13980",]
    eitherresid <- either_resumes$BGTResId
    peep1 <- peep1[BGTResId %chin% eitherresid]
    peep<- merge(peep,peep1,all=TRUE, by= names(peep))
  }
  for (i in (seq(1:4)+450)){
    peep <-merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",fileType,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
  }
  peep$msa <- sub("^(\\d{5}).*$", "\\1", peep$msa)
  either_resumes <- peep[peep$msa == "40060" | peep$msa == "13980",]
  eitherresid <- either_resumes$BGTResId
  peep <- peep[BGTResId %chin% eitherresid]
  return(peep)
}

# Get's all of the resume's personal info's together, takes ~5 mins to run
all_resumes <- bringTogether("personal_info")

richmond_resumes <- all_resumes[all_resumes$msa == "40060",]
blacksburg_resumes <- all_resumes[all_resumes$msa == "13980",]
richresid <- richmond_resumes$BGTResId
blacksburgresid <- blacksburg_resumes$BGTResId

# Combines the other files into one based off of the people from our MSAs
combine_other <- function(msa, filet) {
  peep <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/1","_",filet,".csv.gz",sep=""), data.table = TRUE)
  for (i in (seq(2:5)+1)){
    peep<- merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",filet,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
  }
  peep <- peep[BGTResId %chin% msa]
  for (j in (seq(1:89))) {
    print(paste(as.character((j*100)/89), "%"))
    x <- j * 5 + 1
    peep1 <- fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",x,"_",filet,".csv.gz",sep=""), data.table = TRUE)
    for (i in (seq(2:5)+x)){
      peep1<- merge(peep1,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",filet,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
    }
    peep1 <- peep1[BGTResId %chin% msa]
    peep<- merge(peep,peep1,all=TRUE, by= names(peep))
  }
  for (i in (seq(1:4)+450)){
    peep <-merge(peep,fread(paste("../stem_edu/data/stem_edu/original/Burning_Glass_Data/Resume_Data/",i,"_",filet,".csv.gz",sep=""), data.table = TRUE), all=TRUE)
  }
  peep <- peep[BGTResId %chin% msa]
  return(peep)
}

all_edu <- bringTogether("education_info")
all_job <- bringTogether("job_info")
all_skill <- bringTogether("skill_info")

# Gets all of the certificates that come from our MSAs, takes ~3 minutes to run
richmond_certs <- combine_other(richresid, "certification_info")
blacksburg_certs <- combine_other(blacksburgresid, "certification_info")

# Gets all of the education that come from our MSAs, takes ~20 minutes to run
richmond_edu <- combine_other(richresid, "education_info")
blacksburg_edu <- combine_other(blacksburgresid, "education_info")

# Gets all of the job history that come from our MSAs, takes ~ minutes to run
richmond_job <- combine_other(richresid, "job_info")
blacksburg_job <- combine_other(blacksburgresid, "job_info")


all_resumes <- read.table("../stem_edu/all_resumes.txt", sep="\t")
blacksburg_resumes <- all_resumes[all_resumes$msa == "13980",]
blacksburgresid <- blacksburg_resumes$BGTResId


# Gets all of the skills that come from our MSAs, takes ~ minutes to run
richmond_skill <- combine_other(richresid, "skill_info")
blacksburg_skill <- combine_other(blacksburgresid, "skill_info")

write.table(blacksburg_skill, "../stem_edu/blacksburg_skills.txt", sep="\t")
