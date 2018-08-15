# tbl 20: Percent of students absent

library(dplyr)
library(RCurl)
library(XML)
library(rvest)
library(tibble)

high_schools <- read.csv('~/stem_edu/data/stem_edu/final/VDOE/all_high_school_level.csv')
high_schools$SchoolName <- gsub(" ", "-", high_schools$SchoolName, fixed = TRUE)
absences_df <- bind_rows(replicate(42, high_schools, simplify = FALSE))
absences_df$Year <- c(rep("2014-15",4662), rep("2015-16",4662), rep("2015-16",4662))
absences_df$Student_Type <- c(rep("All_Students",333),rep("Female",333),rep("Male",333),
                              rep("American_Indian",333),rep("Asian",333),rep("Black",333),
                              rep("Hispanic",333),rep("Native_Hawaiian",333),rep("White",333),
                              rep("Two_or_more_races",333),rep("Students_with_Disabilities",333),
                              rep("Economically_Disadvantaged",333),rep("English_Learners",333),rep("Homeless",333))
absences_df$percent_0_to_10 <- rep(NA,length(absences_df$SchoolName))
absences_df$percent_10_to_15 <- rep(NA,length(absences_df$SchoolName))
absences_df$percent_15_to_20 <- rep(NA,length(absences_df$SchoolName))
absences_df$percent_20_plus <- rep(NA,length(absences_df$SchoolName))

# school_list_22 <- read.csv("~/stem_edu/data/stem_edu/final/VDOE/22_schools_list.csv")
# school_list_22$SchoolName <- gsub(" ", "-", school_list_22$SchoolName, fixed = TRUE)
# absences_df <- bind_rows(replicate(42, school_list_22, simplify = FALSE))
# absences_df$Year <- c(rep("2014-15",308),rep("2015-16",308),rep("2016-17",308))
# absences_df$Student_Type <- c(rep("All_Students",22),rep("Female",22),rep("Male",22),
#                               rep("American_Indian",22),rep("Asian",22),rep("Black",22),
#                               rep("Hispanic",22),rep("Native_Hawaiian",22),rep("White",22),
#                               rep("Two_or_more_races",22),rep("Students_with_Disabilities",22),
#                               rep("Economically_Disadvantaged",22),rep("English_Learners",22),rep("Homeless",22))
# absences_df$percent_0_to_10 <- rep(NA,length(absences_df$SchoolName))
# absences_df$percent_10_to_15 <- rep(NA,length(absences_df$SchoolName))
# absences_df$percent_15_to_20 <- rep(NA,length(absences_df$SchoolName))
# absences_df$percent_20_plus <- rep(NA,length(absences_df$SchoolName))




for (i in 1:333)
{
  #i = 48
  wl_school_hs <- read_html(paste("http://schoolquality.virginia.gov/schools/", high_schools$SchoolName[i], sep = ""))
  tbls_school_hs <- html_nodes(wl_school_hs, 'table')

  absent_row <- 0
  for (j in 1:length(tbls_school_hs))
  {
    if (grepl("Absent", html_text(tbls_school_hs[j])))
    {
      absent_row <- j

    }
  }

  assign("tbl_hs", html_table(tbls_school_hs[absent_row], header=TRUE, fill=TRUE)[[1]])
  if (ncol(tbl_hs) == 17){
    tbl_hs[,2:5] <- NULL
  } else if (tbl_hs[1,2] == "2016-2017"){
    tbl_hs[,6:13] <- NA
    tbl_hs <- tbl_hs[,c(1,6:13,2:5)]
  } else if (tbl_hs[1,2] == "2015-2016"){
    tbl_hs[,10:13] <- NA
    tbl_hs <- tbl_hs[,c(1,10:13,2:9)]
  }


  #all_students <- tbl_20[tbl_20[,1]=="All Students",]
  all_students <- tbl_hs[tbl_hs[,1]=="All Students",]
  if (dim(all_students)[1]!=0){
    absences_df[i,6:9] <- all_students[1,2:5]
    #absences_df[i+308, 6:9] <- all_students[1,6:9]
    #absences_df[i+616,6:9] <- all_students[1,10:13]

    absences_df[i+4662, 6:9] <- all_students[1,6:9]
    absences_df[i+9324,6:9] <- all_students[1,10:13]

  }

  #female <- tbl_20[tbl_20[,1]=="Female",]
  female <- tbl_hs[tbl_hs[,1]=="Female",]
  if (dim(female)[1]!=0){
    # absences_df[i+22,6:9] <- female[1,2:5]
    # absences_df[i+22+308,6:9] <- female[1,6:9]
    # absences_df[i+22+616,6:9] <- female[1,10:13]

    absences_df[i+333,6:9] <- female[1,2:5]
    absences_df[i+333+4662,6:9] <- female[1,6:9]
    absences_df[i+333+9324,6:9] <- female[1,10:13]
  }

  # male <- tbl_20[tbl_20[,1]=="Male",]
  male <- tbl_hs[tbl_hs[,1]=="Male",]
  if (dim(male)[1]!=0){
    # absences_df[i+44,6:9] <- male[1,2:5]
    # absences_df[i+44+308,6:9] <- male[1,6:9]
    # absences_df[i+44+616,6:9] <- male[1,10:13]

    absences_df[i+666,6:9] <- female[1,2:5]
    absences_df[i+666+4662,6:9] <- female[1,6:9]
    absences_df[i+666+9324,6:9] <- female[1,10:13]
  }

  # american_indian <- tbl_20[tbl_20[,1]=="American Indian",]
  american_indian <- tbl_hs[tbl_hs[,1]=="American Indian",]
  if (dim(american_indian)[1]!=0){
    # absences_df[i+66,6:9] <- american_indian[1,2:5]
    # absences_df[i+66+308,6:9] <- american_indian[1,6:9]
    # absences_df[i+66+616,6:9] <- american_indian[1,10:13]

    absences_df[i+999,6:9] <- female[1,2:5]
    absences_df[i+999+4662,6:9] <- female[1,6:9]
    absences_df[i+999+9324,6:9] <- female[1,10:13]
  }

  # asian <- tbl_20[tbl_20[,1]=="Asian",]
  asian <- tbl_hs[tbl_hs[,1]=="asian",]
  if (dim(asian)[1]!=0){
    # absences_df[i+88,6:9] <- asian[1,2:5]
    # absences_df[i+88+308,6:9] <- asian[1,6:9]
    # absences_df[i+88+616,6:9] <- asian[1,10:13]

    absences_df[i+1332,6:9] <- female[1,2:5]
    absences_df[i+1332+4662,6:9] <- female[1,6:9]
    absences_df[i+1332+9324,6:9] <- female[1,10:13]
  }

  #black <- tbl_20[tbl_20[,1]=="Black",]
  black <- tbl_hs[tbl_hs[,1]=="black",]
  if (dim(black)[1]!=0){
    # absences_df[i+110,6:9] <- black[1,2:5]
    # absences_df[i+110+308,6:9] <- black[1,6:9]
    # absences_df[i+110+616,6:9] <- black[1,10:13]

    absences_df[i+1665,6:9] <- female[1,2:5]
    absences_df[i+1665+4662,6:9] <- female[1,6:9]
    absences_df[i+1665+9324,6:9] <- female[1,10:13]
  }

  # hispanic <- tbl_20[tbl_20[,1]=="Hispanic",]
  hispanic <- tbl_hs[tbl_hs[,1]=="hispanic",]
  if (dim(hispanic)[1]!=0){
    # absences_df[i+132,6:9] <- hispanic[1,2:5]
    # absences_df[i+132+308,6:9] <- hispanic[1,6:9]
    # absences_df[i+132+616,6:9] <- hispanic[1,10:13]

    absences_df[i+1998,6:9] <- female[1,2:5]
    absences_df[i+1998+4662,6:9] <- female[1,6:9]
    absences_df[i+1998+9324,6:9] <- female[1,10:13]
  }

  # hawaiian <- tbl_20[tbl_20[,1]=="Native Hawaiian",]
  hawaiian <- tbl_hs[tbl_hs[,1]=="Native Hawaiian",]
  if (dim(hawaiian)[1]!=0){
    # absences_df[i+154,6:9] <- hawaiian[1,2:5]
    # absences_df[i+154+308,6:9] <- hawaiian[1,6:9]
    # absences_df[i+154+616,6:9] <- hawaiian[1,10:13]

    absences_df[i+2331,6:9] <- female[1,2:5]
    absences_df[i+2331+4662,6:9] <- female[1,6:9]
    absences_df[i+2331+9324,6:9] <- female[1,10:13]
  }

  # white <- tbl_20[tbl_20[,1]=="White",]
  white <- tbl_hs[tbl_hs[,1]=="White",]
  if (dim(white)[1]!=0){
    # absences_df[i+176,6:9] <- white[1,2:5]
    # absences_df[i+176+308,6:9] <- white[1,6:9]
    # absences_df[i+176+616,6:9] <- white[1,10:13]

    absences_df[i+2664,6:9] <- female[1,2:5]
    absences_df[i+2664+4662,6:9] <- female[1,6:9]
    absences_df[i+2664+9324,6:9] <- female[1,10:13]
  }

  # two_or_more <- tbl_20[tbl_20[,1]=="Two or more races",]
  two_or_more <- tbl_hs[tbl_hs[,1]=="Two or more races",]
  if (dim(two_or_more)[1]!=0){
    # absences_df[i+198,6:9] <- two_or_more[1,2:5]
    # absences_df[i+198+308,6:9] <- two_or_more[1,6:9]
    # absences_df[i+198+616,6:9] <- two_or_more[1,10:13]

    absences_df[i+2997,6:9] <- female[1,2:5]
    absences_df[i+2997+4662,6:9] <- female[1,6:9]
    absences_df[i+2997+9324,6:9] <- female[1,10:13]
  }

  # disabled <- tbl_20[tbl_20[,1]=="Students with Disabilities",]
  disabled <- tbl_hs[tbl_hs[,1]=="Students with Disabilities",]
  if (dim(disabled)[1]!=0){
    # absences_df[i+220,6:9] <- disabled[1,2:5]
    # absences_df[i+220+308,6:9] <- disabled[1,6:9]
    # absences_df[i+220+616,6:9] <- disabled[1,10:13]

    absences_df[i+3330,6:9] <- female[1,2:5]
    absences_df[i+3330+4662,6:9] <- female[1,6:9]
    absences_df[i+3330+9324,6:9] <- female[1,10:13]
  }

  # econ <- tbl_20[tbl_20[,1]=="Economically Disadvantaged",]
  econ <- tbl_hs[tbl_hs[,1]=="Economically Disadvantaged",]
  if (dim(econ)[1]!=0){
    # absences_df[i+242,6:9] <- econ[1,2:5]
    # absences_df[i+242+308,6:9] <- econ[1,6:9]
    # absences_df[i+242+616,6:9] <- econ[1,10:13]

    absences_df[i+3663,6:9] <- female[1,2:5]
    absences_df[i+3663+4662,6:9] <- female[1,6:9]
    absences_df[i+3663+9324,6:9] <- female[1,10:13]
  }

  # english <- tbl_20[tbl_20[,1]=="English Learners",]
  english <- tbl_hs[tbl_hs[,1]=="English Learners",]
  if (dim(english)[1]!=0){
    # absences_df[i+264,6:9] <- english[1,2:5]
    # absences_df[i+264+308,6:9] <- english[1,6:9]
    # absences_df[i+264+616,6:9] <- english[1,10:13]

    absences_df[i+3996,6:9] <- female[1,2:5]
    absences_df[i+3996+4662,6:9] <- female[1,6:9]
    absences_df[i+3996+9324,6:9] <- female[1,10:13]
  }

  # homeless <- tbl_20[tbl_20[,1]=="Homeless",]
  homeless <- tbl_hs[tbl_hs[,1]=="Homeless",]
  if (dim(homeless)[1]!=0){
    # absences_df[i+286,6:9] <- homeless[1,2:5]
    # absences_df[i+286+308,6:9] <- homeless[1,6:9]
    # absences_df[i+286+616,6:9] <- homeless[1,10:13]

    absences_df[i+4329,6:9] <- female[1,2:5]
    absences_df[i+4329+4662,6:9] <- female[1,6:9]
    absences_df[i+4329+9324,6:9] <- female[1,10:13]
  }

  all_students=NULL;female=NULL;male=NULL;american_indian=NULL;asian=NULL;
  black=NULL;hispanic=NULL;hawaiian=NULL;white=NULL;two_or_more=NULL;
  disabled=NULL;econ=NULL;english=NULL;homeless=NULL
  print(i)
}

write.csv(absences_df, "~/stem_edu/data/stem_edu/final/VDOE/absences_all.csv")
