# tbl 10: diplomas and completion
load(file = "~/git/dashboard/data/VDOE/school_list_22.RData")

library(dplyr)
diplomas_completers_df <- bind_rows(replicate(14, school_list_22, simplify = FALSE))
diplomas_completers_df$Year <- rep("2016-17",length(diplomas_completers_df$SchoolName))
diplomas_completers_df$Student_Type <- c(rep("All_Students",22),rep("Female",22),rep("Male",22),
                                         rep("American_Indian",22),rep("Asian",22),rep("Black",22),
                                         rep("Hispanic",22),rep("Native_Hawaiian",22),rep("White",22),
                                         rep("Two_or_more_races",22),rep("Students_with_Disabilities",22),
                                         rep("Economically_Disadvantaged",22),rep("English_Learners",22),rep("Homeless",22))
diplomas_completers_df$Advanced_Diplomas <- rep(NA,length(diplomas_completers_df$SchoolName))
diplomas_completers_df$Standard_Diplomas <- rep(NA,length(diplomas_completers_df$SchoolName))
diplomas_completers_df$Other_Diplomas <- rep(NA,length(diplomas_completers_df$SchoolName))
diplomas_completers_df$GEDs <- rep(NA,length(diplomas_completers_df$SchoolName))
diplomas_completers_df$Dropouts <- rep(NA,length(diplomas_completers_df$SchoolName))
diplomas_completers_df$Other_NonGraduates <- rep(NA,length(diplomas_completers_df$SchoolName))

for (i in 1:22){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",school_list_22$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')

  assign("tbl_10", rvest::html_table(tbls_school_i[10], header=TRUE, fill=TRUE)[[1]])

  all_students <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="All Students",]
  if (dim(all_students)[1]!=0){
    diplomas_completers_df[i,7:12] <- all_students[1,3:8]
  }

  female <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Female",]
  if (dim(female)[1]!=0){
    diplomas_completers_df[i+22,7:12] <- female[1,3:8]
  }

  male <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Male",]
  if (dim(male)[1]!=0){
    diplomas_completers_df[i+44,7:12] <- male[1,3:8]
  }

  american_indian <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="American Indian",]
  if (dim(american_indian)[1]!=0){
    diplomas_completers_df[i+66,7:12] <- american_indian[1,3:8]
  }

  asian <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Asian",]
  if (dim(asian)[1]!=0){
    diplomas_completers_df[i+88,7:12] <- asian[1,3:8]
  }

  black <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Black",]
  if (dim(black)[1]!=0){
    diplomas_completers_df[i+110,7:12] <- black[1,3:8]
  }

  hispanic <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Hispanic",]
  if (dim(hispanic)[1]!=0){
    diplomas_completers_df[i+132,7:12] <- hispanic[1,3:8]
  }

  hawaiian <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Native Hawaiian",]
  if (dim(hawaiian)[1]!=0){
    diplomas_completers_df[i+154,7:12] <- hawaiian[1,3:8]
  }

  white <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="White",]
  if (dim(white)[1]!=0){
    diplomas_completers_df[i+176,7:12] <- white[1,3:8]
  }

  two_or_more <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Two or more races",]
  if (dim(two_or_more)[1]!=0){
    diplomas_completers_df[i+198,7:12] <- two_or_more[1,3:8]
  }

  disabled <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Students with Disabilities",]
  if (dim(disabled)[1]!=0){
    diplomas_completers_df[i+220,7:12] <- disabled[1,3:8]
  }

  econ <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Economically Disadvantaged",]
  if (dim(econ)[1]!=0){
    diplomas_completers_df[i+242,7:12] <- econ[1,3:8]
  }

  english <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="English Learners",]
  if (dim(english)[1]!=0){
    diplomas_completers_df[i+264,7:12] <- english[1,3:8]
  }

  homeless <- tbl_10[tbl_10$`Status of the Students in the 2016-2017 Cohort`=="Homeless",]
  if (dim(homeless)[1]!=0){
    diplomas_completers_df[i+286,7:12] <- homeless[1,3:8]
  }

  all_students=NULL;female=NULL;male=NULL;american_indian=NULL;asian=NULL;
  black=NULL;hispanic=NULL;hawaiian=NULL;white=NULL;two_or_more=NULL;
  disabled=NULL;econ=NULL;english=NULL;homeless=NULL
  print(i)
}

write.csv(diplomas_completers_df, "~/stem_edu/data/stem_edu/final/VDOE/diploma_completers.csv")
