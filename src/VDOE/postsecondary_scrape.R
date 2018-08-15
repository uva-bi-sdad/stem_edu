# tbl 13: Postsecondary Enrollment
load(file = "~/git/dashboard/data/VDOE/school_list_22.RData")

library(dplyr)
postsecondary_df <- bind_rows(replicate(14, school_list_22, simplify = FALSE))
postsecondary_df$Year <- rep("2014-15",length(postsecondary_df$SchoolName))
postsecondary_df$Student_Type <- c(rep("All_Students",22),rep("Female",22),rep("Male",22),
                                   rep("American_Indian",22),rep("Asian",22),rep("Black",22),
                                   rep("Hispanic",22),rep("Native_Hawaiian",22),rep("White",22),
                                   rep("Two_or_more_races",22),rep("Students_with_Disabilities",22),
                                   rep("Economically_Disadvantaged",22),rep("English_Learners",22),rep("Homeless",22))
postsecondary_df$Total_in_category <- rep(NA,length(postsecondary_df$SchoolName))
postsecondary_df$Total_higher_ed <- rep(NA,length(postsecondary_df$SchoolName))
postsecondary_df$Remaining_percent <- rep(NA,length(postsecondary_df$SchoolName))


for (i in 1:22){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",school_list_22$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')

  assign("tbl_13", rvest::html_table(tbls_school_i[13], header=TRUE, fill=TRUE)[[1]])

  all_students <- tbl_13[tbl_13[,1]=="All Students",]
  if (dim(all_students)[1]!=0){
    postsecondary_df[i,7:9] <- all_students[1,3:5]
  }

  female <- tbl_13[tbl_13[,1]=="Female",]
  if (dim(female)[1]!=0){
    postsecondary_df[i+22,7:9] <- female[1,3:5]
  }

  male <- tbl_13[tbl_13[,1]=="Male",]
  if (dim(male)[1]!=0){
    postsecondary_df[i+44,7:9] <- male[1,3:5]
  }

  american_indian <- tbl_13[tbl_13[,1]=="American Indian",]
  if (dim(american_indian)[1]!=0){
    postsecondary_df[i+66,7:9] <- american_indian[1,3:5]
  }

  asian <- tbl_13[tbl_13[,1]=="Asian",]
  if (dim(asian)[1]!=0){
    postsecondary_df[i+88,7:9] <- asian[1,3:5]
  }

  black <- tbl_13[tbl_13[,1]=="Black",]
  if (dim(black)[1]!=0){
    postsecondary_df[i+110,7:9] <- black[1,3:5]
  }

  hispanic <- tbl_13[tbl_13[,1]=="Hispanic",]
  if (dim(hispanic)[1]!=0){
    postsecondary_df[i+132,7:9] <- hispanic[1,3:5]
  }

  hawaiian <- tbl_13[tbl_13[,1]=="Native Hawaiian",]
  if (dim(hawaiian)[1]!=0){
    postsecondary_df[i+154,7:9] <- hawaiian[1,3:5]
  }

  white <- tbl_13[tbl_13[,1]=="White",]
  if (dim(white)[1]!=0){
    postsecondary_df[i+176,7:9] <- white[1,3:5]
  }

  two_or_more <- tbl_13[tbl_13[,1]=="Two or more races",]
  if (dim(two_or_more)[1]!=0){
    postsecondary_df[i+198,7:9] <- two_or_more[1,3:5]
  }

  disabled <- tbl_13[tbl_13[,1]=="Students with Disabilities",]
  if (dim(disabled)[1]!=0){
    postsecondary_df[i+220,7:9] <- disabled[1,3:5]
  }

  econ <- tbl_13[tbl_13[,1]=="Economically Disadvantaged",]
  if (dim(econ)[1]!=0){
    postsecondary_df[i+242,7:9] <- econ[1,3:5]
  }

  english <- tbl_13[tbl_13[,1]=="English Learners",]
  if (dim(english)[1]!=0){
    postsecondary_df[i+264,7:9] <- english[1,3:5]
  }

  homeless <- tbl_13[tbl_13[,1]=="Homeless",]
  if (dim(homeless)[1]!=0){
    postsecondary_df[i+286,7:9] <- homeless[1,3:5]
  }

  all_students=NULL;female=NULL;male=NULL;american_indian=NULL;asian=NULL;
  black=NULL;hispanic=NULL;hawaiian=NULL;white=NULL;two_or_more=NULL;
  disabled=NULL;econ=NULL;english=NULL;homeless=NULL
  print(i)
}

write.csv(postsecondary_df, "~/stem_edu/data/stem_edu/final/VDOE/postsecondary.csv")
