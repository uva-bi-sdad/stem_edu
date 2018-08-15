# tbl 11: 4 year Va on-time grad rate
load(file = "~/git/dashboard/data/VDOE/school_list_22.RData")

library(dplyr)
four_year_grad_df <- bind_rows(replicate(14, school_list_22, simplify = FALSE))
four_year_grad_df$Year <- rep("2016-17",length(four_year_grad_df$SchoolName))
four_year_grad_df$Student_Type <- c(rep("All_Students",22),rep("Female",22),rep("Male",22),
                                    rep("American_Indian",22),rep("Asian",22),rep("Black",22),
                                    rep("Hispanic",22),rep("Native_Hawaiian",22),rep("White",22),
                                    rep("Two_or_more_races",22),rep("Students_with_Disabilities",22),
                                    rep("Economically_Disadvantaged",22),rep("English_Learners",22),rep("Homeless",22))
four_year_grad_df$Total_in_category <- rep(NA,length(four_year_grad_df$SchoolName))
four_year_grad_df$Graduates <- rep(NA,length(four_year_grad_df$SchoolName))
four_year_grad_df$OnTime_GradRate <- rep(NA,length(four_year_grad_df$SchoolName))
four_year_grad_df$Completers <- rep(NA,length(four_year_grad_df$SchoolName))
four_year_grad_df$Completion_Rate <- rep(NA,length(four_year_grad_df$SchoolName))
four_year_grad_df$Dropouts <- rep(NA,length(four_year_grad_df$SchoolName))
four_year_grad_df$Dropout_Rate <- rep(NA,length(four_year_grad_df$SchoolName))

for (i in 1:22){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",school_list_22$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')

  assign("tbl_11", rvest::html_table(tbls_school_i[11], header=TRUE, fill=TRUE)[[1]])

  all_students <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="All Students",]
  if (dim(all_students)[1]!=0){
    four_year_grad_df[i,7:13] <- all_students[1,2:8]
  }

  female <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Female",]
  if (dim(female)[1]!=0){
    four_year_grad_df[i+22,7:13] <- female[1,2:8]
  }

  male <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Male",]
  if (dim(male)[1]!=0){
    four_year_grad_df[i+44,7:13] <- male[1,2:8]
  }

  american_indian <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="American Indian",]
  if (dim(american_indian)[1]!=0){
    four_year_grad_df[i+66,7:13] <- american_indian[1,2:8]
  }

  asian <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Asian",]
  if (dim(asian)[1]!=0){
    four_year_grad_df[i+88,7:13] <- asian[1,2:8]
  }

  black <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Black",]
  if (dim(black)[1]!=0){
    four_year_grad_df[i+110,7:13] <- black[1,2:8]
  }

  hispanic <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Hispanic",]
  if (dim(hispanic)[1]!=0){
    four_year_grad_df[i+132,7:13] <- hispanic[1,2:8]
  }

  hawaiian <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Native Hawaiian",]
  if (dim(hawaiian)[1]!=0){
    four_year_grad_df[i+154,7:13] <- hawaiian[1,2:8]
  }

  white <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="White",]
  if (dim(white)[1]!=0){
    four_year_grad_df[i+176,7:13] <- white[1,2:8]
  }

  two_or_more <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Two or more races",]
  if (dim(two_or_more)[1]!=0){
    four_year_grad_df[i+198,7:13] <- two_or_more[1,2:8]
  }

  disabled <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Students with Disabilities",]
  if (dim(disabled)[1]!=0){
    four_year_grad_df[i+220,7:13] <- disabled[1,2:8]
  }

  econ <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Economically Disadvantaged",]
  if (dim(econ)[1]!=0){
    four_year_grad_df[i+242,7:13] <- econ[1,2:8]
  }

  english <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="English Learners",]
  if (dim(english)[1]!=0){
    four_year_grad_df[i+264,7:13] <- english[1,2:8]
  }

  homeless <- tbl_11[tbl_11$`Status of Students After Four Years of High School`=="Homeless",]
  if (dim(homeless)[1]!=0){
    four_year_grad_df[i+286,7:13] <- homeless[1,2:8]
  }

  all_students=NULL;female=NULL;male=NULL;american_indian=NULL;asian=NULL;
  black=NULL;hispanic=NULL;hawaiian=NULL;white=NULL;two_or_more=NULL;
  disabled=NULL;econ=NULL;english=NULL;homeless=NULL
  print(i)
}

write.csv(four_year_grad_df, file = "~/stem_edu/data/stem_edu/final/VDOE/four_year_grads.csv")
