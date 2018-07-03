# tbl_15: Advanced Placement Participation and Achievement
load(file = "~/git/dashboard/data/VDOE/school_list_22.RData")

library(dplyr)
ap_achievement_df <- bind_rows(replicate(3, school_list_22, simplify = FALSE))
ap_achievement_df$Year <- c(rep("2013-14",22),rep("2014-15",22),rep("2015-16",22))
ap_achievement_df$Student_Type <- c(rep("All_Students",22))
ap_achievement_df$Number_test_takers <- rep(NA,length(ap_achievement_df$SchoolName))
ap_achievement_df$Number_tests_taken <- rep(NA,length(ap_achievement_df$SchoolName))
ap_achievement_df$Number_tests_qualifying_scores <- rep(NA,length(ap_achievement_df$SchoolName))
ap_achievement_df$percentage_passed <- rep(NA,length(ap_achievement_df$SchoolName))


for (i in 1:22){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",school_list_22$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')

  assign("tbl_15", rvest::html_table(tbls_school_i[15], header=TRUE, fill=TRUE)[[1]])

  all_students <- tbl_15[tbl_15[,1]=="All Students",]
  if (dim(all_students)[1]!=0){
    ap_achievement_df[i,7:10] <- all_students[1,2:5]
    ap_achievement_df[i+22,7:10] <- all_students[2,2:5]
    ap_achievement_df[i+44,7:10] <- all_students[3,2:5]
  }
  all_students=NULL
  print(i)
}

write.csv(ap_achievement_df, "~/stem_edu/data/stem_edu/final/VDOE/ap_achievement.csv")
