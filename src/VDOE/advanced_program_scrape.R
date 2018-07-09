# tbl_12: Advanced Program Information
load(file = "~/git/dashboard/data/VDOE/school_list_22.RData")

advanced_program_info_df <- rbind(school_list_22,school_list_22,school_list_22)
advanced_program_info_df$Year <- rep(NA,66)
advanced_program_info_df$AP_test_taken <- rep(NA,66)
advanced_program_info_df$AP_course_enrollment <- rep(NA,66)
advanced_program_info_df$Dual_enrollment <- rep(NA,66)
advanced_program_info_df$Governors_school_enrollment <- rep(NA,66)
advanced_program_info_df$IB_course_enrollment <- rep(NA,66)
advanced_program_info_df$Senior_enrolled_in_IB <- rep(NA,66)


for (i in 1:22){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",advanced_program_info_df$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')
  assign("tbl_12", rvest::html_table(tbls_school_i[12], header=TRUE, fill=TRUE)[[1]])
  #2014-15
  advanced_program_info_df[i,5:11] <- tbl_12[2:8,2]
  #2015-16
  advanced_program_info_df[i+22,5:11] <- tbl_12[2:8,3]
  #2016-17
  advanced_program_info_df[i+44,5:11] <- tbl_12[2:8,4]
  print(i)
}

write.csv(advanced_program_info_df, "~/stem_edu/data/stem_edu/final/VDOE/advanced_program_info.csv")
