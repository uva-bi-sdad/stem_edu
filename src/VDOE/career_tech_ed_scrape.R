# tbl_14: Career & Technical Education
load(file = "~/git/dashboard/data/VDOE/school_list_22.RData")

career_tech_ed_df <- rbind(school_list_22,school_list_22,school_list_22)
career_tech_ed_df$Year <- rep(NA,66)
career_tech_ed_df$NOCTI_assessments <- rep(NA,66)
career_tech_ed_df$State_Licensures <- rep(NA,66)
career_tech_ed_df$Industry_Certification <- rep(NA,66)
career_tech_ed_df$Workplace_Readiness <- rep(NA,66)
career_tech_ed_df$Total_credentials_earned <- rep(NA,66)
career_tech_ed_df$Students_earning_1orMore <- rep(NA,66)
career_tech_ed_df$CTE_Completers <- rep(NA,66)


for (i in 1:22){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",career_tech_ed_df$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')
  assign("tbl_14", rvest::html_table(tbls_school_i[14], header=TRUE, fill=TRUE)[[1]])
  #2014-15
  career_tech_ed_df[i,5:12] <- tbl_14[c(2,3,6,9,12,15,18,21),3]
  #2015-16
  career_tech_ed_df[i+22,5:12] <- tbl_14[c(2,3,6,9,12,15,18,21),4]
  #2016-17
  career_tech_ed_df[i+44,5:12] <- tbl_14[c(2,3,6,9,12,15,18,21),5]
  print(i)
}

write.csv(career_tech_ed_df, "~/stem_edu/data/stem_edu/final/VDOE/career_tech_ed.csv")
