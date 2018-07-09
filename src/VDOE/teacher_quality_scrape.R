# tbl 29: Provisionally licensed teachers
# tbl 30: Percentage of Core Academic Classes Taught by Teachers Not Meeting the Federal Definition of Highly Qualified
# tbl 31: Teacher Educational Attainment
load(file = "~/git/dashboard/data/VDOE/school_list_22.RData")

licensed_teachers_df <- school_list_22
licensed_teachers_df$Provisional_Special_Ed <- rep(NA,22)
licensed_teachers_df$Provisional <- rep(NA,22)
licensed_teachers_df$Year <- rep(NA,22)

for (i in 1:length(licensed_teachers_df$School)){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",licensed_teachers_df$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')
  assign("tbl_29", rvest::html_table(tbls_school_i[29], header=TRUE, fill=TRUE)[[1]])
  licensed_teachers_df$Year[i] <- tbl_29[1,2]
  licensed_teachers_df$Provisional_Special_Ed[i] <- tbl_29[2,2]
  licensed_teachers_df$Provisional[i] <- tbl_29[3,2]
  print(i)
  tbl_29=NULL
}

licensed_teachers_df <- rbind(licensed_teachers_df,licensed_teachers_df)
for (i in 23:length(licensed_teachers_df$School)){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",licensed_teachers_df$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')
  assign("tbl_29", rvest::html_table(tbls_school_i[29], header=TRUE, fill=TRUE)[[1]])
  licensed_teachers_df$Year[i] <- tbl_29[1,3]
  licensed_teachers_df$Provisional_Special_Ed[i] <- tbl_29[2,3]
  licensed_teachers_df$Provisional[i] <- tbl_29[3,3]
  print(i)
  tbl_29=NULL
}
save(licensed_teachers_df, file = "~/git/dashboard/data/VDOE/licensed_teachers_df.RData")



# tbl 30: Percentage of Core Academic Classes Taught by Teachers Not
# Meeting the Federal Definition of Highly Qualified

percentage_not_qualified_df <- rbind(school_list_22,school_list_22,school_list_22)
percentage_not_qualified_df$Year <- rep(NA,66)
percentage_not_qualified_df$This_School <- rep(NA,66)
percentage_not_qualified_df$Schools_in_Division <- rep(NA,66)
percentage_not_qualified_df$HighPoverty_in_Division <- rep(NA,66)
percentage_not_qualified_df$LowPoverty_in_Division <- rep(NA,66)
percentage_not_qualified_df$Schools_in_State <- rep(NA,66)
percentage_not_qualified_df$HighPoverty_in_State <- rep(NA,66)
percentage_not_qualified_df$LowPoverty_in_state <- rep(NA,66)


for (i in 1:22){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",licensed_teachers_df$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')
  assign("tbl_30", rvest::html_table(tbls_school_i[30], header=TRUE, fill=TRUE)[[1]])
  #2014-15
  percentage_not_qualified_df$Year[i] <- tbl_30[1,2]
  percentage_not_qualified_df$This_School[i] <- tbl_30[3,2]
  percentage_not_qualified_df$Schools_in_Division[i] <- tbl_30[5,2]
  percentage_not_qualified_df$HighPoverty_in_Division[i] <- tbl_30[6,2]
  percentage_not_qualified_df$LowPoverty_in_Division[i] <- tbl_30[7,2]
  percentage_not_qualified_df$Schools_in_State[i] <- tbl_30[9,2]
  percentage_not_qualified_df$HighPoverty_in_State[i] <- tbl_30[10,2]
  percentage_not_qualified_df$LowPoverty_in_state[i] <- tbl_30[11,2]
  #2015-16
  percentage_not_qualified_df$Year[i+22] <- tbl_30[1,3]
  percentage_not_qualified_df$This_School[i+22] <- tbl_30[3,3]
  percentage_not_qualified_df$Schools_in_Division[i+22] <- tbl_30[5,3]
  percentage_not_qualified_df$HighPoverty_in_Division[i+22] <- tbl_30[6,3]
  percentage_not_qualified_df$LowPoverty_in_Division[i+22] <- tbl_30[7,3]
  percentage_not_qualified_df$Schools_in_State[i+22] <- tbl_30[9,3]
  percentage_not_qualified_df$HighPoverty_in_State[i+22] <- tbl_30[10,3]
  percentage_not_qualified_df$LowPoverty_in_state[i+22] <- tbl_30[11,3]
  #2016-17
  percentage_not_qualified_df$Year[i+44] <- tbl_30[1,4]
  percentage_not_qualified_df$This_School[i+44] <- tbl_30[3,4]
  percentage_not_qualified_df$Schools_in_Division[i+44] <- tbl_30[5,4]
  percentage_not_qualified_df$HighPoverty_in_Division[i+44] <- tbl_30[6,4]
  percentage_not_qualified_df$LowPoverty_in_Division[i+44] <- tbl_30[7,4]
  percentage_not_qualified_df$Schools_in_State[i+44] <- tbl_30[9,4]
  percentage_not_qualified_df$HighPoverty_in_State[i+44] <- tbl_30[10,4]
  percentage_not_qualified_df$LowPoverty_in_state[i+44] <- tbl_30[11,4]
  print(i)
  tbl_30=NULL
}

save(percentage_not_qualified_df, file = "~/git/dashboard/data/VDOE/percentage_not_qualified_df.RData")



# tbl 31: Teacher Educational Attainment
teacher_ed_attainment_df <- rbind(school_list_22,school_list_22,school_list_22)
teacher_ed_attainment_df$Year <- rep(NA,66)
teacher_ed_attainment_df$Bachelors <- rep(NA,66)
teacher_ed_attainment_df$Masters <- rep(NA,66)
teacher_ed_attainment_df$Doctoral <- rep(NA,66)
teacher_ed_attainment_df$Other <- rep(NA,66)

for (i in 1:22){
  wl_school_i <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/",licensed_teachers_df$School[i]))
  tbls_school_i <- rvest::html_nodes(wl_school_i, 'table')
  assign("tbl_31", rvest::html_table(tbls_school_i[31], header=TRUE, fill=TRUE)[[1]])
  #2014-15
  teacher_ed_attainment_df$Year[i] <- tbl_31[2,1]
  teacher_ed_attainment_df$Bachelors[i] <- tbl_31[2,2]
  teacher_ed_attainment_df$Masters[i] <- tbl_31[2,3]
  teacher_ed_attainment_df$Doctoral[i] <- tbl_31[2,4]
  teacher_ed_attainment_df$Other[i] <- tbl_31[2,5]
  #2015-16
  teacher_ed_attainment_df$Year[i+22] <- tbl_31[3,1]
  teacher_ed_attainment_df$Bachelors[i+22] <- tbl_31[3,2]
  teacher_ed_attainment_df$Masters[i+22] <- tbl_31[3,3]
  teacher_ed_attainment_df$Doctoral[i+22] <- tbl_31[3,4]
  teacher_ed_attainment_df$Other[i+22] <- tbl_31[3,5]
  #2016-17
  teacher_ed_attainment_df$Year[i+44] <- tbl_31[4,1]
  teacher_ed_attainment_df$Bachelors[i+44] <- tbl_31[4,2]
  teacher_ed_attainment_df$Masters[i+44] <- tbl_31[4,3]
  teacher_ed_attainment_df$Doctoral[i+44] <- tbl_31[4,4]
  teacher_ed_attainment_df$Other[i+44] <- tbl_31[4,5]
  print(i)
  tbl_31=NULL
}

write.csv(teacher_ed_attainment_df, "~/stem_edu/data/stem_edu/final/VDOE/teacher_quality.csv")
