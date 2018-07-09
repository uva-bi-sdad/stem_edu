## This is a script to scrape the accreditation table from the VDOE webpage.
library(rvest)
library(xml2)

#school_list_final <- read.csv("~/stem_edu/data/stem_edu/final/VDOE/22_schools_list.csv")
school_list_final <- read.csv("~/stem_edu/data/stem_edu/final/VDOE/all_high_school_level.csv")
school_list_final$SchoolName <- gsub(" ", "-", school_list_final$SchoolName, fixed = TRUE)
accreditation_df <- c("aaa_State_Benchmark", 75, 75, 75, 75, 75, 75, NA,
                      70,70,70,70,70,70,NA, 70,70,70,70,70,70,NA, 70,70,70,70,70,70,NA,
                      85,85,85,85,85,85,NA)
accreditation_df <- as.data.frame(t(accreditation_df))


for (i in 1:nrow(school_list_final))
{
  wl_accreditation <- read_html(paste("http://schoolquality.virginia.gov/schools/",school_list_final$SchoolName[i],"#fndtn-desktopTabs-accountability",sep=""))
  tbls_school_i <- html_nodes(wl_accreditation, 'table')
  #tbls_school_i <- html_nodes(wl_accreditation, 'tbody td , th')
  # acc_info <- html_nodes(wl_accreditation, '#chart-accreditationstatus h4')
  # acc_info <- wl_accreditation %>% html_nodes('#chart-accreditationstatus h4') %>% html_text()

  assign("tbl_1", html_table(tbls_school_i[1], header=TRUE, fill=TRUE)[[1]])

  data_i <- c(school_list_final$SchoolName[i],
              tbl_1[3,3], tbl_1[3,4], tbl_1[3,5], tbl_1[3,6], tbl_1[3,7], tbl_1[3,8], tbl_1[3,9],
              tbl_1[4,3], tbl_1[4,4], tbl_1[4,5], tbl_1[4,6], tbl_1[4,7], tbl_1[4,8], tbl_1[4,9],
              tbl_1[5,3], tbl_1[5,4], tbl_1[5,5], tbl_1[5,6], tbl_1[5,7], tbl_1[5,8], tbl_1[5,9],
              tbl_1[6,3], tbl_1[6,4], tbl_1[6,5], tbl_1[6,6], tbl_1[6,7], tbl_1[6,8], tbl_1[6,9],
              tbl_1[7,3], tbl_1[7,4], tbl_1[7,5], tbl_1[7,6], tbl_1[7,7], tbl_1[7,8], tbl_1[7,9])
  data_i <- as.data.frame(t(data_i))
  accreditation_df <- rbind(accreditation_df, data_i)
  data_i=NULL;tbl_1=NULL;tbl_2=NULL
  print(i)
}

colnames(accreditation_df) <- c("School","eng_14_15_1yr","eng_14_15_3yr","eng_15_16_1yr",
                                "eng_15_16_3yr","eng_16_17_1yr","eng_16_17_3yr","eng_met_bench",
                                "math_14_15_1yr","math_14_15_3yr","math_15_16_1yr","math_15_16_3yr",
                                "math_16_17_1yr","math_16_17_3yr","math_met_bench",
                                "hist_14_15_1yr","hist_14_15_3yr","hist_15_16_1yr","hist_15_16_3yr",
                                "hist_16_17_1yr","hist_16_17_3yr","hist_met_bench",
                                "sci_14_15_1yr","sci_14_15_3yr","sci_15_16_1yr","sci_15_16_3yr",
                                "sci_16_17_1yr","sci_16_17_3yr","sci_met_bench",
                                "grad_14_15_1yr","grad_14_15_3yr","grad_15_16_1yr","grad_15_16_3yr",
                                "grad_16_17_1yr","grad_16_17_3yr","grad_met_bench")

write.csv(accreditation_df, "~/stem_edu/data/stem_edu/final/VDOE/accredidation.csv")
