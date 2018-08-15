wl <- xml2::read_html("http://schoolquality.virginia.gov/schools/washington-lee-high")
tbls <- rvest::html_nodes(wl, 'table')

for (i in 1:length(tbls)) {
  assign(paste0("tbl_", i), rvest::html_table(tbls[i], header=TRUE, fill=TRUE)[[1]])
}


##### Example: multiple schools w/ same name can cause confusion
wl <- xml2::read_html("http://schoolquality.virginia.gov/schools/fairfield-elementary")
tbls <- rvest::html_nodes(wl, 'table')

for (i in 1:length(tbls)) {
  assign(paste0("tbl_", i), rvest::html_table(tbls[i], header=TRUE, fill=TRUE)[[1]])
}



### Scrape entire list of school names
for (i in 1:61) {
  wl_lists <- xml2::read_html(paste0("http://schoolquality.virginia.gov/schools/page/", i))
  lists <- rvest::html_nodes(wl_lists, 'table')

  assign(paste0("list_", i), rvest::html_table(lists, header=TRUE, fill=TRUE)[[1]])

  print(i)
}
## make list of all 1822 public schools
list_of_schools <- rbind(list_1, list_2, list_3, list_4, list_5, list_6, list_7, list_8,
                         list_9, list_10, list_11, list_12, list_13, list_14, list_15,
                         list_16, list_17, list_18, list_19, list_20, list_21, list_22,
                         list_23, list_24, list_25, list_26, list_27, list_28, list_29,
                         list_30, list_31, list_32, list_33, list_34, list_35, list_36,
                         list_37, list_38, list_39, list_40, list_41, list_42, list_43,
                         list_44, list_45, list_46, list_47, list_48, list_49, list_50,
                         list_51, list_52, list_53, list_54, list_55, list_56, list_57,
                         list_58, list_59, list_60, list_61)

list_of_schools$SchoolName <- list_of_schools$School

## Convert into what's needed for the url
list_of_schools$School <- gsub(pattern = "'", replacement = "", x=list_of_schools$School)
list_of_schools$School <- gsub(pattern = "[.]", replacement = "-", x=list_of_schools$School)
list_of_schools$School <- gsub(pattern = " ", replacement = "-", x=list_of_schools$School)
list_of_schools$School <- gsub(pattern = "--", replacement = "-", x=list_of_schools$School)
list_of_schools$School <- gsub(pattern = "/", replacement = "", x=list_of_schools$School)


## Sort out schools that have the same name as another in the system
name_freq <- as.data.frame(table(list_of_schools$School))
repeated_names <- name_freq[name_freq$Freq>1,]

## a loop to put numbers in the urls of repeated schools
repeat_list <-NULL
for (k in 1:length(repeated_names$Var1)) {
  school_df <- list_of_schools[list_of_schools$School==repeated_names$Var1[k],]
  school_df <- school_df[order(school_df$Division),]
  for (j in 2:length(school_df$School)) {
    school_df$School[j] <- paste0(school_df$School[j],"-",j)
  }
  repeat_list <- rbind(repeat_list,school_df)
  print(k)
}

# get all nonrepeated schools
library(dplyr)
nonrepeat_schools <- anti_join(list_of_schools,repeat_list,by="School")

# final list with proper url format for all schools
school_list_final <- rbind(nonrepeat_schools,repeat_list)


write.csv(school_list_final, "~/stem_edu/data/stem_edu/final/VDOE/school_quality_profiles.csv")
