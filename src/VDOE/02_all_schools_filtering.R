library(dplyr)

schools_list <- read.csv("~/stem_edu/data/stem_edu/final/VDOE/all_schools_list.csv")

high <- filter(schools_list, grepl("High", SchoolName))
secondary <- filter(schools_list, grepl("Secondary", SchoolName))
combined <- filter(schools_list, grepl("Combined", SchoolName))
collegiate <- filter(schools_list, grepl("Green Run Collegiate", SchoolName))
career_ac <- filter(schools_list, grepl("Career Academy", SchoolName))

filtered <- rbind(high, secondary, combined, collegiate, career_ac)

write.csv(filtered, "~/stem_edu/data/stem_edu/final/VDOE/all_high_school_level.csv")
