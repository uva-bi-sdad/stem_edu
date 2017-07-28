# create comprehensive IPUMS data dictionary

setwd("data/stem_edu/original/IPUMS/origin")
naics <- readr::read_csv("2017_NAICS_Descriptions.csv")
ipums <- readr::read_csv("IPUMS_descriptions.csv")
soc <- readr::read_csv("SOC_Occupations.csv")

names(naics)
names(ipums)
names(soc)

colnames(soc)[6] <- "details"

ipums$details <- NA

ipums_data_dict <- rbind(ipums, soc, naics)

write.csv(ipums_data_dict, "ipums_data_dictionary.csv")
