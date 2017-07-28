# setwd("~/git/stem_edu/data/stem_edu/original/IPUMS/")
# ipums <- read.delim(file = "usa_00002.dat", header = TRUE, sep = "\t")
ipums <- readr::read_csv("data/stem_edu/original/IPUMS/ipums.csv")
ipums_data_dict <- read.csv("data/stem_edu/original/IPUMS/ipums_data_dictionary.csv")

ipums[ipums == "N/A"] <- NA

stem_soc <- read.csv("data/stem_edu/original/IPUMS/Attachment_C_STEM.csv")
stem_soc$X2010.SOC.code <- gsub("-", "", stem_soc$X2010.SOC.code)
stem_soc$X2010.SOC.code <- substr(stem_soc$X2010.SOC.code, 1, nchar(stem_soc$X2010.SOC.code) - 1)

library(dplyr)

female <- ipums %>% filter(sex == "Female")
female$occsoc <- substr(female$occsoc, 1, nchar(female$occsoc) - 1)
female$occsoc_stem <- female$occsoc %in% stem_soc$X2010.SOC.code
sum(female$occsoc_stem)

table(female$educd)
female$educd_red <- female$educd
female$educd_red[female$educd %in% c("No schooling completed","Nursery school, preschool", "Kindergarten",
                                     "Grade 1", "Grade 2", "Grade 3", "Grade 4",
                                     "Grade 5", "Grade 6", "Grade 7", "Grade 8")] <- "Less than high school"
female$educd_red[female$educd %in% c("Grade 9", "Grade 10", "Grade 11", "12th grade, no diploma")] <- "Some high school, no diploma"

female$degfield_stem <- NA
female$degfield_stem[female$degfield %in% c("Business",
                                            "Public Affairs, Policy, and Social Work",
                                            "Communications",
                                            "English Language, Literature, and Composition",
                                            "Linguistics and Foreign Languages",
                                            "Interdisciplinary and Multi-Disciplinary Studies (General)", #
                                            "Fine Arts",
                                            "Liberal Arts and Humanities",
                                            "Theology and Religious Vocations",
                                            "Physical Fitness, Parks, Recreation, and Leisure",
                                            "Philosophy and Religious Studies")] <- "non-STEM degree"
female$degfield_stem[female$degfield %in% c("Psychology",
                                            "Education Administration and Teaching",
                                            "Medical and Health Sciences and Services",
                                            "Physical Sciences",
                                            "Biology and Life Sciences",
                                            "Family and Consumer Sciences", #
                                            "Social Sciences",
                                            "Computer and Information Sciences",
                                            "Criminal Justice and Fire Protection",
                                            "Mathematics and Statistics",
                                            "Engineering Technologies",
                                            "Engineering",
                                            "History", #
                                            "Agriculture",
                                            "Nuclear, Industrial Radiology, and Biological Technologies",
                                            "Architecture",
                                            "Communication Technologies",
                                            "Library Science",
                                            "Law", #
                                            "Cosmetology Services and Culinary Arts", #
                                            "Electrical and Mechanic Repairs and Technologies",
                                            "Transportation Sciences and Technologies",
                                            "Construction Services",
                                            "Military Technologies",
                                            "Environment and Natural Resources", #
                                            "Area, Ethnic, and Civilization Studies")] <- "STEM degree"
female$degfield_stem[female$educd %in% c("No schooling completed","Nursery school, preschool", "Kindergarten",
                                         "Grade 1", "Grade 2", "Grade 3", "Grade 4",
                                         "Grade 5", "Grade 6", "Grade 7", "Grade 8",
                                         "Grade 9", "Grade 10", "Grade 11", "12th grade, no diploma", NA)] <- "No degree"

female$occVT <- NA
female$occVT[!female$occsoc %in% stem_soc$X2010.SOC.code] <- "non-STEM Career"
female$occVT[female$occsoc %in% stem_soc$X2010.SOC.code] <- "Other STEM Career"
# female$occVT[female$occsoc == 17208] <- "Environmental Engineer"
# female$occVT[female$occsoc == 15113] <- "Software Developer"
# female$occVT[female$occsoc == 29202] <- "Dental Hygienist"

table_sankey1 <- as.data.frame(table(as.character(female$educd_red),
                                     as.character(female$degfield_stem)), stringsAsFactors = FALSE)
table_sankey2 <- as.data.frame(table(as.character(female$degfield_stem),
                                     as.character(female$occVT)), stringsAsFactors = FALSE)
links <- rbind(table_sankey1, table_sankey2)

codes <- as.data.frame(as.integer(seq(0,15,1)))
colnames(codes) <- "number"
codes$name <- c(unique(links$Var1), "non-STEM Career","Other STEM Career")

links <- merge(links, codes, by.x = "Var1", by.y = "name", all.x = TRUE)
colnames(links)[4] <- "source"
links <- merge(links, codes, by.x = "Var2", by.y = "name", all.x = TRUE)
colnames(links)[5] <- "target"

name <- as.data.frame(codes$name, stringsAsFactors = FALSE)
colnames(name) <- "name"

library(networkD3)
sankey_data <- list(nodes = name, links = links)

sankeyNetwork(Links = sankey_data$links, Nodes = sankey_data$nodes, Source = "source",
              Target = "target", Value = "Freq", NodeID = "name",
              units = "freq", fontSize = 16, nodeWidth = 30)

# focus on  3 careers ####################################################################################
careers <- female %>% filter(occsoc %in% c(17208, 15113, 29202))

careers$deg_type <- NA
careers$deg_type <- paste(careers$educd, careers$degfield_stem)

table_sankey1 <- as.data.frame(table(as.character(careers$educd_red),
                                     as.character(careers$degfield_stem)), stringsAsFactors = FALSE)
table_sankey2 <- as.data.frame(table(as.character(careers$degfield_stem),
                                     as.character(careers$occVT)), stringsAsFactors = FALSE)
# table_sankey3 <- as.data.frame(table(as.character(careers$deg_type),
#                                      as.character(careers$occVT)), stringsAsFactors = FALSE)

links <- rbind(table_sankey1, table_sankey2)

codes <- as.data.frame(as.integer(seq(0,16,1)))
colnames(codes) <- "number"
codes$name <- c(unique(links$Var1), "Environmental Engineer","Software Developer","Dental Hygienist")

links <- merge(links, codes, by.x = "Var1", by.y = "name", all.x = TRUE)
colnames(links)[4] <- "source"
links <- merge(links, codes, by.x = "Var2", by.y = "name", all.x = TRUE)
colnames(links)[5] <- "target"

name <- as.data.frame(codes$name, stringsAsFactors = FALSE)
colnames(name) <- "name"

sankey_data <- list(nodes = name, links = links)

sankeyNetwork(Links = sankey_data$links, Nodes = sankey_data$nodes, Source = "source",
              Target = "target", Value = "Freq", NodeID = "name",
              units = "freq", fontSize = 16, nodeWidth = 30)
