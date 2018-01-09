# make sankeys and other plots for DSPG 2017 poster

library(dplyr)

# FORMAT DATA FOR SANKEYS --------------------------------------------------------------------------------------------------------------

ipums <- readr::read_csv("data/stem_edu/original/IPUMS/ipums.csv")
ipums_data_dict <- read.csv("data/stem_edu/original/IPUMS/ipums_data_dictionary.csv")

ipums[ipums == "N/A"] <- NA
ipums <- ipums %>% filter(occsoc > 0)

# rothwell stem score high/super/other stem classification
rothwell <- readr::read_csv("data/stem_edu/working/OccupationsbySTEMClassification.csv")
rothwell$Code <- gsub("-", "", rothwell$Code)
rothwell$Code <- substr(rothwell$Code, 1, nchar(rothwell$Code) - 3)
rothwell <- rothwell %>% group_by(Code) %>% slice(1)

# merge ipums with 6-digit rothwell soc codes to get stem classification (for part of ipums...)
ipums6 <- merge(ipums, rothwell[, c("Occupation","Code","STEM_classification")],
                by.x = "occsoc", by.y = "Code", all.x = TRUE)

table(ipums6$STEM_classification, useNA = 'ifany')

ipums6$occsoc5 <- substr(ipums6$occsoc, 1, 5)
rothwell$code5 <- substr(rothwell$Code, 1, 5)

# merge by 5 digit codes to salvage extra info..
rothwell5 <- rothwell %>% group_by(code5) %>% slice(1)

ipums6 <- merge(ipums6, rothwell5[, c("STEM_classification", "code5")],
                by.x = "occsoc5", by.y = "code5", all.x = TRUE)

table(ipums6$STEM_classification.y, useNA = 'ifany')
table(ipums6$STEM_classification.x, useNA = 'ifany')

# wait.
other <- unique(ipums6[ipums6$sankey_stem_classification == "Other", "occsoc"])
bls_soc_stem <- read.csv("data/stem_edu/original/Attachment_C_STEM.csv")
bls_soc_stem$X2010_SOC_code <- gsub("-", "", bls_soc_stem$X2010_SOC_code)
sum((other %in% bls_soc_stem$X2010_SOC_code) %in% TRUE) / length(other)

ipums6$occsoc_in_blsstem <- ipums6$occsoc %in% bls_soc_stem$X2010_SOC_code

# pick best category for each row based on contents of the 5 and 6 digit matches
ipums6$sankey_stem_classification <- ipums6$STEM_classification.x # first, assign 6 digit classification (most accurate)
ipums6[is.na(ipums6['sankey_stem_classification']), 'sankey_stem_classification'] <- ipums6[is.na(ipums6['sankey_stem_classification']), 'STEM_classification.y'] # second, assign 5 digit classification (more broad)
# fill na values
ipums6[is.na(ipums6['sankey_stem_classification']) & ipums6$occsoc_in_blsstem==TRUE, 'sankey_stem_classification'] <- "Other STEM"
ipums6[is.na(ipums6['sankey_stem_classification']) & ipums6$occsoc_in_blsstem==FALSE, 'sankey_stem_classification'] <- "non-STEM"
# correct 'other' classification
ipums6[ipums6$sankey_stem_classification=='Other' & ipums6$occsoc_in_blsstem==TRUE, 'sankey_stem_classification'] <- "Other STEM"
ipums6[ipums6$sankey_stem_classification=='Other' & ipums6$occsoc_in_blsstem==FALSE, 'sankey_stem_classification'] <- "non-STEM"

table(ipums6$sankey_stem_classification, useNA = 'ifany')

# aggregate detailed edu level
ipums6$educd_red <- ipums6$educd
ipums6$educd_red[ipums6$educd %in% c("No schooling completed","Nursery school, preschool", "Kindergarten",
                                     "Grade 1", "Grade 2", "Grade 3", "Grade 4",
                                     "Grade 5", "Grade 6", "Grade 7", "Grade 8")] <- "Less than high school"
ipums6$educd_red[ipums6$educd %in% c("Grade 9", "Grade 10", "Grade 11", "12th grade, no diploma")] <- "Some high school, no diploma"

# SANKEY NETWORK 1 (ONLY HIGH AND SUPER STEM OCCUPATIONS) ------------------------------------------------------------------------------------------
# only women
female <- ipums6 %>% filter(sex == "Female" & sankey_stem_classification %in% c("Super STEM", "High STEM"))
# female$occsoc <- substr(female$occsoc, 1, nchar(female$occsoc) - 1)

# sankey
table_sankey1 <- as.data.frame(table(as.character(female$educd_red),
                                     as.character(female$sankey_stem_classification)), stringsAsFactors = FALSE)
# table_sankey2 <- as.data.frame(table(as.character(female$degfield_stem),
                                     # as.character(female$occVT)), stringsAsFactors = FALSE)
# links <- rbind(table_sankey1, table_sankey2)
links <- table_sankey1

codes <- as.data.frame(as.integer(seq(0,12,1)))
colnames(codes) <- "number"
codes$name <- c(unique(links$Var1), unique(links$Var2))

links <- merge(links, codes, by.x = "Var1", by.y = "name", all.x = TRUE)
colnames(links)[4] <- "source"
links <- merge(links, codes, by.x = "Var2", by.y = "name", all.x = TRUE)
colnames(links)[5] <- "target"

name <- as.data.frame(codes$name, stringsAsFactors = FALSE)
colnames(name) <- "name"

library(networkD3)
library(magrittr)
sankey_data <- list(nodes = name, links = links)

setwd("output/sankey_diagrams/")

sankeyNetwork(Links = sankey_data$links,
              Nodes = sankey_data$nodes,
              Source = "source",
              Target = "target",
              Value = "Freq",
              NodeID = "name",
              units = "freq",
              fontSize = 16,
              nodeWidth = 30,
              fontFamily = "sans-serif") %>% saveNetwork('ipums_women_degreelevel_high.super.stem.occs.html')

# VENN DIAGRAM ----------------------------------------------------------------------------------------------------------------------------------
library(VennDiagram)
sh <- sum(rothwell$STEM_classification %in% c("Super STEM", "High STEM"))
bls <- length(unique(bls_soc_stem$X2010_SOC_code))
both <- sum(rothwell$Code[rothwell$STEM_classification %in% c("Super STEM", "High STEM")] %in% bls_soc_stem$X2010_SOC_code) + 1
ipums <- length(unique(ipums$occsoc))

grid.newpage()
draw.pairwise.venn(area1 = sh, area2 = bls, cross.area = both,
                   category = c("High or Super STEM", "BLS STEM"),
                   fill = c("lavender", "palegreen3"))

# POSTER SANKEY 2 (ALL OCCUPATION TYPES) --------------------------------------------------------------------------------------------------------------------------------------------
# only women
female <- ipums6 %>% filter(sex == "Female")
# female$occsoc <- substr(female$occsoc, 1, nchar(female$occsoc) - 1)

# sankey
table_sankey1 <- as.data.frame(table(as.character(female$educd_red),
                                     as.character(female$sankey_stem_classification)), stringsAsFactors = FALSE)
# table_sankey2 <- as.data.frame(table(as.character(female$degfield_stem),
# as.character(female$occVT)), stringsAsFactors = FALSE)
# links <- rbind(table_sankey1, table_sankey2)
links <- table_sankey1

codes <- as.data.frame(as.integer(seq(0,14,1)))
colnames(codes) <- "number"
codes$name <- c(unique(links$Var1), unique(links$Var2))

links <- merge(links, codes, by.x = "Var1", by.y = "name", all.x = TRUE)
colnames(links)[4] <- "source"
links <- merge(links, codes, by.x = "Var2", by.y = "name", all.x = TRUE)
colnames(links)[5] <- "target"

name <- as.data.frame(codes$name, stringsAsFactors = FALSE)
colnames(name) <- "name"

sankey_data <- list(nodes = name, links = links)

setwd("output/sankey_diagrams/")

sankeyNetwork(Links = sankey_data$links,
              Nodes = sankey_data$nodes,
              Source = "source",
              Target = "target",
              Value = "Freq",
              NodeID = "name",
              units = "freq",
              fontSize = 16,
              nodeWidth = 30,
              fontFamily = "sans-serif") %>% saveNetwork('ipums_women_degreelevel_all.stem.occs.html')




