#########################################################
###           ATES Data - Exploratory Code            ###
###                   Emily Sheen                     ###
#########################################################
library("dplyr")
library("readr")
library("pacman")
library(stringr)
library(ggplot2)
library(readxl)

#Load ATES data and code books
load("./data/stem_edu/original/ATES/ates_pu_pert.rdata")
ates <- ates_pu_pert
rm(ates_pu_pert)
varnames <- read_excel("./data/stem_edu/original/ATES/variable_names.xlsx",
                    sheet = 1)
codenames <- fread("./data/stem_edu/original/ATES/ates_code_values.csv")
length(unique(codenames$`Variable Name`))
length(colnames(ates)) #Not the same number of variables, may have missing

#Clean up the codenames file

#remove trailing F's from variable names and leading digits in code desc.
codenames$varname <- str_remove(codenames$`Variable Name`, pattern = 'F$')
# delete `-` 0 or 1 times
codenames$`Code Description` <- str_remove(codenames$`Code Description`, pattern = '^-?\\d{1,4}')
codenames$varcode <- codenames$`Variable Code`
codenames$code_desc <- trimws(codenames$`Code Description`)
codenames <- codenames[, -c(1,2,3)]

#Note the varnames and codenames have fewer vars than ates data (incomplete)

#Histogram for educational attainment of survey participants
key_eduattn <- filter(codenames, varname == "EDUATTN")
hdkeySec$codevalue <- as.numeric(hdkeySec$codevalue)

#Merge data with key
hd2012_2017_key <- left_join(as.data.frame(hd2012_2017), select(hdkeySec, codevalue, valuelabel), by = c("SECTOR" = "codevalue"))
head(hd2012_2017_key)

#Sector histogram: Not helpful, notice 99 = NA code
ggplot(data = hd2012_2017) +
  geom_histogram(aes(SECTOR), bins = 100) +
  facet_wrap(~YEAR)

hdSec <- filter(hd2012_2017, hd2012_2017$SECTOR != 99)

#Plot of SECTOR facet wrap Year
ggplot(data = hd2012_2017_key) +
  geom_bar(aes(valuelabel)) +
  facet_wrap(~YEAR) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sector of Institutions") +
  ylab("Count of Institutions")


#Analysis of first (most important) certifications

