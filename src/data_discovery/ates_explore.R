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
key_eduattn[1,3] <- "No high school diploma or GED"
class(key_eduattn$varcode)
class(ates$eduattn)
key_eduattn$varcode <- as.integer(key_eduattn$varcode)

#Merge data with key
ates_key <- left_join(ates, select(key_eduattn, varcode, code_desc), by = c("eduattn" = "varcode"))
head(ates_key)

#make code_desc wrap text for prettier plot
ates_key$code_desc <- str_wrap(ates_key$code_desc, width = 20)
ates_key$eduattn <- as.factor(ates_key$eduattn)
levels(ates_key$eduattn)
ates_key$code_desc <- factor(ates_key$code_desc, levels=levels(ates_key$eduattn),ordered=TRUE)

levels(ates_key$code_desc)

#Educational attainment histogram
ggplot(data = ates_key) +
  geom_bar(aes(code_desc)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Educational Attainment of Survey Respondents") +
  ylab("Count of Survey Respondents")


#Analysis of first (most important) certifications

