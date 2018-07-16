#############################################################
###             ATES Data - Exploratory Code              ###
###             *Plots of some ATES variables             ###
###                   Emily Sheen                         ###
#############################################################
library("dplyr")
library("readr")
library("pacman")
library(stringr)
library(ggplot2)
library(readxl)
library(data.table)

#Load ATES data and code books from working
ates <- fread("./data/stem_edu/working/DSPG18/ATES/ates.csv")
varnames <- fread("./data/stem_edu/working/DSPG18/ATES/varnames.csv")
codenames <- fread("./data/stem_edu/working/DSPG18/ATES/codenames.csv")

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

#Educational attainment histogram
ggplot(data = ates_key) +
  geom_bar(aes(code_desc)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Educational Attainment of Survey Respondents") +
  ylab("Count of Survey Respondents")



#Histogram for field of study highest degree
key_edufos <- filter(codenames, varname == "EDUFOS")
class(key_edufos$varcode)
class(ates$edufos)
key_edufos$varcode <- as.integer(key_edufos$varcode)
  #Merge data with key
ates_key <- left_join(ates, select(key_edufos, varcode, code_desc), by = c("edufos" = "varcode"))
head(ates_key)
  #make code_desc wrap text for prettier plot
ates_key$code_desc <- str_wrap(ates_key$code_desc, width = 50)
  #Educational attainment histogram
ggplot(data = ates_key) +
  geom_bar(aes(code_desc)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Field of Study of Highest Degree") +
  ylab("Count of Survey Respondents") +
  ggtitle("Fields of Study of Survey Respondents")


levels(ates$cnnum)
length(which(ates$cnnum >= 1))

#Histogram for category of first certification
key_cnfieldcat1 <- filter(codenames, varname == "CNFIELDCAT1")
class(key_cnfieldcat1$varcode)
class(ates$CNFIELDCAT1)
key_cnfieldcat1$varcode <- as.integer(key_cnfieldcat1$varcode)
#Merge data with key
ates_key <- left_join(ates, select(key_cnfieldcat1, varcode, code_desc), by = c("CNFIELDCAT1" = "varcode"))
head(ates_key)
#make code_desc wrap text for prettier plot
ates_key$code_desc <- str_wrap(ates_key$code_desc, width = 50)
#filter out valid skips
ates_key <- filter(ates_key, CNFIELDCAT1 > -1)
#Educational attainment histogram
ggplot(data = ates_key) +
  geom_bar(aes(code_desc)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Field Category of First Certification") +
  ylab("Count of Survey Respondents") +
  ggtitle("Fields of Certifications for Survey Respondents")


#Histogram for whether first certification req by law
key_cnprov1 <- filter(codenames, varname == "CNPROV1")
class(key_cnprov1$varcode)
class(ates$CNPROV1)
key_cnprov1$varcode <- as.integer(key_cnprov1$varcode)
#Merge data with key
ates_key <- left_join(ates, select(key_cnprov1, varcode, code_desc), by = c("CNPROV1" = "varcode"))
head(ates_key)
#make code_desc wrap text for prettier plot
ates_key$code_desc <- str_wrap(ates_key$code_desc, width = 50)
#filter out valid skips
ates_key <- filter(ates_key, CNFIELDCAT1 > -1)
#Educational attainment histogram
ggplot(data = ates_key) +
  geom_bar(aes(x = code_desc, y = (..count..)/sum(..count..))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("First Certification required by Law?") +
  ylab("Proportion of Respondents") +
  ggtitle("Is your most important certification required by law?")
