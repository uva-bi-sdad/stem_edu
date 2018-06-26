library("dplyr")
library("readr")
library("pacman")
library(stringr)
library(ggplot2)

#pacman loads many libraries at once, auto downloads if you don't have it
pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)

# Import Data files = CSVs, save as data tables because data tables can be merged
# quicker than dataframes using fread function
hd2012_2017 <- data.table::fread("data/stem_edu/working/DSPG18/IPEDS/hd2012_2017.csv")
ic2012_2017 <- fread("data/stem_edu/working/DSPG18/IPEDS/ic2012_2017.csv")
efa2012_2016 <- fread("data/stem_edu/working/DSPG18/IPEDS/efa2012_2016.csv")
efb2012_2016 <- fread("data/stem_edu/working/DSPG18/IPEDS/efb2012_2016.csv")
efaDist2012_2016 <- fread("data/stem_edu/working/DSPG18/IPEDS/efaDist2012_2016.csv")

#Import data key for SECTOR Variable
hdkey <- read_excel("./data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2016.xlsx",
                    sheet = 4)
hdkeySec <- filter(hdkey, varname == "SECTOR")
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

# Change valuelabel to wrap
hd2012_2017_key$valuelabel2 <- str_wrap(hd2012_2017_key$valuelabel, width = 25)

#INCLUDE in PowerPoint: Sector bar graph for 2016 only
ggplot(data = filter(hd2012_2017_key, YEAR == 2016)) +
  geom_bar(aes(valuelabel2), fill = 'blue') +
  theme(text = element_text(size = 15), #size of x-axis label text
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  xlab("Sector of Institutions") +
  ylab("Count of Institutions") +
  ggtitle("2016 Count of US Institutions by Sector")


#Merge data key for ICLEVEL Variable
# From before: hdkey <- read_excel("./data/stem_edu/original/IPEDS_Data/1_Institutional_Char_Directory_Info/hd2016.xlsx",
#                    sheet = 4)
hdkeyLev <- filter(hdkey, varname == "ICLEVEL")
hdkeyLev$codevalue <- as.numeric(hdkeyLev$codevalue)

#Merge data with key
hd2012_2017_key <- left_join(as.data.frame(hd2012_2017), select(hdkeyLev, codevalue, valuelabel), by = c("ICLEVEL" = "codevalue"))
head(hd2012_2017_key)

#Plot of ICLEVEL facet wrap Year: notice little change over years
ggplot(data = hd2012_2017_key) +
  geom_bar(aes(valuelabel)) +
  facet_wrap(~YEAR) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Level of Institution") +
  ylab("Count of Institutions")

# Change valuelabel to wrap
hd2012_2017_key$valuelabel2 <- str_wrap(hd2012_2017_key$valuelabel, width = 20)

#INCLUDE in PowerPoint: ICLEVEL bar graph for 2016 only
ggplot(data = filter(hd2012_2017_key, YEAR == 2016)) +
  geom_bar(aes(valuelabel2), fill = 'green') +
  theme(text = element_text(size = 20), #size of axis label text
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  xlab("Level of Institutions") +
  ylab("Count of Institutions") +
  ggtitle("2016 Counts of US Institutions by Level")



