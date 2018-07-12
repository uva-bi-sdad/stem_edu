#############################################################
###             ATES Data - Exploratory Code              ###
###   *Clean up code names and variable names cook books  ###
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
View(codenames)

#Write codenames to new csv in working
write.csv(x = codenames,
          file = "data/stem_edu/working/DSPG18/ATES/codenames.csv",
          row.names = FALSE)


#Clean up the varnames file

#remove quotes around variable descriptions and leading digits
varnames$`Description` <- str_remove(varnames$`Description`, pattern = '"$') #removes end quote
varnames$`Description` <- str_remove(varnames$`Description`, pattern = '^"') #removes start quote
varnames$`Description` <- str_remove(varnames$Description, pattern = )
# delete one or 2 digits and .
varnames$Description <- str_remove(varnames$Description, pattern = '^\\d{1,2}.')
#remove white space
varnames$Description <- trimws(varnames$Description)
#rename columns
names(varnames) <- c("varname", "var_desc")

#Write varnames to new csv
write.csv(x = varnames,
          file = "data/stem_edu/working/DSPG18/ATES/varnames.csv",
          row.names = FALSE)

#write ATES file to working
write.csv(x = ates,
          file = "data/stem_edu/working/DSPG18/ATES/ates.csv",
          row.names = FALSE)

#Note the varnames and codenames have fewer vars than ates data (incomplete)
