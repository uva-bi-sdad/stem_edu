library(dplyr)
library(reshape2)
library(data.table)
library(dtplyr)
library(stringr)
library(xlsx)
library(readr)



dat <- read_csv("data/stem_edu/working/DSPG18/ATES/ates.csv", col_types = types)
codenames <- read_csv("data/stem_edu/working/DSPG18/ATES/codenames.csv")
varnames <- read_csv("data/stem_edu/working/DSPG18/ATES/varnames.csv")


types = c(rep('c', 2), rep(x = 'd', 352))
types[!str_detect(string = names(dat),
                  pattern = '(basmid|HHM(AGE|ENRL|SEX|GRD))')] = '_'
types = str_c(types, collapse = '')

dat = read_csv("data/stem_edu/working/DSPG18/ATES/ates.csv",
               col_types = types) %>%
  data.table() %>%
  melt.data.table(id.vars = 'basmid') %>%
  filter(value != -1) %>%
  mutate(HHMID = str_sub(variable, start = -1L),
         variable = str_sub(variable, end = -2L)) %>%
  dcast.data.table(formula = basmid + HHMID ~ variable)



chk = dat %>%
  filter(basmid %in% basmid[2]) #%>%
# filter(HHMID == 1)

ates_household_data <- dat

levels(chk$variable)

write.csv(x = ates_household_data, file = "data/stem_edu/working/DSPG18/ATES/ates_household_variables.csv")




####################################################################

## This code below is used to obtain demographic variables and form a sub dataset from our master dataset

ates <- read.csv("data/stem_edu/working/DSPG18/ATES/ates.csv")
ates_demographic_data <- select(ates, basmid, eduattn, xxsex, RACEETH2, agecat)
write.csv(x = ates_household_data, file = "data/stem_edu/working/DSPG18/ATES/ates_demographic_variables.csv")


####################################################################

## The following code would be used to obtain certification data for all survey respondents


types = c(rep('c', 2), rep(x = 'd', 352))

data <- read_csv("data/stem_edu/working/DSPG18/ATES/ates.csv")
codenames <- read_csv("data/stem_edu/working/DSPG18/ATES/codenames.csv")
varnames <- read_csv("data/stem_edu/working/DSPG18/ATES/varnames.csv")



types[!str_detect(string = names(data),
                  pattern = '(basmid|^CN(FIELD|INVALID|PROV|REVOKE|YEAR|RP|CURR|USE ))')] = '_'
types = str_c(types, collapse = '')

data = read_csv("data/stem_edu/working/DSPG18/ATES/ates.csv",
               col_types = types) %>%
  data.table() %>%
  melt.data.table(id.vars = 'basmid') %>%
  filter(value != -1) %>%
  mutate(CNMAIN = str_sub(variable, start = -1L),
         variable = str_sub(variable, end = -2L)) %>%
  dcast.data.table(formula = basmid + CNMAIN ~ variable)



chk = data %>%
  filter(basmid %in% basmid[2]) #%>%
# filter(CNNUM == 1)

ates_certifications_data <- data

levels(chk$variable)

write.csv(x = ates_certifications_data, file = "data/stem_edu/working/DSPG18/ATES/ates_certifications_variables.csv")



