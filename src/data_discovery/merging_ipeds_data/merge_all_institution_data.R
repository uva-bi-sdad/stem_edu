#########################################################
###  IPEDS Data - Code to Merge All institution data  ###
###  Fall Enrollment & Institutional characteristics  ###
###      Unique observations are UNITID + YEAR        ###
###                   Emily Sheen                     ###
#########################################################

library("dplyr")
library("readr")
library("pacman")
library(stringr)
#pacman loads many libraries at once, auto downloads if you don't have it
pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)

# Import Data files = CSVs, save as data tables because data tables can be merged
# quicker than dataframes using fread function
hd2012_2017 <- data.table::fread("data/stem_edu/working/DSPG18/IPEDS/hd2012_2017.csv")
ic2012_2017 <- fread("data/stem_edu/working/DSPG18/IPEDS/ic2012_2017.csv")
efa2012_2016 <- fread("data/stem_edu/working/DSPG18/IPEDS/efa2012_2016.csv")
efb2012_2016 <- fread("data/stem_edu/working/DSPG18/IPEDS/efb2012_2016.csv")
efaDist2012_2016 <- fread("data/stem_edu/working/DSPG18/IPEDS/efaDist2012_2016.csv")

#Change colnames to lowercase for use in SQL server
names(hd2012_2017) <- str_to_lower(names(hd2012_2017))
names(ic2012_2017) <- str_to_lower(names(ic2012_2017))
names(efa2012_2016) <- str_to_lower(names(efa2012_2016))
names(efb2012_2016) <- str_to_lower(names(efb2012_2016))
names(efaDist2012_2016) <- str_to_lower(names(efaDist2012_2016))

# write out table to db and delete loaded table
# con_db is special function for lab to create database connection
conn = con_db(dbname = 'stem_edu', pass = get_my_password())

output = dbWriteTable(conn = conn,
                      name = c('ipeds', 'hd2012_2017'), #ipeds = SCHEMA
                      value = hd2012_2017,
                      row.names = FALSE,
                      overwrite = TRUE)
output = dbWriteTable(conn = conn,
                      name = c('ipeds', 'ic2012_2017'), #ipeds = SCHEMA
                      value = ic2012_2017,
                      row.names = FALSE,
                      overwrite = TRUE)
output = dbWriteTable(conn = conn,
                      name = c('ipeds', 'efa2012_2016'), #ipeds = SCHEMA
                      value = efa2012_2016,
                      row.names = FALSE,
                      overwrite = TRUE)
output = dbWriteTable(conn = conn,
                      name = c('ipeds', 'efb2012_2016'), #ipeds = SCHEMA
                      value = efb2012_2016,
                      row.names = FALSE,
                      overwrite = TRUE)
output = dbWriteTable(conn = conn,
                      name = c('ipeds', 'efaDist2012_2016'), #ipeds = SCHEMA
                      value = efaDist2012_2016,
                      row.names = FALSE,
                      overwrite = TRUE)

# use SQL to join tables
# DBI is the library that talks to databases, conn = database connection


# Realize that merging the tables is a pain, will keep separate tables to do analysis




#Send SQL query to merge the 5 tables into one master table
DBI::dbCreateTable(conn,
  'SELECT hd2012_2017.unitid, hd2012_2017.instnm, hd2012_2017.addr, hd2012_2017.city,
  hd2012_2017.stabbr, hd2012_2017.zip, hd2012_2017.fips, hd2012_2017.sector,
                 hd2012_2017.iclevel, hd2012_2017.hloffer, hd2012_2017.ugoffer,
                 hd2012_2017.groffer, hd2012_2017.hdegofr1, hd2012_2017.deggrant,
                 hd2012_2017.instcat, hd2012_2017.countycd, hd2012_2017.longitud,
                 hd2012_2017.latitude, ic2012_2017.level1, ic2012_2017.level2,
                 ic2012_2017.level3,
                 ic2012_2017.level4,
                 ic2012_2017.level5,
                 ic2012_2017.level6,
                 ic2012_2017.level7,
                 ic2012_2017.level8,
                 ic2012_2017.level12,
                 ic2012_2017.level17,
                 ic2012_2017.level18,
                 ic2012_2017.level19,
                 ic2012_2017.distcrs,
                 ic2012_2017.distnced,
                 ic2012_2017.dstnced1,
                 ic2012_2017.dstnced2,
                 ic2012_2017.dstnced3
                 FROM hd2012_2017
                 FULL OUTER JOIN ic2012_2017
                 ON hd2012_2017.unitid = ic2012_2017.unitid
                 AND hd2012_2017.year = ic2012_2017.year')

# use SQL to get what you want

DBI::dbGetQuery(conn 'SELECT col1 col2 col3 from ipeds_master;')











### I notice that trying to load the data causes server to crash
### Must upload data to "postgresql" database at https://analytics.bi.vt.edu/db/



upload_data = function() {
  #' Uploading my data to the database.
  #' Note that the code books for this data are housed in the
  #' stem_edu/data/stem_edu/original/IPEDS_Data folder, as .xls files of same name
  #' @example some_data()
  hd2012_2017 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/hd2012_2017.csv") %>%
    data.table(key = c("UNITID", "YEAR"))
  ic2012_2017 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/ic2012_2017.csv") %>%
    data.table(key = c("UNITID", "YEAR"))
  efa2012_2016 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/efa2012_2016.csv") %>%
    data.table(key = c("UNITID", "YEAR"))
  efb2012_2016 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/efb2012_2016.csv") %>%
    data.table(key = c("UNITID", "YEAR"))
  efaDist2012_2016 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/efaDist2012_2016.csv") %>%
    data.table(key = c("UNITID", "YEAR"))

  # R cannot do this joining without overloading the server.
  master_ipeds <- merge(hd2012_2017, ic2012_2017, all = TRUE, by = c("UNITID", "YEAR")) %>%
    merge(efa2012_2016, all = TRUE, by = c("UNITID", "YEAR")) %>%
    merge(efb2012_2016, all = TRUE, by = c("UNITID", "YEAR")) %>%
    merge(efaDist2012_2016, all = TRUE, by = c("UNITID", "YEAR"))

  conn = con_db(dbname = 'sheen',
                pass = get_my_password())
  output = dbWriteTable(conn = conn,
                        name = 'master_ipeds',
                        value = master_ipeds,
                        row.names = FALSE,
                        overwrite = TRUE)
  on.exit(dbDisconnect(conn = conn)) #disconnect from server after function runs
}

df = data.frame(a = 1:3)


