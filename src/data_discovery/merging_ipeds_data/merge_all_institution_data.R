#########################################################
###  IPEDS Data - Code to Merge All institution data  ###
###  Fall Enrollment & Institutional characteristics  ###
###      Unique observations are UNITID + YEAR        ###
###                   Emily Sheen                     ###
#########################################################


library("dplyr")
library(readr)

# Import Data files = CSVs
hd2012_2017 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/hd2012_2017.csv")
ic2012_2017 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/ic2012_2017.csv")
efa2012_2016 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/efa2012_2016.csv")
efb2012_2016 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/efb2012_2016.csv")
efaDist2012_2016 <- read_csv("data/stem_edu/working/DSPG18/IPEDS/efaDist2012_2016.csv")

#Change to data table

colnames(hd2012_2017)
colnames(ic2012_2017)

temp <- full_join(hd2012_2017, ic2012_2017, by = c("UNITID", "YEAR"))
temp <- full_join(temp, efa2012_2016, by = c("UNITID", "YEAR"))
temp <- full_join(temp, efb2012_2016, by = c("UNITID", "YEAR"))
temp <- full_join(temp, efaDist2012_2016, by = c("UNITID", "YEAR"))


totCol <- ncol(hd2012_2017) + ncol(ic2012_2017) + ncol(efa2012_2016) + ncol(efb2012_2016) + ncol(efaDist2012_2016) - 8

### I notice that trying to load the data causes server to crash
### Must upload data to "postgresql" database at https://analytics.bi.vt.edu/db/
library(pacman)

pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)
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


