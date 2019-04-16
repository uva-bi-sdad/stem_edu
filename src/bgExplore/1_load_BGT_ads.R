library(data.table)
library(sdalr)
library(DBI)
library(magrittr) # pipes
library(readr) #read_tsv


## CREATE FOLDER PATHS
data.dir = "./data/stem_edu/original/Burning_Glass_Data/"
folderNames = c("Certs", "CIP", "Main", "Skill", "Degree", "Major")
year = as.character(c(2007, 2010:2017))

paths = expand.grid(data.dir, folderNames, "/", year)
paths <- paths %>%
  dplyr::mutate(path = stringr::str_c(Var1, Var2, Var3, Var4),
                path2 = stringr::str_c(path, "/"),
                table = Var2) %>%
  dplyr::select(path,path2, table)

testthat::expect_equal(nrow(paths), 54)

## CREATE FILE NAMES
zip_files <- purrr::map(paths$path, function(x){
  zip_files = list.files(x, full.names = TRUE)
  zip_files
})  %>%
  unlist() %>%
  tibble::tibble(zip_p = .) %>%
  dplyr::mutate(save_p = stringr::str_extract(zip_p, '.+(?=/.*\\.zip)/')) %>%
  dplyr::left_join(paths, by = c('save_p' = "path2"))

nrow(zip_files)
testthat::expect_true(nrow(zip_files) == 649)

## SPLIT ZIP FILES BY TABLE NAME
zip_split <- split(zip_files$zip_p, zip_files$table)
zip_certs <- unlist(zip_split$Certs)
zip_cip <- unlist(zip_split$CIP)
zip_main <- unlist(zip_split$Main)
zip_skill <- unlist(zip_split$Skill)
zip_degree <- unlist(zip_split$Degree)
zip_major <- unlist(zip_split$Major)

# Connect to database
con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dnair1", pass = "dnair1")

# Drop table if you want to start empty
#dbGetQuery(con, "DROP TABLE IF EXISTS CIP")

#For every file read in and write to db


### ZIP CERTS
datalist = list()

for (f in zip_certs) {
  print(paste("Reading", f))
  data <- fread(cmd = paste("zcat", f))
  colnames(data) <- tolower(colnames(data))
  print(sum(nrow(data)))
  x <- list()
  x$f <- nrow(data)
  datalist[f] <- x
  print(paste("Writing", f))
  dbWriteTable(con, "ads_certs" , data, row.names = F, append = TRUE)
}
y = do.call(rbind, datalist)
sum(y[,1]) # matches! at 184120344
saveRDS(y, "data/stem_edu/working/BGexplorevalidate/sizechecks/check2/zipcertssize.RDS")

### ZIP CIPS
datalist = list()

for (f in zip_cip) {
  print(paste("Reading", f))
  data <- fread(cmd = paste("zcat", f))
  colnames(data) <- tolower(colnames(data))
  print(sum(nrow(data)))
  x <- list()
  x$f <- nrow(data)
  datalist[f] <- x
  print(paste("Writing", f))
  dbWriteTable(con, "ads_cip" , data, row.names = F, append = TRUE)
}
y = do.call(rbind, datalist)
sum(y[,1]) # 6422397
saveRDS(y, "data/stem_edu/working/BGexplorevalidate/sizechecks/check2/zipcipssize.RDS")

### ZIP SKILLS
# datalist = list()
#
# for (f in zip_skill) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   #print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_skills" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# skill_rows <- sum(y[,1])
# skill_rows # did not match, dbquery said 1305728994 (missing 1.5M rows 1489849337 )
# db - 1305728994
# rstudio - 1489849338


### ZIP DEGREES
datalist = list()

for (f in zip_degree) {
  print(paste("Reading", f))
  data <- fread(cmd = paste("zcat", f))
  colnames(data) <- tolower(colnames(data))
  print(sum(nrow(data)))
  x <- list()
  x$f <- nrow(data)
  datalist[f] <- x
  print(paste("Writing", f))
  dbWriteTable(con, "ads_degrees" , data, row.names = F, append = TRUE)
}
y = do.call(rbind, datalist)
sum(y[,1]) # matched ! at 93515167
saveRDS(y, "data/stem_edu/working/BGexplorevalidate/sizechecks/check2/zipdegreessize.RDS")

### ZIP MAJOR
datalist = list()

for (f in zip_major) {
  print(paste("Reading", f))
  data <- fread(cmd = paste("zcat", f))
  colnames(data) <- tolower(colnames(data))
  print(sum(nrow(data)))
  x <- list()
  x$f <- nrow(data)
  datalist[f] <- x
  print(paste("Writing", f))
  dbWriteTable(con, "ads_major" , data, row.names = F, append = TRUE)
}
y = do.call(rbind, datalist)
sum(y[,1]) # matched! AT 177817048
saveRDS(y, "data/stem_edu/working/BGexplorevalidate/sizechecks/check2/zipmajorssize.RDS")

### ZIP MAIN
# datalist = list()
#
# for (f in zip_main[85:87]) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_q12016" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1]) #




#skill_rows


##############

# cip plain is read tsv with correct columns - 6,422,440 (43 more rows, lots of parsing erros)
# cip 2 is read tsv with all columns set to character - 6,422,690 (293 more rows, no parsing errors)
# cip 3 is fread - 6,422,160	(237 fewer rows, no parsing errors)
# cip 4 is read tsv with BGTid set to character - 6,422,360	(37 fewer rows, lots of parsing errors)


# compare <- tibble::tibble("file" = c("./data/stem_edu/original/Burning_Glass_Data/CIP/2007/CIP_2007-01.zip",
# "./data/stem_edu/original/Burning_Glass_Data/CIP/2007/CIP_2007-02.zip",
# "./data/stem_edu/original/Burning_Glass_Data/CIP/2007/CIP_2007-03.zip",
# "./data/stem_edu/original/Burning_Glass_Data/CIP/2007/CIP_2007-04.zip",
# "./data/stem_edu/original/Burning_Glass_Data/CIP/2007/CIP_2007-05.zip"),
# "Numbers" = c(1428915, 1002160, 1283532, 1360466, 1347329))
# compare
# y
# sum(compare$Numbers) - sum(y[,1])


# test <- read_tsv(file = "data/stem_edu/original/Burning_Glass_Data/CIP/2007/CIP_2007-01.zip",
#                  col_types = cols(
#                    BGTJobId = col_character(),
#                    JobDate = col_date(format = ""),
#                    CIP = col_character(),
#                    Salary = col_double()
#                    ))
# head(test)
# nrow(test)
#
#
# dbGetQuery(conn = con, statement = "SELECT COUNT(BGTJobId) AS Number FROM Certs")
# DBI::dbListTables(conn = con)
# dbGetQuery(conn = con, statement = "SELECT COUNT(BGTJobId) FROM ads_skill")
# dbGetQuery(conn = con, statement = "SELECT COUNT('bgtjobid') FROM ads_skills")
#
# query07 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2007'"
# query10 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2010'"
# query11 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2011'"
# query12 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2012'"
# query13 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2013'"
# query14 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2014'"
# query15 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2015'"
# query16 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2016'"
# query17 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2017'"
#
# db07 <- DBI::dbGetQuery(con, query07)
# db10 <- DBI::dbGetQuery(con, query10)
# saveRDS(object, file = "my_data.rds")
#
# datalist <- list()
#
# for (f in zip_skill[25:36]) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   #print(sum(nrow(data)))
#   #x <- list()
#   #x$f <- nrow(data)
#   datalist[[f]] <- data
#   #print(paste("Writing", f))
#   #dbWriteTable(con, "ads_skills" , data, row.names = F, append = TRUE)
# }
# r_11 = do.call(rbind, datalist)

###JUNK

##zip_files %>% dplyr::mutate(save_p = stringr::str_extract(zip_p, '(_Data/)(.*)(/)' )) # best one so far

#purrr::map(zip_files, utils::unzip, list = FALSE, overwrite = FALSE)

# Stopifnot - if not true, it will error
# testthat will not stop the code, usually used for unit testing

# read_tsv(f, col_types = cols(BGTJobId = col_character(), JobDate = col_date(format = ""), CIP = col_character(),Salary = col_double()   ))
# col_types = cols(.default = "c")  #


