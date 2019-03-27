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

zip_skills <- split(zip_skill, rep(1:9,each=12))

# Connect to database
con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dnair1", pass = "dnair1")


### ZIP MAIN
# datalist = list()
#
# for (f in zip_skills$`1`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_skills_2007" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2007skillsize.RDS")

# datalist = list()
#
# for (f in zip_skills$`2`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_skills_2010" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2010skillsize.RDS")
#
# datalist = list()
#
# for (f in zip_skills$`3`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_skills_2011" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2011skillsize.RDS")
#
# datalist = list()
#
# for (f in zip_skills$`4`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_skills_2012" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2012skillsize.RDS")
#
# datalist = list()
#
# for (f in zip_skills$`5`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_skills_2013" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2013skillsize.RDS")
#
# datalist = list()
#
# for (f in zip_skills$`6`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_skills_2014" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2014skillsize.RDS")

datalist = list()

for (f in zip_skills$`7`) {
  print(paste("Reading", f))
  data <- fread(cmd = paste("zcat", f))
  colnames(data) <- tolower(colnames(data))
  print(sum(nrow(data)))
  x <- list()
  x$f <- nrow(data)
  datalist[f] <- x
  print(paste("Writing", f))
  dbWriteTable(con, "ads_skills_2015" , data, row.names = F, append = TRUE)
}
y = do.call(rbind, datalist)
sum(y[,1])
saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2015skillsize.RDS")

datalist = list()

for (f in zip_skills$`8`) {
  print(paste("Reading", f))
  data <- fread(cmd = paste("zcat", f))
  colnames(data) <- tolower(colnames(data))
  print(sum(nrow(data)))
  x <- list()
  x$f <- nrow(data)
  datalist[f] <- x
  print(paste("Writing", f))
  dbWriteTable(con, "ads_skills_2016" , data, row.names = F, append = TRUE)
}
y = do.call(rbind, datalist)
sum(y[,1])
saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2016skillsize.RDS")

datalist = list()

for (f in zip_skills$`9`) {
  print(paste("Reading", f))
  data <- fread(cmd = paste("zcat", f))
  colnames(data) <- tolower(colnames(data))
  print(sum(nrow(data)))
  x <- list()
  x$f <- nrow(data)
  datalist[f] <- x
  print(paste("Writing", f))
  dbWriteTable(con, "ads_skills_2017" , data, row.names = F, append = TRUE)
}
y = do.call(rbind, datalist)
sum(y[,1])
saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2017skillsize.RDS")

