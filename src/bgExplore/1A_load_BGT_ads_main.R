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

zip_mains <- split(zip_main, rep(1:9,each=12))

# Connect to database
con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dnair1", pass = "dnair1")


### ZIP MAIN
datalist = list()


# for (f in zip_mains$`1`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2007" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2007mainsize.RDS")

# for (f in zip_mains$`2`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2010" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2010mainsize.RDS")
#
# for (f in zip_mains$`3`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2011" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2011mainsize.RDS")
#
# for (f in zip_mains$`4`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2012" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2012mainsize.RDS")
#
# for (f in zip_mains$`5`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2013" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2013mainsize.RDS")
#
# for (f in zip_mains$`6`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2014" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2014mainsize.RDS")
#
# for (f in zip_mains$`7`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2015" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2015mainsize.RDS")
#
# for (f in zip_mains$`8`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2016" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2016mainsize.RDS")
#
# for (f in zip_mains$`9`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2017" , data, row.names = F, append = TRUE)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, file = "data/stem_edu/working/BGexplorevalidate/2017mainsize.RDS")

