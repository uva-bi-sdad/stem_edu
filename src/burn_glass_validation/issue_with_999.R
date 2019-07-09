library(dataplumbr)
library(data.table)
library(readr)
f <- dataplumbr::file.unzip2temp("data/stem_edu/original/Burning_Glass_Data/Main/2016/Main_2016-01.zip")
ff <- list.files(f, full.names = T)
f_read <- fread(ff)

f_text <- read_file(ff)

m9s <- f_read[MSA==-999]
