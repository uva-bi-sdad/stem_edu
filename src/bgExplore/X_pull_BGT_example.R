library(sdalr)
library(data.table)
library(magrittr)
library(dplyr)
library(DBI)

# Connect to database
con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dnair1", pass = "dnair1")

querymain <- "SELECT * FROM \"ads_main_q12016\" LIMIT 10"
query_ex_main <- "SELECT * FROM \"ads_main_q12016\" WHERE \"bgtjobid\" = '37994041050'"
query_ex_certs <- "SELECT * FROM \"ads_certs\" WHERE \"bgtjobid\" = '37994041050'"
query_ex_cip <- "SELECT * FROM \"ads_cip\" WHERE \"bgtjobid\" = '37994041050'"
query_ex_degree <- "SELECT * FROM \"ads_degrees\" WHERE \"bgtjobid\" = '37994041050'"
query_ex_major <- "SELECT * FROM \"ads_major\" WHERE \"bgtjobid\" = '37994041050'"
query_ex_skills <- "SELECT * FROM \"ads_skills\" WHERE \"bgtjobid\" = '37994041050'"
query_skills_2017 <- "SELECT * FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2017'"
query_main_2011 <- "SELECT LEFT(\"jobdate\",7), COUNT(\"bgtjobid\")  FROM \"ads_main\" GROUP BY LEFT(\"jobdate\", 7)"

#query_skills_2017_delete <- "DELETE FROM \"ads_skills\" WHERE LEFT(\"jobdate\", 4) = '2017'"
#dbGetQuery(con, query_skills_2017_delete)
dbGetQuery(con, query_main_2011)

test <- dbGetQuery(con, querymain)
ex_main <- dbGetQuery(con, query_ex_main)
ex_certs <- dbGetQuery(con, query_ex_certs)
ex_cip <- dbGetQuery(con, query_ex_cip)
ex_degree <- dbGetQuery(con, query_ex_degree)
ex_major <- dbGetQuery(con, query_ex_major)
ex_skills <- dbGetQuery(con, query_ex_skills)

test <- as.data.table(test)
ex_main <- as.data.table(ex_main)
ex_certs <- as.data.table(ex_certs)
ex_cip <- as.data.table(ex_cip)
ex_degree <- as.data.table(ex_degree)
ex_major <- as.data.table(ex_major)
ex_skills <- as.data.table(ex_skills)


setkeyv(ex_main, c("bgtjobid", "jobdate"))
setkeyv(ex_cip,c("bgtjobid", "jobdate", "salary"))
#setkey(ex_degree,bgtjobid)
setkeyv(ex_major,c("bgtjobid", "jobdate", "salary"))
setkeyv(ex_certs,c("bgtjobid", "jobdate", "salary"))
setkeyv(ex_skills,c("bgtjobid", "jobdate", "salary"))

ex_main[ex_cip][ex_major][ex_certs][]   #[ex_skills]
library(dplyr)
library(magrittr)
examplecomputerguy <- ex_main %>%
  left_join(ex_cip, by = c("bgtjobid", "jobdate")) %>%
  left_join(ex_major, by = c("bgtjobid", "jobdate", "salary")) %>%
  left_join(ex_certs, by = c("bgtjobid", "jobdate", "salary")) %>%
  left_join(ex_skills, by = c("bgtjobid", "jobdate", "salary"))

write.csv(examplecomputerguy, file = "data/stem_edu/working/examplecomputerguy.csv")
write.csv(ex_main, file = "data/stem_edu/working/exmain.csv")
write.csv(ex_cip, file = "data/stem_edu/working/excip.csv")
write.csv(ex_major, file = "data/stem_edu/working/exmajor.csv")
write.csv(ex_certs, file = "data/stem_edu/working/excerts.csv")
write.csv(ex_skills, file = "data/stem_edu/working/exskills.csv")


paths <- list.files("data/stem_edu/working/BGexplorevalidate/")
paths <- paste0("data/stem_edu/working/BGexplorevalidate/", paths)


example_cert <- readr::read_csv(paths[2])
example_cip<- readr::read_csv(paths[3])
example_main<- readr::read_csv(paths[4])
example_major<- readr::read_csv(paths[5])
example_skills<- readr::read_csv(paths[6])

example_cert
