## CHECK lengths

rdata07 <- readRDS("data/stem_edu/working/BGexplorevalidate/2007mainsize.RDS")
rdata10 <- readRDS("data/stem_edu/working/BGexplorevalidate/2010mainsize.RDS")
rdata11 <- readRDS("data/stem_edu/working/BGexplorevalidate/2011mainsize.RDS")
rdata12 <- readRDS("data/stem_edu/working/BGexplorevalidate/2012mainsize.RDS")
rdata13 <- readRDS("data/stem_edu/working/BGexplorevalidate/2013mainsize.RDS")
rdata14 <- readRDS("data/stem_edu/working/BGexplorevalidate/2014mainsize.RDS")
rdata15 <- readRDS("data/stem_edu/working/BGexplorevalidate/2015mainsize.RDS")
rdata16 <- readRDS("data/stem_edu/working/BGexplorevalidate/2016mainsize.RDS")
rdata17 <- readRDS("data/stem_edu/working/BGexplorevalidate/2017mainsize.RDS")

con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dnair1", pass = "dnair1")

query07 <- "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_main_2007\" GROUP BY LEFT(\"jobdate\", 7)"
query10 <- "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_main_2010\" GROUP BY LEFT(\"jobdate\", 7)"
query11 <- "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_main_2011\" GROUP BY LEFT(\"jobdate\", 7)"
query12 <- "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_main_2012\" GROUP BY LEFT(\"jobdate\", 7)"
query13 <- "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_main_2013\" GROUP BY LEFT(\"jobdate\", 7)"
query14 <- "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_main_2014\" GROUP BY LEFT(\"jobdate\", 7)"
query15 <- "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_main_2015\" GROUP BY LEFT(\"jobdate\", 7)"
query16 <- "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_main_2016\" GROUP BY LEFT(\"jobdate\", 7)"
query17 <- "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_main_2017\" GROUP BY LEFT(\"jobdate\", 7)"


db07 <- dbGetQuery(con, query07)
db10 <- dbGetQuery(con, query10)
db11 <- dbGetQuery(con, query11)
db12 <- dbGetQuery(con, query12)
db13 <- dbGetQuery(con, query13)
db14 <- dbGetQuery(con, query14)
db15 <- dbGetQuery(con, query15)
db16 <- dbGetQuery(con, query16)
db17 <- dbGetQuery(con, query17)


rdata <- rbind(rdata07, rdata10, rdata11, rdata12, rdata13, rdata14, rdata15, rdata16, rdata17)
dbdata <- rbind(db07, db10, db11, db12, db13, db14, db15, db16, db17)

dbdata <- dplyr::arrange(dbdata, left)

compare1 <- tibble::tibble(rdata, dbdata)
compare2 <- dplyr::mutate(.data = compare, diff = compare$rdata[,1] - compare$dbdata$count)

saveRDS(compare1, "data/stem_edu/working/BGexplorevalidate/sizechecks/check2/maincompare1.RDS")
saveRDS(compare2, "data/stem_edu/working/BGexplorevalidate/sizechecks/check2/maincompare2.RDS")


