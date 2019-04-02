
## zip objects

skillsize <- readRDS("data/stem_edu/working/BGexplorevalidate/skill_zip_sizes.RDS")

##DATABASE

assess_ads_skills07 <-
  "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_skills_2007\"
   GROUP BY LEFT(\"jobdate\", 7) ORDER BY LEFT(\"jobdate\", 7) DESC"

assess_ads_skills10 <-
  "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_skills_2010\"
GROUP BY LEFT(\"jobdate\", 7) ORDER BY LEFT(\"jobdate\", 7) DESC"

assess_ads_skills11 <-
  "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_skills_2011\"
GROUP BY LEFT(\"jobdate\", 7) ORDER BY LEFT(\"jobdate\", 7) DESC"

assess_ads_skills12 <-
  "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_skills_2012\"
GROUP BY LEFT(\"jobdate\", 7) ORDER BY LEFT(\"jobdate\", 7) DESC"

assess_ads_skills13 <-
  "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_skills_2013\"
GROUP BY LEFT(\"jobdate\", 7) ORDER BY LEFT(\"jobdate\", 7) DESC"

assess_ads_skills14 <-
  "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_skills_2014\"
GROUP BY LEFT(\"jobdate\", 7) ORDER BY LEFT(\"jobdate\", 7) DESC"

assess_ads_skills15 <-
  "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_skills_2015\"
GROUP BY LEFT(\"jobdate\", 7) ORDER BY LEFT(\"jobdate\", 7) DESC"

assess_ads_skills16 <-
  "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_skills_2016\"
GROUP BY LEFT(\"jobdate\", 7) ORDER BY LEFT(\"jobdate\", 7) DESC"

assess_ads_skills17 <-
  "SELECT LEFT(\"jobdate\", 7), COUNT(\"bgtjobid\") FROM \"ads_skills_2017\"
GROUP BY LEFT(\"jobdate\", 7) ORDER BY LEFT(\"jobdate\", 7) DESC"


con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dnair1", pass = "dnair1")
dbskillsize07 <- dbGetQuery(con, assess_ads_skills07)
dbskillsize10 <- dbGetQuery(con, assess_ads_skills10)
dbskillsize11 <- dbGetQuery(con, assess_ads_skills11)
dbskillsize12 <- dbGetQuery(con, assess_ads_skills12)
dbskillsize13 <- dbGetQuery(con, assess_ads_skills13)
dbskillsize14 <- dbGetQuery(con, assess_ads_skills14)
dbskillsize15 <- dbGetQuery(con, assess_ads_skills15)
dbskillsize16 <- dbGetQuery(con, assess_ads_skills16)
dbskillsize17 <- dbGetQuery(con, assess_ads_skills17)

dbskillsize <- rbind(dbskillsize07, dbskillsize10, dbskillsize11, dbskillsize12, dbskillsize13,
                     dbskillsize14, dbskillsize15, dbskillsize16, dbskillsize17)

dbskillsize <- dplyr::arrange(dbskillsize, left)

sum(dbskillsize$count) - sum(skillsize)
