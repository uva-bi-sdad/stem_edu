library(sf)
library(magrittr)
library(dplyr)

#always BGT files
BGpoint <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/BG_join_point.RDS")
BGshape <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/BG_join_shape_ct.RDS")
#original OJ files?
#OJpoint <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/open_point.RDS")
OJshape <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/open_join_shape_ct.RDS")
#new OJ files?
#OJpoint2 <- readRDS("data/stem_edu/working/BGexplorevalidate/OJpoint.RDS")
#OJshape2 <- readRDS("~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/open_join_shape_ct.RDS")
#new OJ point
OJpoint <- readRDS("data/stem_edu/working/BGexplorevalidate/OJ_point.RDS")

OJpoint$occfam <- as.numeric(stringr::str_extract(OJpoint$normalizedTitle_onetCode, "[0-9]{2}"))

BG1 <- BGpoint %>%
  group_by(occfam, occfamname) %>%
  summarise(count = n()) %>%
  filter(occfam != 'na')


OJ1 <- OJpoint %>%
  filter(as.Date(datePosted) >= "2017-07-01" ) %>%
  group_by(occfam) %>%
  summarise(count = n()) %>%
  filter(occfam != 99)

st_geometry(BG1) <- NULL
st_geometry(OJ1) <- NULL

BG1$occfam <- as.numeric(BG1$occfam)

thing <- BG1 %>%
  left_join(OJ1, by = "occfam")

colnames(thing) <- c("occfam", "occfamname", "BGTcount", "OJcount")
thing

View(thing)


BG2 <- BGpoint %>%
  group_by(Region, GOorg) %>%
  summarise(count = n())


OJ2 <- OJpoint2 %>%
  filter(as.Date(datePosted) >= "2017-07-01" ) %>%
  group_by(Region, GOorg) %>%
  summarise(count = n())

st_geometry(BG2) <- NULL
st_geometry(OJ2) <- NULL


thing2 <- BG2 %>%
  left_join(OJ2, by = c("Region", "GOorg"))

colnames(thing2) <- c("Region", "GOorg", "BGTcount", "OJcount")
thing2

View(thing)

thing
thing2

thing2 %>%
  filter(Region != "NA") %>%
  mutate(diff = BGTcount - OJcount,
         percdiff = 100*(abs(diff)/(BGTcount + OJcount)))


BG3 <- BGpoint %>%
  group_by(employer) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(source = "BGT")


OJ3 <- OJpoint2 %>%
  filter(as.Date(datePosted) >= "2017-07-01" ) %>%
  group_by(hiringOrg) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(source = "OJ")

st_geometry(BG3) <- NULL
st_geometry(OJ3) <- NULL

BG3
OJ3
colnames(OJ3) <- c("employer", "count", "source")

newthing <- rbind(BG3, OJ3)
View(arrange(newthing, employer))


thing3 <- BG3 %>% inner_join(OJ3, by = "employer") %>% select(employer, count.x, count.y)
colnames(thing3) <- c("employer", "BGTjobs", "OJjobs")
thing3
View(thing3)
nrow(BG3)
nrow(OJ3)
nrow(thing3)
nrow(thing3) / nrow(BG3)
nrow(thing3)/ nrow(OJ3)


# library(xlsx)
# attributes(thing)$class <- c("data.frame")
# attributes(thing2)$class <- c("data.frame")
# attributes(thing3)$class <- c("data.frame")
#
# write.xlsx(thing, file="validtables.xlsx", sheetName="occfam", row.names=FALSE)
# write.xlsx(thing2, file="validtables.xlsx", sheetName="econ_region", append=TRUE, row.names=FALSE)
# write.xlsx(thing3, file="validtables.xlsx", sheetName="employer", append=TRUE, row.names=FALSE)

write.csv(thing, "data/stem_edu/working/BGexplorevalidate/BGTOJ_summ_occfam.csv")
write.csv(thing2, "data/stem_edu/working/BGexplorevalidate/BGTOJ_summ_econreg.csv")
write.csv(thing3, "data/stem_edu/working/BGexplorevalidate/BGTOJ_summ_employer.csv")
