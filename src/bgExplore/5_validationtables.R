library(sf)
library(magrittr)
library(dplyr)

#always BGT files
BGpoint <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/BG_join_point.RDS")
BGshape <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/BG_join_shape_ct.RDS")
#original OJ files?
OJpoint <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/open_point.RDS")
OJshape <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/open_join_shape_ct.RDS")
#new OJ files?
OJpoint2 <- readRDS("data/stem_edu/working/BGexplorevalidate/OJpoint.RDS")
OJshape2 <- readRDS("~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/open_join_shape_ct.RDS")
#new OJ point
OJpoint2 <- readRDS("data/stem_edu/working/BGexplorevalidate/OJ_point.RDS")

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




