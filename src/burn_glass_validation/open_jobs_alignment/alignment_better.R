library(data.table)
library(sf)
library(dplyr)
library(stringr)


# Import datasets - Economic Regions + BGT
econ_va_counties <- readRDS("data/stem_edu/working/BGexplorevalidate/econvacounties.RDS")
openjobs <- fread('data/stem_edu/working/allOpenjobsParsed.csv.gz')

# Filter out all missing geographies
open_jobs <- openjobs[complete.cases(openjobs$jobLocation_geo_longitude)]
#84,734 jobs removed for a total of 761,879

# convert the job geographies to sf dataframe using va counties geography CRS
open_point <- sf::st_as_sf(x = open_jobs,
                           coords = c('jobLocation_geo_longitude','jobLocation_geo_latitude'),
                           crs = st_crs(econ_va_counties))

# determining jobs that are within a va county or not
# this takes a long time to run
open_point$within <- st_within(open_point$geometry, econ_va_counties$geometry) %>% lengths > 0

open_point$occfam <- str_extract(open_point$normalizedTitle_onetCode, "[:alnum:]{0,2}")
open_point %>% select(normalizedTitle_onetCode, occfam)

openjobspoint <- open_point


saveRDS(openjobspoint, "data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/openjobs_point.RDS")

open_point <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/openjobs_point.RDS")


open_point_0717 <- open_point %>% filter(str_extract(datePosted, "\\d{4}-\\d{2}") == "2017-07")
open_point_0717$geom_grp = sapply(st_equals(open_point_0717), max)
open_point_aggr_0717 <- open_point_0717 %>%
  group_by(normalizedTitle_onetCode, normalizedTitle_onetName, occfam, datePosted, hiringOrg, geom_grp) %>%
  summarize(count = n())

saveRDS(open_point_aggr_0717, "data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/openjobs_point_aggregated_0717.RDS")

open_point_aggr_0717 <- st_join(econ_va_counties, open_point_aggr_0717)
saveRDS(open_point_aggr_0717, "data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/openjobs_point_aggregated_0717_wregion.RDS")
colnames(open_point_aggr_0717)
test <- open_point_aggr_0717 %>% group_by(Region, occfam) %>% summarise(sum = sum(count))
test2 <- test
st_geometry(test2) <- NULL
test2
saveRDS(test2, "data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/openjobs_point_aggregated_0717_fortable.RDS")

OpenJobs <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/openjobs_point_aggregated_0717_fortable.RDS")
#BGT <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/BG_join_point.RDS")

