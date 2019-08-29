library(data.table)
library(sf)
library(dplyr)
library(stringr)


# Import datasets - Economic Regions + BGT
econ_va_counties <- readRDS("data/stem_edu/working/BGexplorevalidate/econvacounties.RDS")
openjobs <- fread('data/stem_edu/working/allOpenjobsParsed.csv')

# ------------------------------------------------------------------------------------

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
#SAVE OUT TO FILE IF U RUN ABOVE LINE, otherwise use readRDS
#open_point <- readRDS('./data/stem_edu/working/open_point.RDS')
joined <- st_join(open_point, econ_va_counties)
#filter out the correct dates
library(lubridate)
joined <- filter(joined,as.Date(joined$datePosted) >= "2017-07-01")

#
joined_within <- joined %>% filter(within == TRUE) # removes 101 observations
saveRDS(joined_within, "./data/stem_edu/working/BGexplorevalidate/OJ_point.RDS")

# aggregate jobs per county
#join_cty_ct <- joined_within %>% group_by(fipscounty) %>% summarise(count = n())
join_cty_ct <- joined_within %>% group_by(NAME) %>% summarise(count = n())
econ_va_counties$COUNTYFP <- as.integer(econ_va_counties$COUNTYFP)
#econ_va_counties <- econ_va_counties %>% filter(`County/City` != "City")
join_cty_ct <- join_cty_ct %>% filter(!is.na(fipscounty))
#join_shape_ct <- st_join(econ_va_counties, join_cty_ct, suffix = c("COUNTYFP" = "fipscounty"))
join_shape_ct <- st_join(econ_va_counties, join_cty_ct, suffix = c("NAME" = "NAME"))

library(leaflet)

labels <- sprintf(
  "<strong>%s</strong><br/>%s",
  join_shape_ct$count, join_shape_ct$NAMELSAD
) %>% lapply(htmltools::HTML)

leaflet(join_shape_ct) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", count)(count),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

######### make new script later
open_join_shape_ct <-join_shape_ct
saveRDS(open_join_shape_ct, './data/stem_edu/working/BGexplorevalidate/open_join_shape_ct.RDS')

####new plot for onet
#onet code grouping- make first two digits
joined_within$two_dig_onet <- str_extract(joined_within$normalizedTitle_onetCode, "[0-9]{2}")
join_cty_onet <- joined_within %>% group_by(NAME) %>% summarise(job_count = n())
join_cty_onet <- joined_within %>% group_by(NAME, two_dig_onet) %>% summarise(onet_count = n())

#get rid of missing onet codes
join_cty_onet <- join_cty_onet[complete.cases(join_cty_onet$two_dig_onet),]

onet_11 <- filter(join_cty_onet, two_dig_onet == '11')
join_shape_onet <- st_join(econ_va_counties, onet_11, suffix = c("NAME" = "NAME"))

#plotting onet code 11
labels <- sprintf(
  "<strong>%s</strong><br/>%s",
  join_shape_onet$onet_count, join_shape_onet$NAMELSAD
) %>% lapply(htmltools::HTML)

leaflet(join_shape_onet) %>% addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", onet_count)(onet_count),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

#### Find way to add each onet code as a layer in the leaflet plot
