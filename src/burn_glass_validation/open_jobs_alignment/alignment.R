#align open jobs data with economic regions in VA
library(data.table)
library(sf)
library(dplyr)


# Import datasets - Economic Regions + BGT
econ_va_counties <- readRDS("data/stem_edu/working/BGexplorevalidate/econvacounties.RDS")
openjobs <- fread('data/stem_edu/working/allOpenjobsParsed.csv')

# ------------------------------------------------------------------------------------

# Filter out all missing geographies
open_coords <- select(openjobs, 'jobLocation_geo_longitude','jobLocation_geo_latitude')
names(open_coords) <- c('lon','lat')
open_coords <- open_coords[complete.cases(open_coords)]
#84,734 jobs removed for a total of 761,879

# convert the job geographies to sf dataframe using va counties geography CRS
open_point <- sf::st_as_sf(x = open_coords,
                          coords = c("lon", "lat"),
                          crs = st_crs(econ_va_counties))

# determining jobs that are within a va county or not
# this messes up - takes too long to run? econ_va_counties_sf not found
open_point$within <- st_within(open_point$geometry, econ_va_counties$geometry) %>% lengths > 0

#load back in the data
open_point <- readRDS('./data/stem_edu/working/open_points.RDS')
#attach back to regular data
open_prev <- openjobs[!is.na(openjobs$jobLocation_geo_latitude)]
open_prev <- sf::st_as_sf(x = open_prev,
                           coords = c('jobLocation_geo_longitude','jobLocation_geo_latitude'),
                           crs = st_crs(econ_va_counties))
open_new <- cbind(open_prev,open_point$within)
open_point <- open_new
joined <- st_join(open_point, econ_va_counties)
#filter out the correct dates
library(lubridate)
x <- filter(as.Date(joined$datePosted) >= "2017-07-01")

#rename the within column
colnames(joined)[colnames(joined)=="open_point.within"] <- "within"
joined_within <- joined %>% filter(within == TRUE) # removes 1855 observations

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
ggplot(join_shape_ct) +
  geom_sf(mapping = aes(fill = GOorg)) +
  geom_sf_text(mapping = aes(label = count))
