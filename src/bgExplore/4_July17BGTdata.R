library(data.table)
library(sf)
library(dplyr)

# Import datasets - Economic Regions + BGT
econ_va_counties <- readRDS("data/stem_edu/working/BGexplorevalidate/econvacounties.RDS")
query_0717 <- readRDS("data/stem_edu/working/BGexplorevalidate/BGT_main_0717.RDS")

# ------------------------------------------------------------------------------------

# Datatable filter out unknown geographies - have 991 jobs across 13 MSAs + na + blank (but lat/lons are populated)
query_0717_min999 <- query_0717[query_0717$lat != -999 | query_0717$lon != -999, ] #430 observations removed out of 63K

# convert the job geographies to sf dataframe using va counties geography CRS
bgt_point <- sf::st_as_sf(x = query_0717_min999,
                        coords = c("lon", "lat"),
                        crs = st_crs(econ_va_counties))

# determining jobs that are within a va county or not
bgt_point$within <- st_within(bgt_point$geometry, econ_va_counties$geometry) %>% lengths > 0
joined <- st_join(bgt_point, econ_va_counties)
joined_within <- joined %>% filter(within == TRUE) # removes 1314 observations

# aggregate jobs per county
#join_cty_ct <- joined_within %>% group_by(fipscounty) %>% summarise(count = n())
join_cty_ct <- joined_within %>% group_by(NAME) %>% summarise(count = n())
econ_va_counties$COUNTYFP <- as.integer(econ_va_counties$COUNTYFP)
#econ_va_counties <- econ_va_counties %>% filter(`County/City` != "City")
#join_shape_ct <- st_join(econ_va_counties, join_cty_ct, suffix = c("COUNTYFP" = "fipscounty"))
join_shape_ct <- st_join(econ_va_counties, join_cty_ct, suffix = c("NAME" = "NAME"))

# ------------------------------------------------------------------------------------

# INDIVIDUAL & AGGREGATE JOB PLOTS
plot(joined_within["occfam"], key.pos = 4, key.width =lcm(2))
plot(join_shape_ct["count"])
text(join_shape_ct["count"])

# ------------------------------------------------------------------------------------
library(ggplot2)
library(leaflet)

# Mucking around with buckets
join_shape_ct$bucket <- (cut(x = join_shape_ct$count, breaks = c(0, 100, 200, 300, 400, 500, 1000, 16000)))
plot(join_shape_ct["bucket"])
ggplot2::ggplot(join_shape_ct) +
  geom_sf(mapping = aes(fill = GOorg)) +
  geom_sf_text(mapping = aes(label = count))

join_shape_ct$popup <- paste(join_shape_ct$Locations, join_shape_ct$count)


# ------------------------------------------------------------------------------------

##LEAFLET PLOTS

palette <- colorNumeric("YlOrRd", domain = log(join_shape_ct$count), na.color = "#FFFFFF")

leaflet(join_shape_ct) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~palette(log(count)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~htmltools::htmlEscape(popup)) %>%
  addLegend(pal =  palette, values = ~log(count), labels = ~count, title = "Jobs", opacity = 0.5)


#saveRDS(join_shape_ct, "data/stem_edu/working/BGexplorevalidate/BG_join_shape_ct.RDS")
#saveRDS(joined, "data/stem_edu/working/BGexplorevalidate/BG_join_point.RDS")

# ------------------------------------------------------------------------------------
openjoinshapect <- readRDS("data/stem_edu/working/BGexplorevalidate/BG_Shapefiles/open_join_shape_ct.RDS")
openjoinshapect <- sf::st_as_sf(openjoinshapect)
#setdiff(colnames(openjoinshapect), colnames(join_shape_ct))
head(openjoinshapect)
st_join(openjoinshapect, join_shape_ct, join = st_equals_exact)

openjoinshapect %>% filter(NAMENAME == "Wythe")
join_shape_ct %>% filter(NAMENAME == "Wythe")

BGTcount <- join_shape_ct %>% select(GOorg, count) %>% as.data.frame() %>% select(-geometry)
OJcount <- openjoinshapect %>% select(GOorg, count) %>% as.data.frame() %>% select(-geometry)


test <- left_join(BGTcount, OJcount, by = c("GOorg" = "GOorg")) %>%
  mutate(diff = count.x - count.y) %>%
  group_by(GOorg) %>%
  summarise(bgt = sum(count.x),
            oj = sum(count.y))

# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------

(hist(join_shape_ct$count[count < 2000]))

joined_within %>% group_by(NAME, fipscounty, Locations, `County/City`) %>% summarise(count = n()) %>% filter(Locations == "Fairfax")
joined_within %>% group_by(city, state, county, `County/City`) %>% summarise(count = n()) %>% View()

join_shape_ct %>% filter(NAMENAME == "Fairfax")
econ_va_counties %>% filter(NAME == "Fairfax")
join_cty_ct %>% filter(fipscounty == 59)
join_shape_ct %>% filter(NAME == "Arlington")

# ------------------------------------------------------------------------------------

# ~/git/stem_edu/data/stem_edu/original/STWcheatsheet.xlsx


#join_shape_ct$bucket <-  ifelse(join_shape_ct$count > 1000, 1000, ifelse(join_shape_ct$count > 500, 500, join_shape_ct$count/100))
join_shape_ct$bucket <- (cut(x = join_shape_ct$count, breaks = c(0, 100, 200, 300, 400, 500, 1000, 16000)))
join_shape_ct$bucket2 <- (cut(x = join_shape_ct$count, breaks = c(0, 20, 40, 60, 80, 100, 16000)))

plot(join_shape_ct["bucket"])
plot(join_shape_ct["bucket2"]) #, joined_within["sectorname"], add = TRUE)

points(joined_within$INTPTLAT, joined_within$INTPTLON)

plot(join_shape_ct["bucket"])
plot(joined_within, add = TRUE)

geom_joinshape <- st_geometry(join_shape_ct)
geom_joinwithin <- st_geometry(joined_within)

st_bbox(join_shape_ct)
st_bbox(joined_within)
sf::

sf::st_drop_geometry()
sf::st_set_geometry(joined_within, geom_joinshape)

joined_within



# ------------------------------------------------------------------------------------

# Plot multiple shapefiles
plot(aoiBoundary_HARV,
     col = "grey93",
     border="grey",
     main="NEON Harvard Forest Field Site")

plot(lines_HARV,
     col=roadColors,
     add = TRUE)

plot(point_HARV,
     add  = TRUE,
     pch = 19,
     col = "purple")



join_shape_ct %>% select(count) %>% arrange(count) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')
title("the ten counties with smallest area")

# Test plots - remove or shift later
#joined_within$GOorg <- as.factor(joined_within$GOorg)
plot(joined_within["Region"])
plot(joined_within["GOorg"])

plot_sf(joined_within, bgc = "#CCCCCC")
plot(joined_within["GOorg"])
plot(econ_va_counties["Region"])
thing <- st_centroid(econ_va_counties)
st_set_crs(thing, value = st_crs(econ_va_counties))
st_set_geometry(thing, value = st_geometry(econ_va_counties))
st_set_precision(thing, value = st_bbox(econ_va_counties))
st_set_geometry(thing, value = st_bbox(econ_va_counties))
plot(thing, add = TRUE)
plot_sf(econ_va_counties)

