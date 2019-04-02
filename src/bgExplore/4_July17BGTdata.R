library(sdalr)
library(DBI)

sqlquery_0717 <-
  "
SELECT
\"bgtjobid\",
\"jobid\",
\"jobdate\",
\"cleantitle\",
\"canontitle\",
\"occfam\",
\"occfamname\",
\"soc\",
\"socname\",
\"onet\",
\"onetname\",
\"bgtocc\",
\"bgtoccname\",
\"bgtoccgroupname\",
\"bgtoccgroupname2\",
\"bgtcareerareaname\",
\"bgtcareerareaname2\",
\"employer\",
\"sector\",
\"sectorname\",
\"naics3\",
\"naics4\",
\"naics5\",
\"naics6\",
\"city\",
\"state\",
\"county\",
\"fipsstate\",
\"fipscounty\",
\"fips\",
\"lat\",
\"lon\",
\"bestfitmsa\",
\"bestfitmsaname\",
\"bestfitmsatype\",
\"msa\",
\"msaname\",
\"edu\",
\"maxedu\",
\"degree\",
\"maxdegree\",
\"exp\",
\"maxexp\",
\"minsalary\",
\"maxsalary\",
\"minhrlysalary\",
\"maxhrlysalary\",
\"payfrequency\",
\"salarytype\",
\"jobhours\",
\"taxterm\",
\"internship\",
\"subocc\"

FROM \"ads_main_2017\" WHERE LEFT(\"jobdate\", 7) = '2017-07' AND \"fipsstate\" = 51
"

# Database + SQL Query
con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dnair1", pass = "dnair1")
query_0717_1000r <- dbGetQuery(con, sqlquery_0717)

# Datatable filter out unknown geographies - have 991 jobs across 13 MSAs + na + blank (but lat/lons are populated)
library(data.table)
query_0717_min999 <- query_0717_1000r[query_0717_1000r$lat != -999 | query_0717_1000r$lon != -999, ]
plot(query_0717_min999$lat, query_0717_min999$lon)
unique(query_0717_1000r$msaname)

# Get Geometry CRS of the counties
# va_counties came from 3_VAshapes.R from tigris package - NEED to clean this script up!!
library(sf)
library(dplyr)
sp::geometry(va_counties) # CRS:  +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0

# convert the job geographies to sf dataframe using va counties geography CRS
bgt_point <- sf::st_as_sf(x = query_0717_min999,
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

# from the other script
econ_va_counties_sf <- sf::st_as_sf(econ_va_counties)
plot(econ_va_counties_sf["Region"])

# checking everything is now sf
class(bgt_point)
class(econ_va_counties_sf)

#checking geography matches
sf::st_geometry(bgt_point)
sf::st_geometry(econ_va_counties_sf)

# determining jobs that are within a va county or not
bgt_point$within <- st_within(bgt_point$geometry, econ_va_counties_sf$geometry) %>% lengths > 0

joined <- st_join(bgt_point, econ_va_counties_sf)
isTRUE(joined$within)
class(joined)

joined_within <- joined %>% filter(within == TRUE)

plot(joined_within["Region"])
plot(joined_within["Support Org"])
plot(joined_within["sectorname"])


#join_reg_ct <- joined_within %>% group_by(Region) %>% summarise(count = n())
join_cty_ct <- joined_within %>% group_by(fipscounty) %>% summarise(count = n())
#st_join(econ_va_counties_sf, join_ct, suffix = c("Region"))
econ_va_counties_sf$COUNTYFP <- as.integer(econ_va_counties_sf$COUNTYFP)
join_shape_ct <- st_join(econ_va_counties_sf, join_cty_ct, suffix = c("COUNTYFP" = "fipscounty"))


join_shape_ct$bucket <-  ifelse(join_shape_ct$count > 1000, 1000, ifelse(join_shape_ct$count > 500, 500, join_shape_ct$count/100))
join_shape_ct$bucket <- (cut(x = join_shape_ct$count, breaks = c(0, 100, 200, 300, 400, 500, 1000, 2000)))
join_shape_ct$bucket2 <- (cut(x = join_shape_ct$count, breaks = c(0, 20, 40, 60, 80, 100, 16000)))
plot(join_shape_ct["count"])
plot(join_shape_ct["bucket"])
plot(join_shape_ct["bucket2"])

join_shape_ct %>% select(count) %>% arrange(count) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')
title("the ten counties with smallest area")

