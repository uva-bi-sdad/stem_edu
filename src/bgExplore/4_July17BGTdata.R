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

FROM \"ads_main_2017\" WHERE LEFT(\"jobdate\", 7) = '2017-07' AND \"fipsstate\" = 51 LIMIT 1000
"

con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dnair1", pass = "dnair1")
query_0717_1000r <- dbGetQuery(con, sqlquery_0717)
library(data.table)
query_0717_991r <- query_0717_1000r[query_0717_1000r$lat != -999 | query_0717_1000r$lon != -999, ]
## filter out points below -79.7 degrees longitude?


colnames(query_0717_1000r)

plot(query_0717_991r$lat, query_0717_991r$lon)
unique(query_0717_1000r$msaname)

sp::geometry(va_counties) # CRS:  +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0


bgt_point <- sf::st_as_sf(x = query_0717_991r,
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

plot(bgt_point["occfam"])
sort(bgt_point$county)

#from the other script
econ_va_counties_sf <- sf::st_as_sf(econ_va_counties)

plot(econ_va_counties_sf["Region"])

class(bgt_point)
class(econ_va_counties_sf)
library(sf)
sf::st_geometry(bgt_point)
sf::st_geometry(econ_va_counties_sf)

bgt_point$within <- st_within(bgt_point, econ_va_counties_sf)
bgt_point$within <- st_within(bgt_point$geometry, econ_va_counties_sf$geometry) %>% lengths > 0

as.data.frame(bgt_point$within)

joined <- st_join(bgt_point, econ_va_counties_sf)
joined
plot(joined)
plot(joined["Region"])
plot(joined["Support Org"])
plot(joined["sectorname"])


