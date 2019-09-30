### use nohup to write data
########
### convert job date to an actual field
########
### INDEX BGTJOBID, JOB DATE - first run
### CLEANTITLE, INDUSTRY? index as you need, separate lines for each one

library(magrittr)
library(sf)

# Import VA Economic Regions
econ_reg_orgs <- readxl::read_excel("data/stem_edu/original/BurningGlassData/Economic Regions.xlsx", sheet = "2019RegionsOrgs")
econ_reg_locs <- readxl::read_excel("data/stem_edu/original/BurningGlassData/Economic Regions.xlsx", sheet = "2019RegionsLoc")

# Import US & VA County Geographies
#con <- sdalr::con_db(dbname = "sdad", host = "127.0.0.1", port = 5433, user = "dtn2ep", pass = "dtn2ep")
con <- sdalr::con_db(dbname = "gis", host = "postgis_1", port = 5432, user = "dtn2ep", pass = "dtn2ep")
sql <- "SELECT * FROM census_tl.tl_2017_us_county WHERE \"STATEFP\" = \'51\'"
va_counties <- st_read(con, query = sql)
plot(va_counties["COUNTYFP"])


# Import BGT Data
sqlquery_0717 <-
  "
SELECT
\"bgtjobid\", \"jobid\", \"jobdate\", \"cleantitle\", \"canontitle\",
\"occfam\", \"occfamname\", \"soc\", \"socname\",  \"onet\", \"onetname\",
\"bgtocc\", \"bgtoccname\", \"bgtoccgroupname\", \"bgtoccgroupname2\", \"bgtcareerareaname\", \"bgtcareerareaname2\",
\"employer\", \"sector\", \"sectorname\", \"naics3\", \"naics4\", \"naics5\", \"naics6\",
\"city\", \"state\", \"county\", \"fipsstate\", \"fipscounty\", \"fips\", \"lat\", \"lon\",
\"bestfitmsa\", \"bestfitmsaname\", \"bestfitmsatype\", \"msa\", \"msaname\",
\"edu\", \"maxedu\", \"degree\",\"maxdegree\", \"exp\", \"maxexp\",
\"minsalary\", \"maxsalary\", \"minhrlysalary\", \"maxhrlysalary\", \"payfrequency\", \"salarytype\",
\"jobhours\", \"taxterm\",\"internship\", \"subocc\"

FROM \"ads_main_2017\" WHERE LEFT(\"jobdate\", 7) = '2017-07' AND \"fipsstate\" = 51
"

# Database + SQL Query
#con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dnair1", pass = "dnair1")
#query_0717 <- dbGetQuery(con, sqlquery_0717)
#write_rds(query_0717, "data/stem_edu/working/BGexplorevalidate/BGT_main_0717.RDS")
query_0717 <- read_rds("data/stem_edu/working/BGexplorevalidate/BGT_main_0717.RDS")

# ------------------------------------------------------------------------------------

# Clean columns
colnames(econ_reg_orgs) <- c("Region", "GOorg")
va_counties$GEOID <- as.numeric(va_counties$GEOID)

# JOIN Region tables with Geographies

econ_va_counties <- merge(va_counties, econ_reg_locs, by.x = 'GEOID', by.y = 'FIPS')  # note db geo is sf
econ_va_counties <- merge(econ_va_counties, econ_reg_orgs, 'Region')
#write_rds(econ_va_counties, "data/stem_edu/working/BGexplorevalidate/econvacounties.RDS")
econ_va_counties <- read_rds("data/stem_edu/working/BGexplorevalidate/econvacounties.RDS")

plot(econ_va_counties["Region"])
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FFFFFF")
plot(econ_va_counties["Region"], pal = cbPalette)

ggplot(data = econ_va_counties) +
  geom_sf(aes(fill = as.factor(Region))) +
  xlab("Longitude") + ylab("Latitude") + labs(fill = "Economic Region")
  theme_minimal() +
  scale_fill_manual(breaks = as.factor(1:9), values = cbPalette)

