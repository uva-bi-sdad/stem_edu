## Install dataplumbr with devtools::install_github("dads2busy/dataplumbr")

library(data.table)
library(jsonlite)
library(dataplumbr)

# get fast food search from google api
goog1 <- fromJSON("https://maps.googleapis.com/maps/api/place/textsearch/json?query=fast+food+in+Caroline+County+Virginia&key=AIzaSyCLmT3kQD4udAyLHlBaqYrBEEeX8XdZo7I")

# create data.table of needed items
name <- goog1$results$name
addr <- goog1$results$formatted_address
placeid <- goog1$results$place_id
loc <- goog1$results$geometry$location
gresult <- data.table(name, addr, placeid, loc)

# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult$placeid, gresult$lat, gresult$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")

# merge
fnl <- merge(gresult, locations, by = "placeid")

# get the ones from Caroline County
fnl[county_name=="Caroline", .(name, addr, lat, lng, county_fips, county_name)][order(name)]

