## DON'T RUN THIS
## Unless something tragic happens, this code should never have to be run again (or until there's more jobs data.)

library(ggplot2)
library(sp)
library(data.table)
library(rpostgis)
library(stringr)
library(spdep)
library(sdalr)

# Load in the file containing the locations and onet codes of all job postings
parsedJobs = fread("./data/stem_edu/working/allOpenjobsParsed.csv")

tlats <- parsedJobs$jobLocation_geo_latitude %>% unname() %>% unlist()
tlons <- parsedJobs$jobLocation_geo_longitude %>% unname() %>% unlist()
dt = data.table(tlats, tlons)

uniqueLocations = na.omit(dt[,.N,by = c("tlats", "tlons")])

uniqueJobsGeocode = sdalr::FCClocations2FIPS(1:length(uniqueLocations$tlats), uniqueLocations$tlats, uniqueLocations$tlons)

uniqueJobsGeocode = cbind(uniqueLocations, uniqueJobsGeocode)[,-c("place_id", "state_name", "N")]

fwrite(uniqueJobsGeocode, "./data/stem_edu/working/allOpenjobsGeocoded.csv")


