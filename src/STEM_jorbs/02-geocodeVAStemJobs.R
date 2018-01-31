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

uniqueJobsGeocode <- sdalr::FCClocation2FIPS(1, lat = uniqueLocations$tlats[1], lon = uniqueLocations$tlons[1])

cutoffs = c(2, (1:12)*50, 637)

for(i in 2:652){
  #lower = cutoffs[i]
  #upper = cutoffs[1+1]
  uniqueJobsGeocode = rbind(allJobsGeocode, sdalr::FCClocation2FIPS(i, uniqueLocations$tlats[i], uniqueLocations$tlons[i]))
  #Sys.sleep(60)
}

uniqueJobsGeocode = cbind(uniqueLocations, uniqueJobsGeocode)

allJobsGeocode = dt[uniqueJobsGeocode, on = c("tlats", "tlons")]

fwrite(allJobsGeocode, "./data/stem_edu/working/allOpenjobsGeocoded.csv")


