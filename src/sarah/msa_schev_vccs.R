library(sf)
library(tidyverse)
library(tidyr)
library(reshape2)
library(data.table)

#load MSA data from Census TIGER/Line Shapefiles
msa <- st_read("src/sarah/tl_2017_us_cbsa/tl_2017_us_cbsa.shp")
st_crs(msa) #unprojected crs

#filter for richmond and blacksburg MSAs
msa <- filter(msa, NAME %in% c("Richmond, VA","Blacksburg-Christiansburg-Radford, VA"))

#load VCCS data
vccs <- read.csv("data/stem_edu/working/ipeds_vccs/ipeds_vccs_cred.csv")
names(vccs)[names(vccs) == "College"] <- "institution"

levels(vccs$institution)[levels(vccs$institution)=="J. Sargeant Reynolds Community College"] <- "J Sargeant Reynolds Community College"

vccs_list <- vccs[c(2, 15:18)]

#filter to unique College, long and lat
vccs <- vccs[c(2, 13:14)]
vccs <- unique(vccs)

#set same coordinates as msa data
vccs <- vccs %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) %>%
  st_set_crs(value = st_crs(x = msa))
st_crs(vccs)

#load SCHEV STW Schools data
stw_schools <- read.csv("data/stem_edu/working/ipeds_schev/ipeds_schev_stw_schools.csv")

stw_schools_list <- stw_schools[c(2, 16:21)]

#filter to unique institution, long and lat
stw_schools <- stw_schools[c(2, 13:14)]
stw_schools <- unique(stw_schools)

#set same coordinates as msa data
stw_schools <- stw_schools %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) %>%
  st_set_crs(value = st_crs(x = msa))
st_crs(stw_schools)

#load SCHEV STW subbach combine data
stw_subbach_combine <- read.csv("data/stem_edu/working/ipeds_schev/ipeds_schev_stw_subbach_combine.csv")

stw_subbach_combine_list <- stw_subbach_combine[c(2, 16:19)]

#filter to unique institution, long and lat
stw_subbach_combine <- stw_subbach_combine[c(2, 13:14)]
stw_subbach_combine <- unique(stw_subbach_combine)

#set same coordinates as msa data
stw_subbach_combine <- stw_subbach_combine %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) %>%
  st_set_crs(value = st_crs(x = msa))
st_crs(stw_subbach_combine)

#load SCHEV  subbach combine data
subbach_combine <- read.csv("data/stem_edu/working/ipeds_schev/ipeds_schev_subbach_combine.csv")

subbach_combine_list <- subbach_combine[c(2, 16:19)]

#filter to unique institution, long and lat
subbach_combine <- subbach_combine[c(2, 13:14)]
subbach_combine <- unique(subbach_combine)

#set same coordinates as msa data
subbach_combine <- subbach_combine %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) %>%
  st_set_crs(value = st_crs(x = msa))
st_crs(subbach_combine)

#return schools only in Richmond or blacksburg
msa_vccs <- st_join(x=msa, y=vccs)
msa_stw_schools <- st_join(x=msa, y=stw_schools)
msa_stw_subbach_combine <- st_join(x=msa, y=stw_subbach_combine)
msa_subbach_combine <- st_join(x=msa, y=subbach_combine)

#List of all schools in MSAs with MSA geometry
msa_edu <- rbind(msa_vccs, msa_stw_schools, msa_stw_subbach_combine, msa_subbach_combine)
msa_edu <- unique(msa_edu)
msa_edu <- msa_edu %>% select(institution, geometry)

vccs$type = "VCCS"
stw_schools$type = "stw_schools"
stw_subbach_combine$type = "stw_subbach_combine"
subbach_combine$type = "subbach_combine"


###list of all schools in MSAs with point geometry
edu <- rbind(stw_schools, subbach_combine, stw_subbach_combine, vccs)
edu$value =TRUE
edu <- spread(edu, type, value)
edu <- setDT(edu)[institution %chin% msa_edu$institution]
colnames(edu)[colnames(edu)=="institution"] <- "object"
edu <- replace(edu, is.na(edu), FALSE)

#MSA geometry only
msa <- msa %>% select(NAME, geometry)
colnames(msa)[colnames(msa)=="NAME"] <- "object"

#virginia shapefiles
st <- st_read("src/sarah/tl_2017_us_state/tl_2017_us_state.shp")
st_va <- filter(st, STUSPS == "VA")

#high school shapefiles
hs <- st_read ("src/sarah/EDGE_GEOCODE_PUBLICSCH_1718/EDGE_GEOCODE_PUBLICSCH_1718.shp")
st_crs(hs)

hs <-filter(hs, STATE== "VA")
msa_hs <- st_join(x=msa, y=hs)



library(ggrepel)

#plot of Virginia, MSAs, education within MSAs
ggplot()+
  geom_sf(data=st_va, fill=NA)+
  geom_sf(data = msa, fill = NA) +
  geom_sf(data = edu)+
  theme(panel.grid.major = element_line(colour = "transparent"))


#lists of programs
vccs_list <- vccs_list %>%
  filter(institution %in% edu$object)
stw_schools_list <- stw_schools_list %>%
  filter(institution %in% edu$object)
stw_subbach_combine_list <- stw_subbach_combine_list %>%
  filter(institution %in% edu$object)
subbach_combine_list <- subbach_combine_list %>%
  filter(institution %in% edu$object)



cipnurs <- "51.3801"

nurs_schools <- subbach_combine_list %>% filter(cipCode == cipnurs)
#this is just all of the vccs schools

edu_nurs <- edu %>% filter(object %in% nurs_schools$institution)


ggplot()+
  geom_sf(data=st_va, fill=NA)+
  geom_sf(data = msa, fill = NA) +
  geom_sf(data = edu_nurs)+
  theme(panel.grid.major = element_line(colour = "transparent"))
