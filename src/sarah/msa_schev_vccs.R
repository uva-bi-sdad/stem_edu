library(sf)
library(tidyverse)
library(tidyr)
library(reshape2)
library(data.table)
library(ggrepel)
library(ggplot2)

#load MSA data from Census TIGER/Line Shapefiles
msa <- st_read("src/sarah/tl_2017_us_cbsa/tl_2017_us_cbsa.shp")
st_crs(msa) #unprojected crs

#filter for richmond and blacksburg MSAs
msa <- filter(msa, NAME %in% c("Richmond, VA","Blacksburg-Christiansburg-Radford, VA"))

#load VCCS data
vccs <- read.csv("data/stem_edu/working/ipeds_vccs/ipeds_vccs_cred.csv")
names(vccs)[names(vccs) == "College"] <- "institution"
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
levels(stw_schools$institution)[levels(stw_schools$institution)=="J Sargeant Reynolds Community College"] <- "J. Sargeant Reynolds Community College"
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
levels(stw_subbach_combine$institution)[levels(stw_subbach_combine$institution)=="J Sargeant Reynolds Community College"] <- "J. Sargeant Reynolds Community College"
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
levels(subbach_combine$institution)[levels(subbach_combine$institution)=="J Sargeant Reynolds Community College"] <- "J. Sargeant Reynolds Community College"
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
edu_nurs <-edu_nurs[, c("object", "geometry")]
edu_nurs$type = "VCCS"

coord <- edu_nurs %>%
  mutate(lat = unlist(map(edu_nurs$geometry,1)),
         long = unlist(map(edu_nurs$geometry,2)))

edu_nurs$object <- as.character(edu_nurs$object)
fortis <- c("Fortis College", "For-Profit",  "-77.487938", "37.581401")
ECPI <- c("ECPI University", "For-Profit",  "-77.581295", "37.501048")
southside <- c("Southside College of Health Sciences", "For-Profit", "-77.400123","37.267891")
bryant <- c("Bryant and Stratton College", "For-Profit", "-77.450783" ,"37.384098" )
forprofit <- rbind(fortis, ECPI, southside, bryant)
colnames(forprofit) <- c("object", "type", "lat", "long")
forprofit <- data.frame(forprofit)
forprofit$lat <- as.character(forprofit$lat)
forprofit$long <- as.character(forprofit$long)
forprofit$type <- as.character(forprofit$type)
forprofit$object <- as.character(forprofit$object)



forprofit <- forprofit %>%
  st_as_sf(coords = c("lat", "long")) %>%
  st_set_crs(value = st_crs(x = msa))



separated_coord2 <- forprofit %>%
  mutate(lat = unlist(map(forprofit$geometry,1)),
         long = unlist(map(forprofit$geometry,2)))

ggplot()+
  geom_sf(data=st_va, fill=NA)+
  geom_sf(data = msa, aes(fill = object)) +
  geom_sf(data = edu_nurs)+
  geom_sf(data= forprofit)+
  theme(
    panel.grid.major = element_line(colour = "transparent"),
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      text = element_blank(),
      legend.position = "none"
       )+
  geom_text_repel(data= coord, aes(x= lat, y=long, label=object),
    color = "#1B3766", size = 4, check_overlap = FALSE, nudge_x = c(-1.3, -1.5, -.4), nudge_y = c(.5, -0.25, 0.5))+
  geom_text_repel(data= separated_coord2, aes(x= lat, y=long, label=object),
                  color = "grey30", size = 4, check_overlap = FALSE, nudge_x = c(-1.3, -1.5, -1.5, -1.5), nudge_y = c(.25, 0, -.5, -.05))+
  annotate(geom = "text", x = -81.5, y = 37, label = "Blacksburg", fontface = "bold", color = "#1B3766", size = 6)+
  annotate(geom = "text", x = -76.25, y = 37.5, label = "Richmond", fontface = "bold", color = "#1B3766", size = 6)+
  scale_fill_manual(values = c("#02ABD6", "#F17E1D"))


