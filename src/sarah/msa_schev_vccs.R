library(sf)
library(tidyverse)
library(tidyr)
library(reshape2)

#load MSA data from Census TIGER/Line Shapefiles
msa <- st_read("src/sarah/tl_2017_us_cbsa/tl_2017_us_cbsa.shp")
head(msa)
st_crs(msa) #unprojected crs

#filter for richmond and blacksburg MSAs
r_msa <- filter(msa, NAME == "Richmond, VA")
b_msa <- filter(msa, NAME == "Blacksburg-Christiansburg-Radford, VA")
msa <- rbind(r_msa, b_msa)

#load VCCS data
vccs <- read.csv("data/stem_edu/working/ipeds_vccs/ipeds_vccs_cred.csv")
names(vccs)[names(vccs) == "College"] <- "institution"

levels(vccs$institution)[levels(vccs$institution)=="J. Sargeant Reynolds Community College"] <- "J Sargeant Reynolds Community College"


#filter to unique College, long and lat
vccs <- vccs[c(2, 13:14)]
vccs <- unique(vccs)
head(vccs)

#set same coordinates as msa data
vccs <- vccs %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) %>%
  st_set_crs(value = st_crs(x = msa))
st_crs(vccs)



#shows which VCCS colleges are in richmond, then blacksburg
#r_msa_vccs <- st_join(x = r_msa, y = vccs)
#b_msa_vccs <- st_join(x = b_msa, y = vccs)

#load SCHEV STW Schools data
stw_schools <- read.csv("data/stem_edu/working/ipeds_schev/ipeds_schev_stw_schools.csv")

#filter to unique institution, long and lat
stw_schools <- stw_schools[c(2, 13:14)]
stw_schools <- unique(stw_schools)
head(stw_schools)

#set same coordinates as msa data
stw_schools <- stw_schools %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) %>%
  st_set_crs(value = st_crs(x = msa))
st_crs(stw_schools)


#shows which SCHEV STW SCHOOLS  are in richmond, then blacksburg
#r_msa_stw_schools <- st_join(x = r_msa, y = stw_schools)
#b_msa_stw_schools <- st_join(x = b_msa, y = stw_schools)


#load SCHEV STW subbach combine data
stw_subbach_combine <- read.csv("data/stem_edu/working/ipeds_schev/ipeds_schev_stw_subbach_combine.csv")

#filter to unique institution, long and lat
stw_subbach_combine <- stw_subbach_combine[c(2, 13:14)]
stw_subbach_combine <- unique(stw_subbach_combine)
head(stw_subbach_combine)

#set same coordinates as msa data
stw_subbach_combine <- stw_subbach_combine %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) %>%
  st_set_crs(value = st_crs(x = msa))
st_crs(stw_subbach_combine)



#shows which SCHEV STW SUBBACH COMBINE  are in richmond, then blacksburg
#r_msa_stw_subbach_combine <- st_join(x = r_msa, y = stw_subbach_combine)
#b_msa_stw_subbach_combine <- st_join(x = b_msa, y = stw_subbach_combine)

#load SCHEV  subbach combine data
subbach_combine <- read.csv("data/stem_edu/working/ipeds_schev/ipeds_schev_subbach_combine.csv")

#filter to unique institution, long and lat
subbach_combine <- subbach_combine[c(2, 13:14)]
subbach_combine <- unique(subbach_combine)
head(subbach_combine)

#set same coordinates as msa data
subbach_combine <- subbach_combine %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) %>%
  st_set_crs(value = st_crs(x = msa))
st_crs(subbach_combine)


#shows which SCHEV  SUBBACH COMBINE  are in richmond, then blacksburg
#r_msa_subbach_combine <- st_join(x = r_msa, y = subbach_combine)
#b_msa_subbach_combine <- st_join(x = b_msa, y = subbach_combine)


msa_vccs <- st_join(x=msa, y=vccs)
msa_stw_schools <- st_join(x=msa, y=stw_schools)
msa_stw_subbach_combine <- st_join(x=msa, y=stw_subbach_combine)
msa_subbach_combine <- st_join(x=msa, y=subbach_combine)

msa_edu <- rbind(msa_vccs, msa_stw_schools, msa_stw_subbach_combine, msa_subbach_combine)
msa_edu <- unique(msa_edu)
msa_edu <- msa_edu %>% select(institution, geometry)

vccs$type = "VCCS"
stw_schools$type = "stw_schools"
stw_subbach_combine$type = "stw_subbach_combine"
subbach_combine$type = "subbach_combine"


###COMBINE ALL
edu <- rbind(stw_schools, subbach_combine, stw_subbach_combine, vccs)
edu$value =TRUE
edu <- spread(edu, type, value)
library(data.table)
edu <- setDT(edu)[institution %chin% msa_edu$institution]

colnames(edu)[colnames(edu)=="institution"] <- "object"
edu <- replace(edu, is.na(edu), FALSE)

msa <- msa %>% select(NAME, geometry)
colnames(msa)[colnames(msa)=="NAME"] <- "object"

msa$stw_schools = NA
msa$stw_subbach_combine = NA
msa$subbach_combine = NA
msa$VCCS = NA
msa <- msa[,c(1,3,4,5,6,2)]

p <-ggplot()+
  geom_sf(data=st_va)+
  geom_sf(data = msa, fill = NA) +
  geom_sf(data = edu)


st <- st_read("src/sarah/tl_2017_us_state/tl_2017_us_state.shp")
st_va <- filter(st, STUSPS == "VA")








#br <- rbind(b, r)





#b <- rbind(b_msa_stw_schools, b_msa_stw_subbach_combine, b_msa_subbach_combine)
#b <- unique(b)

#colnames(b)
#colnames(b_msa_vccs)

#b_msa_vccs <- b_msa_vccs[ -c(13, 15:29) ]

#colnames(b)
#colnames(b_msa_vccs)

#b<- rbind(b, b_msa_vccs)
#b <- unique(b)
#head(b)

#r<- rbind(r_msa_stw_schools, r_msa_stw_subbach_combine, r_msa_subbach_combine)
#r <- unique(r)

#colnames (r)
#colnames(r_msa_vccs)

#r_msa_vccs <- r_msa_vccs[ -c(13, 15:29) ]
#colnames (r)
#colnames(r_msa_vccs)

#r <- rbind(r, r_msa_vccs)
#r <- unique(r)
#head(r)





