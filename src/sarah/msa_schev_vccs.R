library(sf)
library(tidyverse)

#load MSA data from Census TIGER/Line Shapefiles
msa <- st_read("src/sarah/tl_2017_us_cbsa/tl_2017_us_cbsa.shp")
head(msa)
st_crs(msa) #unprojected crs

#filter for richmond and blacksburg MSAs
r_msa <- filter(msa, NAME == "Richmond, VA")
b_msa <- filter(msa, NAME == "Blacksburg-Christiansburg-Radford, VA")

#load VCCS data
vccs <- read.csv("data/stem_edu/working/ipeds_vccs/ipeds_vccs_cred.csv")

levels(vccs$College)[levels(vccs$College)=="J. Sargeant Reynolds Community College"] <- "J Sargeant Reynolds Community College"


#filter to unique College, long and lat
vccs <- ipeds_vccs_cred[c(2, 13:14)]
vccs <- unique(vccs)
head(vccs)

#set same coordinates as msa data
vccs <- vccs %>%
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) %>%
  st_set_crs(value = st_crs(x = msa))
st_crs(vccs)

#shows which VCCS colleges are in richmond, then blacksburg
r_msa_vccs <- st_join(x = r_msa, y = vccs)
b_msa_vccs <- st_join(x = b_msa, y = vccs)

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
r_msa_stw_schools <- st_join(x = r_msa, y = stw_schools)
b_msa_stw_schools <- st_join(x = b_msa, y = stw_schools)


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
r_msa_stw_subbach_combine <- st_join(x = r_msa, y = stw_subbach_combine)
b_msa_stw_subbach_combine <- st_join(x = b_msa, y = stw_subbach_combine)

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
r_msa_subbach_combine <- st_join(x = r_msa, y = subbach_combine)
b_msa_subbach_combine <- st_join(x = b_msa, y = subbach_combine)





###COMBINE ALL
names(b_msa_vccs)[names(b_msa_vccs) == "College"] <- "institution"
names(r_msa_vccs)[names(r_msa_vccs) == "College"] <- "institution"

b <- rbind(b_msa_stw_schools, b_msa_stw_subbach_combine, b_msa_subbach_combine)
b <- unique(b)

colnames(b)
colnames(b_msa_vccs)

b_msa_vccs <- b_msa_vccs[ -c(13, 15:29) ]

colnames(b)
colnames(b_msa_vccs)

b<- rbind(b, b_msa_vccs)
b <- unique(b)
head(b)

r<- rbind(r_msa_stw_schools, r_msa_stw_subbach_combine, r_msa_subbach_combine)
r <- unique(r)

colnames (r)
colnames(r_msa_vccs)

r_msa_vccs <- r_msa_vccs[ -c(13, 15:29) ]
colnames (r)
colnames(r_msa_vccs)

r <- rbind(r, r_msa_vccs)
r <- unique(r)
head(r)

library(ggplot2)
ggplot(r_geo)+
  geom_sf()

ggplot(vccs_geo)+
  geom_sf()
