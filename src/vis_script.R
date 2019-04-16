

library(magrittr)
library(sf)
library(leaflet)
library(tigris)
library(readxl)

#import shapes and attribute data
states <- states(cb = FALSE, resolution = "500k", year = NULL)
SEflow <- read_excel("data/stem_edu/original/visualization_proj_s&edocinflow.xlsx",
                     sheet = "Sheet2")
#clean and combine data
states <- st_as_sf(states)
statedocsflow <- merge(x = states, y = SEflow, by.x = "NAME", by.y = "State")

#create color palette
pal1 <- colorNumeric(
  palette = c("#af8dc3","#f7f7f7", "#7fbf7b"),
  domain = statedocsflow$`S&E Doctorate Flow`)

pal2 <- colorNumeric(
  palette = c("#af8dc3","#f7f7f7", "#7fbf7b"),
  domain = statedocsflow$`Non-Doctorate Flow`)

#create color palette

scale_range <- c(-300:300)

pal1 <- colorNumeric(
  palette = "RdBu",
  domain = `scale_range`)

## Make vector of colors for values smaller than 0 (20 colors)
rc1 <- colorRampPalette(colors = c("#7fbf7b", "#f7f7f7"), space = "Lab")(50)
rc2 <- colorRampPalette(colors = c("#f7f7f7", "#af8dc3"), space = "Lab")(300)
rampcols <- c(rc1, rc2)
sepal <- colorNumeric(palette = rampcols, domain = statedocsflow$`S&E Doctorate Flow`)
nonpal <- colorNumeric(palette = rampcols, domain = statedocsflow$`Non-Doctorate Flow`)

hist(statedocsflow$`S&E Doctorate Flow`)

#PLOT!

leaflet(statedocsflow) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              fillOpacity = 1,
              color = ~sepal(`S&E Doctorate Flow`),
              weight = 0.5) %>%
  addLegend(pal = sepal, values = ~`S&E Doctorate Flow`, opacity = 1) %>%
  setView(-98.5795, 39.8282, zoom=4)


leaflet(statedocsflow) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              fillOpacity = 1,
              color = ~sepal(`Non-Doctorate Flow`),
              weight = 0.5) %>%
  addLegend(pal = nonpal, values = ~`Non-Doctorate Flow`, opacity = 1) %>%
  setView(-98.5795, 39.8282, zoom=4)

#PLOT!

leaflet(statedocsflow) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              fillOpacity = 1,
              color = ~pal1(`S&E Doctorate Flow`),
              weight = 0.5) %>%
  addLegend(pal = pal1, values = ~`S&E Doctorate Flow`, opacity = 2) %>%
  setView(-98.5795, 39.8282, zoom=4)


leaflet(statedocsflow) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              fillOpacity = 1,
              color = ~pal1(`Non-Doctorate Flow`),
              weight = 0.5) %>%
  addLegend(pal = pal1, values = ~`Non-Doctorate Flow`, opacity = 2) %>%
  setView(-98.5795, 39.8282, zoom=4)




