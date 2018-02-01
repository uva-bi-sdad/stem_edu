library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)
library(data.table)
library(forcats)
data.dir = "./data/stem_edu/working/"

# Load in the file containing the locations and onet codes of all job postings

parsedJobs = na.omit(fread(paste0(data.dir, "allOpenjobsParsed.csv"), drop = c("responsibilities", "experienceRequirements", "jobDescription")))
parsedJobs = rename(parsedJobs, c(jobLocation_geo_latitude = "tlats", jobLocation_geo_longitude = "tlons"))
# Load in the block codes atc for all the lat/longs from above

geocodedJobs = fread(paste0(data.dir, "allOpenjobsGeocoded.csv"))

# Merge the two

completeJobs = parsedJobs[geocodedJobs, on = c("tlats", "tlons")]
rm(parsedJobs, geocodedJobs)

# Make classification columns for each onet code according to the crosswalk produced by Bianica

crosswalkFull = fread("./data/stem_edu/original/occupation_cw.csv", header = T, stringsAsFactors = F)[,-1]
crosswalk = select(crosswalkFull, onet_soc_code, soc_stem, nsf_se, rothwell_classification)
# Recode each classificaiton to a 0 if it's not stem and a 1 if it is stem

crosswalk %>% mutate(soc_stem = recode(soc_stem, "Non-STEM" = "0", "STEM" = "1"),
                     nsf_se = recode(nsf_se, "Non-S&E and Other" = "0", "S&E-Related Occupations" = "1", "S&E Occupations" = "1"),
                     rothwell_classification = recode(rothwell_classification, "Other" = "0", "Super-STEM" = "1", "High-STEM" = "1")) ->
  crosswalk

# Merge stem data with geoData


completeJobs %>%
  left_join(crosswalk, by = c("normalizedTitle_onetCode" = "onet_soc_code")) %>%
  mutate(county_name = tolower(county_name)) %>%
    group_by(county_name) %>%
    summarise(propSocSTEM = sum(soc_stem == 1, na.rm = T)/sum(!is.na(soc_stem)),
              propNsfSTEM = sum(nsf_se == 1, na.rm = T)/sum(!is.na(nsf_se)),
              propRothSTEM = sum(rothwell_classification == 1, na.rm = T)/sum(!is.na(rothwell_classification))) %>%
    merge(subset(map_data("county"), region == "virginia"), by.y = "subregion", by.x = "county_name", all.y = T) %>%
    arrange(order) %>%
    melt(measure = c("propSocSTEM", "propNsfSTEM" ,"propRothSTEM")) ->
    plotData

theme_map <- function(...) {
    theme_minimal() +
        theme(
            text=element_text(family="sans", color="#22211d"),
            axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major=element_blank(),
            #panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
            panel.grid.minor=element_blank(),
            #panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
            plot.background=element_rect(fill="#f5f5f2", color = NA),
            panel.background=element_rect(fill="#f5f5f2", color = NA),
            legend.background=element_rect(fill="#f5f5f2", color = NA),
            panel.border=element_blank(),
            ...
        )
}

# Plot the heatmaps for each classification

{p1 = ggplot() + geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = value)) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_map() +
  coord_fixed(1.5) +
  scale_fill_viridis(
    option = "viridis",
    direction = -1,
    name = "Percentage",
    guide = guide_colorbar(
      ticks = F,
      nbins=100,
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(100, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      label.position = "bottom",na.value="lightgray"
    )
  ) + labs(x=NULL,
           y=NULL,
           title="Proportion of posted jobs which are STEM jobs",
           subtitle="Data: http://opendata.cs.vt.edu/dataset/openjobs-jobpostings.",
           caption = "Geometry: Virginia counties from ggplot2") +
  theme(legend.position = "bottom") +
  facet_grid(. ~ variable, labeller = as_labeller(c(
    "propSocSTEM" = "SOC STEM Proportion",
    "propRothSTEM" = "Roth. STEM Proportion",
    "propNsfSTEM" = "NSF STEM Proportion"))
    )

pdf("./output/virginia STEMmaps.pdf", height = 4.4, width = 14)
plot(p1)
dev.off()}

# Histograms of county wide percentages

plotData %>% select(county_name, variable, value) %>%
  group_by(county_name, variable) %>% summarise(value = mean(value)) %>%
  ungroup %>%
  group_by(variable) %>%
  arrange(value) -> barData

ggplot() + geom_col(data = barData, aes(rev(county_name), value)) +
  facet_grid(. ~ variable, labeller = as_labeller(c(
    "propSocSTEM" = "SOC STEM Proportion",
    "propRothSTEM" = "Roth. STEM Proportion",
    "propNsfSTEM" = "NSF STEM Proportion"))
  )
  theme_map() +
  coord_fixed(1.5) +
  scale_fill_viridis(
    option = "viridis",
    direction = -1,
    name = "Percentage",
    guide = guide_colorbar(
      ticks = F,
      nbins=100,
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(100, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      label.position = "bottom",na.value="lightgray"
    )
  ) + labs(x=NULL,
           y=NULL,
           title="Proportion of posted jobs which are STEM jobs",
           subtitle="Data: http://opendata.cs.vt.edu/dataset/openjobs-jobpostings.",
           caption = "Geometry: Virginia counties from ggplot2") +
  theme(legend.position = "bottom")

pdf("./output/virginia STEMmaps.pdf", height = 4.4, width = 14)
plot(p1)
dev.off()

