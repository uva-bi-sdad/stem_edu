library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)
library(data.table)
library(forcats)
library(gridExtra)
library(scales)
data.dir = "./data/stem_edu/working/"

#Declaring theme function

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

# Load in the file containing the locations and onet codes of all job postings

parsedJobs = na.omit(fread(paste0(data.dir, "allOpenjobsParsed.csv"), drop = c("responsibilities", "experienceRequirements", "jobDescription")))
parsedJobs = rename(parsedJobs, c(jobLocation_geo_latitude = "tlats", jobLocation_geo_longitude = "tlons"))
# Load in the block codes atc for all the lat/longs from above

geocodedJobs = fread(paste0(data.dir, "allOpenjobsGeocoded.csv"))

# Merge the two

completeJobs = parsedJobs[geocodedJobs, on = c("tlats", "tlons")]
rm(parsedJobs, geocodedJobs)

# Make classification columns for each onet code according to the crosswalk produced by Bianica

cw = fread("./data/stem_edu/original/occupation_cw.csv", header = T, stringsAsFactors = F)[,-1]
cw$nsf_stem <- ifelse(cw$nsf_se!="Non-S&E and Other","STEM","Non-STEM")
cw$rothwell_stem <- ifelse(cw$rothwell_classification!="Other","STEM","Non-STEM")
cw$soc_stem2 <- ifelse(cw$soc_stem!="Non-STEM","STEM","Non-STEM")

crosswalk = select(cw, onet_soc_code, onet_soc_title, soc_stem, nsf_se, rothwell_classification)

# Merge stem data with geoData

completeJobs %>%
  left_join(crosswalk, by = c("normalizedTitle_onetCode" = "onet_soc_code")) %>%
  na.omit %>%
  mutate(county_name = tolower(county_name)) %>%
    group_by(county_name) %>%
    summarise(propSocSTEM = sum(soc_stem == "STEM")/n(),
              propNsfSTEM = sum(nsf_se == "S&E Occupations")/n(),
              propRothSTEM = sum(rothwell_classification == "Super-STEM")/n(),
              propSocSTEMRelated = sum(soc_stem == "STEM-related")/n(),
              propNsfSTEMRelated = sum(nsf_se == "S&E-Related Occupations")/n(),
              propRothSTEMRelated = sum(rothwell_classification == "High-STEM")/n()) %>%
    merge(subset(map_data("county"), region == "virginia"), by.y = "subregion", by.x = "county_name", all.y = T) %>%
    arrange(order) %>%
    melt(measure = c("propSocSTEM", "propNsfSTEM" ,"propRothSTEM", "propSocSTEMRelated", "propNsfSTEMRelated" ,"propRothSTEMRelated")) ->
    plotData

# This is some hackish code to make the 6 measures defined above into a 2 sets of 3 identifiers, so they can be plotted thus with facet_grid

pd1 = filter(plotData, variable %in%  c("propSocSTEM", "propNsfSTEM" ,"propRothSTEM"))
pd1$variable = recode(pd1$variable, "propSocSTEM" = "SOC", "propNsfSTEM" = 'NSF', "propRothSTEM" = "Rothwell")
pd1$variable = factor(pd1$variable)
pd1$type = "STEM"

pd2 = filter(plotData, variable %in%  c("propSocSTEMRelated", "propNsfSTEMRelated" ,"propRothSTEMRelated"))
pd2$variable = recode(pd1$variable, "propSocSTEMRelated" = "SOC", "propNsfSTEMRelated" = 'NSF', "propRothSTEMRelated" = "Rothwell")
pd2$variable = factor(pd2$variable)
pd2$type = "STEM-Related"

plotData = rbind(pd1, pd2)

# Plot the heatmaps for each classification

plotData$variable<-factor(plotData$variable,levels=c("SOC","NSF","Rothwell"))

stemPlot =
  ggplot() + geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = value), lwd = .25, color = "grey70") +
  theme_map() +
  coord_fixed(1.5) +
  scale_fill_viridis(
    option = "viridis",
    direction = -1,
    labels = percent,
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
           y=NULL) +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        legend.title=element_text(size=14),legend.text=element_text(size=12),
        plot.title=element_text(size=20,hjust=0.5,face="bold")) +
  facet_wrap(type ~ variable, strip.position="top", labeller = as_labeller(c(
    "NSF" = "NSF Science & Engineering",
    "Rothwell" = "Rothwell Super and High STEM",
    "SOC" = "BLS Standard Occupational Classification",
    "STEM"="", "STEM-Related"="")))


pdf("./output/virginia STEMmaps.pdf", height = 6, width = 10)
plot(stemPlot)
dev.off()



# This next code plots the proportion of jobs that are for software developer or NMT



completeJobs %>%
  left_join(crosswalk, by = c("normalizedTitle_onetCode" = "onet_soc_code")) %>%
  na.omit %>%
  mutate(county_name = tolower(county_name)) %>%
  group_by(county_name) %>%
  summarise(propNMT = mean(normalizedTitle_onetCode == "29-2033.00"),
            propSoftDev = mean(normalizedTitle_onetCode == "15-1132.00")) %>%
  merge(subset(map_data("county"), region == "virginia"), by.y = "subregion", by.x = "county_name", all.y = T) %>%
  arrange(order) ->
  propNMTandSoftwareDev

nmtPlot = ggplot() + geom_polygon(data = propNMTandSoftwareDev, aes(x = long, y = lat, group = group, fill = propNMT), lwd = .25, color = "grey70") +
  theme_map() +
  coord_fixed(1.5) +
  scale_fill_viridis(
    option = "viridis",
    direction = -1,
    labels = percent,
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
           title = "Nuclear Medicine Technologist Jobs") +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        legend.title=element_text(size=14),legend.text=element_text(size=12),
        plot.title=element_text(size=20,hjust=0.5,face="bold"))

softDevPlot = ggplot() + geom_polygon(data = propNMTandSoftwareDev, aes(x = long, y = lat, group = group, fill = propSoftDev), lwd = .25, color = "grey70") +
  theme_map() +
  coord_fixed(1.5) +
  scale_fill_viridis(
    option = "viridis",
    direction = -1,
    labels = percent,
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
           title="Software Developer Jobs") +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        legend.title=element_text(size=14),legend.text=element_text(size=12),
        plot.title=element_text(size=20,hjust=0.5,face="bold"))

pdf("./output/nmtAndSoftDevProportionVA.pdf", height = 4.4, width = 11)
grid.arrange(nmtPlot, softDevPlot, ncol = 2)
dev.off()
