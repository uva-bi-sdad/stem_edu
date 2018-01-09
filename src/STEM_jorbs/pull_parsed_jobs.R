library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)

data.dir = "./data/post_secondary_schev/working/chend"

parsedJobs = readRDS(paste(data.dir, "parsed_jobs_df.RDS", sep = "/"))
smallJobs = parsedJobs[,-2]



load(paste(data.dir, "geocode_openjobs_jobpostings_jul_2017.RData", sep = "/"))

geoData = merge(smallJobs, geocode_openjobs_jobpostings_jul_2017, by.x = "rawdata_id", by.y = "place_id")
geoData$fakeSTEM = sample(c(1, 0), nrow(geoData), rep = T)
geoData = subset(geoData, select = c("normalizedTitle_onetCode", "county_name", "county_fips", "fakeSTEM"))

# Summarize smallJobs into county level aggregates

geoData %>% mutate(county_name = tolower(county_name)) %>%
    group_by(county_name) %>%
    summarise(propStem = sum(fakeSTEM == 1)/n()) %>%
    merge(subset(map_data("county"), region == "virginia"), by.y = "subregion", by.x = "county_name") %>%
    arrange(order)->
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

ggplot() + geom_polygon(data = plotData, aes(x = long, y = lat, group = group, fill = propStem)) +
    theme_map() +
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
            label.position = "bottom"
        )
    ) + labs(x=NULL,
             y=NULL,
             title="Proportion of posted jobs which are STEM jobs",
             subtitle="Data: http://opendata.cs.vt.edu/dataset/openjobs-jobpostings, STEM classification from a place",
             caption = "Geometry: Virginia counties from ggplot2") +
    theme(legend.position = "bottom")

