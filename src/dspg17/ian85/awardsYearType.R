library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(viridis)

data.dir = "./data/stem_edu/working/completionsData/"

cipcode = "CIPCODE"
level = "AWLEVEL"
count = "CTOTALT"
totalMen = "CTOTALM"
totalWomen = "CTOTALW"

# Possible breakouts: CTOTALM, CTOTALW

crosswalk = read.csv("./data/stem_edu/working/nces_stem_cips.csv")
years = as.character(2011:2015)

data.folders = list.files(data.dir, pattern = "A")[-1]

data.file = list.files(paste(data.dir, data.folders[1], sep = "/"), pattern = "_a.csv")

read.csv(paste(data.dir, data.folders[1], data.file, sep = "/")) %>%
  select(one_of(cipcode, level, count, totalMen, totalWomen)) ->
  cipLevel

cipLevel %>% group_by(CIPCODE, AWLEVEL) %>%
  summarise(year = years[1],
            totalAwards = sum(CTOTALT),
            totalMenAwards = sum(CTOTALM),
            totalWomenAwards = sum(CTOTALW)) ->
  cipLevelTotal

cipTotalYear = cipLevelTotal



for(i in 2:length(data.folders)){
  data.file = list.files(paste(data.dir, data.folders[i], sep = "/"), pattern = "_a.csv")
  read.csv(paste(data.dir, data.folders[i], data.file, sep = "/")) %>%
    select(one_of(cipcode, level, count, totalMen, totalWomen)) ->
    cipLevel

  cipLevel %>% group_by(CIPCODE, AWLEVEL) %>%
    summarise(year = years[i],
              totalAwards = sum(CTOTALT),
              totalMenAwards = sum(CTOTALM),
              totalWomenAwards = sum(CTOTALW)) ->
    cipLevelTotal

  cipTotalYear = rbind(cipTotalYear, cipLevelTotal)
}

stemField = ifelse(cipTotalYear$CIPCODE %in% crosswalk$CIP.code, 1, 0)

cipStem = cbind(cipTotalYear, stemField = stemField)

# Group the award levels and make a line for the number of stem degrees awardsd
awardClass = recode(cipStem$AWLEVEL, '1' = 'Award', '2' = 'Award', '4' = 'Award', '3' = "Associates", '5' = "Bachelors", '6' = "Certificate", '8' = "Certificate", '7' = "Masters", '17' = "Doctoral", '18' = "Doctoral", '19' = "Doctoral")

cipStem = cbind(cipStem, awardClass = awardClass)

# Group data by year

cipStem %>%
  filter(stemField == 1) %>%
  group_by(year, awardClass) %>%
  summarise(totalAwards = sum(totalAwards),
            totalMenAwards = sum(totalMenAwards),
            totalWomenAwards = sum(totalWomenAwards))->
  cipPlot

# Make plot

plotList = vector("list", 3)
cols = c("totalAwards", "totalMenAwards", "totalWomenAwards")
labels = c("Total Degrees Awarded (Thousands)", "Degrees Awarded to Men (Thousands)", "Degrees Awarded to Women (Thousands)")
for(i in 1:3){
  plotList[[i]] = ggplot() +
    geom_line(data = cipPlot, aes(x = year, y = get(cols[i])/1000, group = awardClass, col = awardClass), lwd = 2) +
    geom_point(data = cipPlot, aes(x = year, y = get(cols[i])/1000, group = awardClass, col = awardClass), shape = 21, colour = "black", fill = "white", size = 4) +
    labs(y = labels[i],
         x = "Year",
         title = "Stem Awards by Degree Type",
         subtitle = "Data: IPEDS completions survey") + theme_bw() +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
          title = element_text(size = 20),
          legend.text = element_text(size = 15)) +
    labs(colour="Award Type")


}

for(i in 1:3){
  pdf(paste0("./output/awardTrends/",cols[i], ".pdf"), height = 7, width = 7)
  plot(plotList[[i]])
  dev.off()
}









