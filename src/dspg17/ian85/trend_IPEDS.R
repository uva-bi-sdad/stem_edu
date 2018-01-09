# For each year, field, and degree type, plot the number of degrees awarded

library(readr)
library(raster)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(reshape)


##### From here until about lines 430 is code to subset an IPEDS csv.
##### This was written because I didn't that the time to learn the dplyr package
##### which has functions to do the things I manually coded.
# unzip("~/git/stem_edu/data/stem_edu/original/IPEDS_data_zipped/all2010.zip", exdir="~/git/stem_edu/data/stem_edu/working/IPEDS_working/")
# unzip("~/git/stem_edu/data/stem_edu/original/IPEDS_data_zipped/all2011.zip", exdir="~/git/stem_edu/data/stem_edu/working/IPEDS_working/")
# unzip("~/git/stem_edu/data/stem_edu/original/IPEDS_data_zipped/all2012.zip", exdir="~/git/stem_edu/data/stem_edu/working/IPEDS_working/")
# unzip("~/git/stem_edu/data/stem_edu/original/IPEDS_data_zipped/all2013.zip", exdir="~/git/stem_edu/data/stem_edu/working/IPEDS_working/")
# unzip("~/git/stem_edu/data/stem_edu/original/IPEDS_data_zipped/all2014.zip", exdir="~/git/stem_edu/data/stem_edu/working/IPEDS_working/")
# unzip("~/git/stem_edu/data/stem_edu/original/IPEDS_data_zipped/all2015.zip", exdir="~/git/stem_edu/data/stem_edu/working/IPEDS_working/")


palette1 <- brewer.pal(3,"Spectral")


IPEDS_2010_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2010_degrees.csv")
IPEDS_2011_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2011_degrees.csv")
IPEDS_2012_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2012_degrees.csv")
IPEDS_2013_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2013_degrees.csv")
IPEDS_2014_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2014_degrees.csv")
IPEDS_2015_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2015_degrees.csv")


####The data on PhDs read in here should be re-downloaded from IPEDS website.
####There may have been an issue with the 2010 data. I am not sure if it's
####an IPEDS data collection issue, or a data cleaning issue.
all2010 <-read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/all2010_v4.csv")
phd2011 <-read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/phd2011.csv")
phd2012 <-read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/phd2012.csv")
phd2013 <-read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/phd2013.csv")
phd2014 <-read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/phd2014.csv")
phd2015 <-read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/phd2015.csv")


#Standardize column names for 2010 table
all2010 <- cbind(all2010[1:13],'Native Hawaiian or Other Pacific Islander Total',all2010[14:18])

colnames(all2010)[4] <- "First or Second Major"
colnames(all2010)[5] <- "CIP Code"
colnames(all2010)[7] <- "Award Level Code"
colnames(all2010)[8] <- "Grand Total"
colnames(all2010)[9] <- "Grand Total - Women"
colnames(all2010)[10] <- "American Indian or Alaska Native Total"
colnames(all2010)[11] <- "Asian"
colnames(all2010)[12] <- "Black or African Americanl"
colnames(all2010)[13] <- "Hispanic or Latino/Hispanic Total"

colnames(all2010)[15] <- "White Total"
colnames(all2010)[16] <- "Two or More Races Total"
colnames(all2010)[17] <- "Race/Ethnicity Unknown Total"
colnames(all2010)[18] <- "Nonresident Alien Total"



colnames(all2010)[14] <- "Native Hawaiian or Other Pacific Islander Total"
all2010$`Native Hawaiian or Other Pacific Islander Total` <- 0



colnames(all2010)[4:18] <- c("First or Second Major","CIP Code","CipTitle","Award Level Code","Grand Total","Grand Total - Women","American Indian or Alaska Native Total",
                             "Asian","Black or African Americanl","Hispanic or Latino/Hispanic Total","Native Hawaiian or Other Pacific Islander Total",
                             "White Total","Two or More Races Total","Race/Ethnicity Unknown Total","Nonresident Alien Total")

colnames(phd2011)[4:18] <- c("First or Second Major","CIP Code","CipTitle","Award Level Code","Grand Total","Grand Total - Women","American Indian or Alaska Native Total",
                             "Asian","Black or African Americanl","Hispanic or Latino/Hispanic Total","Native Hawaiian or Other Pacific Islander Total",
                             "White Total","Two or More Races Total","Race/Ethnicity Unknown Total","Nonresident Alien Total")
colnames(phd2012)[4:18] <- c("First or Second Major","CIP Code","CipTitle","Award Level Code","Grand Total","Grand Total - Women","American Indian or Alaska Native Total",
                             "Asian","Black or African Americanl","Hispanic or Latino/Hispanic Total","Native Hawaiian or Other Pacific Islander Total",
                             "White Total","Two or More Races Total","Race/Ethnicity Unknown Total","Nonresident Alien Total")
colnames(phd2013)[4:18] <- c("First or Second Major","CIP Code","CipTitle","Award Level Code","Grand Total","Grand Total - Women","American Indian or Alaska Native Total",
                             "Asian","Black or African Americanl","Hispanic or Latino/Hispanic Total","Native Hawaiian or Other Pacific Islander Total",
                             "White Total","Two or More Races Total","Race/Ethnicity Unknown Total","Nonresident Alien Total")
colnames(phd2014)[4:18] <- c("First or Second Major","CIP Code","CipTitle","Award Level Code","Grand Total","Grand Total - Women","American Indian or Alaska Native Total",
                             "Asian","Black or African Americanl","Hispanic or Latino/Hispanic Total","Native Hawaiian or Other Pacific Islander Total",
                             "White Total","Two or More Races Total","Race/Ethnicity Unknown Total","Nonresident Alien Total")
colnames(phd2015)[4:18] <- c("First or Second Major","CIP Code","CipTitle","Award Level Code","Grand Total","Grand Total - Women","American Indian or Alaska Native Total",
                             "Asian","Black or African Americanl","Hispanic or Latino/Hispanic Total","Native Hawaiian or Other Pacific Islander Total",
                             "White Total","Two or More Races Total","Race/Ethnicity Unknown Total","Nonresident Alien Total")


#Standardize column names for 2011 table
colnames(IPEDS_2011_degrees)[4] <- "First or Second Major"
colnames(IPEDS_2011_degrees)[5] <- "CIP Code"
colnames(IPEDS_2011_degrees)[7] <- "Award Level Code"
colnames(IPEDS_2011_degrees)[8] <- "Grand Total"
colnames(IPEDS_2011_degrees)[9] <- "Grand Total - Women"
colnames(IPEDS_2011_degrees)[10] <- "American Indian or Alaska Native Total"
colnames(IPEDS_2011_degrees)[11] <- "Asian"
colnames(IPEDS_2011_degrees)[12] <- "Black or African Americanl"
colnames(IPEDS_2011_degrees)[13] <- "Hispanic or Latino/Hispanic Total"
colnames(IPEDS_2011_degrees)[14] <- "Native Hawaiian or Other Pacific Islander Total"
colnames(IPEDS_2011_degrees)[15] <- "White Total"
colnames(IPEDS_2011_degrees)[16] <- "Two or More Races Total"
colnames(IPEDS_2011_degrees)[17] <- "Race/Ethnicity Unknown Total"
colnames(IPEDS_2011_degrees)[18] <- "Nonresident Alien Total"

#Standardize column names for 2012 table
colnames(IPEDS_2012_degrees)[4] <- "First or Second Major"
colnames(IPEDS_2012_degrees)[5] <- "CIP Code"
colnames(IPEDS_2012_degrees)[7] <- "Award Level Code"
colnames(IPEDS_2012_degrees)[8] <- "Grand Total"
colnames(IPEDS_2012_degrees)[9] <- "Grand Total - Women"
colnames(IPEDS_2012_degrees)[10] <- "American Indian or Alaska Native Total"
colnames(IPEDS_2012_degrees)[11] <- "Asian"
colnames(IPEDS_2012_degrees)[12] <- "Black or African Americanl"
colnames(IPEDS_2012_degrees)[13] <- "Hispanic or Latino/Hispanic Total"
colnames(IPEDS_2012_degrees)[14] <- "Native Hawaiian or Other Pacific Islander Total"
colnames(IPEDS_2012_degrees)[15] <- "White Total"
colnames(IPEDS_2012_degrees)[16] <- "Two or More Races Total"
colnames(IPEDS_2012_degrees)[17] <- "Race/Ethnicity Unknown Total"
colnames(IPEDS_2012_degrees)[18] <- "Nonresident Alien Total"

#Standardize column names for 2013 table
colnames(IPEDS_2013_degrees)[4] <- "First or Second Major"
colnames(IPEDS_2013_degrees)[5] <- "CIP Code"
colnames(IPEDS_2013_degrees)[7] <- "Award Level Code"
colnames(IPEDS_2013_degrees)[8] <- "Grand Total"
colnames(IPEDS_2013_degrees)[9] <- "Grand Total - Women"
colnames(IPEDS_2013_degrees)[10] <- "American Indian or Alaska Native Total"
colnames(IPEDS_2013_degrees)[11] <- "Asian"
colnames(IPEDS_2013_degrees)[12] <- "Black or African Americanl"
colnames(IPEDS_2013_degrees)[13] <- "Hispanic or Latino/Hispanic Total"
colnames(IPEDS_2013_degrees)[14] <- "Native Hawaiian or Other Pacific Islander Total"
colnames(IPEDS_2013_degrees)[15] <- "White Total"
colnames(IPEDS_2013_degrees)[16] <- "Two or More Races Total"
colnames(IPEDS_2013_degrees)[17] <- "Race/Ethnicity Unknown Total"
colnames(IPEDS_2013_degrees)[18] <- "Nonresident Alien Total"

#Standardize column names for 2014 table
colnames(IPEDS_2014_degrees)[4] <- "First or Second Major"
colnames(IPEDS_2014_degrees)[5] <- "CIP Code"
colnames(IPEDS_2014_degrees)[7] <- "Award Level Code"
colnames(IPEDS_2014_degrees)[8] <- "Grand Total"
colnames(IPEDS_2014_degrees)[9] <- "Grand Total - Women"
colnames(IPEDS_2014_degrees)[10] <- "American Indian or Alaska Native Total"
colnames(IPEDS_2014_degrees)[11] <- "Asian"
colnames(IPEDS_2014_degrees)[12] <- "Black or African Americanl"
colnames(IPEDS_2014_degrees)[13] <- "Hispanic or Latino/Hispanic Total"
colnames(IPEDS_2014_degrees)[14] <- "Native Hawaiian or Other Pacific Islander Total"
colnames(IPEDS_2014_degrees)[15] <- "White Total"
colnames(IPEDS_2014_degrees)[16] <- "Two or More Races Total"
colnames(IPEDS_2014_degrees)[17] <- "Race/Ethnicity Unknown Total"
colnames(IPEDS_2014_degrees)[18] <- "Nonresident Alien Total"


#Standardize column names for 2015 table
colnames(IPEDS_2015_degrees)[4] <- "First or Second Major"
colnames(IPEDS_2015_degrees)[5] <- "CIP Code"
colnames(IPEDS_2015_degrees)[7] <- "Award Level Code"
colnames(IPEDS_2015_degrees)[8] <- "Grand Total"
colnames(IPEDS_2015_degrees)[9] <- "Grand Total - Women"
colnames(IPEDS_2015_degrees)[10] <- "American Indian or Alaska Native Total"
colnames(IPEDS_2015_degrees)[11] <- "Asian"
colnames(IPEDS_2015_degrees)[12] <- "Black or African Americanl"
colnames(IPEDS_2015_degrees)[13] <- "Hispanic or Latino/Hispanic Total"
colnames(IPEDS_2015_degrees)[14] <- "Native Hawaiian or Other Pacific Islander Total"
colnames(IPEDS_2015_degrees)[15] <- "White Total"
colnames(IPEDS_2015_degrees)[16] <- "Two or More Races Total"
colnames(IPEDS_2015_degrees)[17] <- "Race/Ethnicity Unknown Total"
colnames(IPEDS_2015_degrees)[18] <- "Nonresident Alien Total"


IPEDS_2010_2015_degrees <- rbind(IPEDS_2011_degrees, IPEDS_2012_degrees, IPEDS_2013_degrees, IPEDS_2014_degrees, IPEDS_2015_degrees)
allDegrees_2010_2015 <- rbind(IPEDS_2011_degrees, IPEDS_2012_degrees, IPEDS_2013_degrees, IPEDS_2014_degrees, IPEDS_2015_degrees, phd2011,phd2012,phd2013, phd2014, phd2015)

allDegrees_2010_2015_2<-mutate(allDegrees_2010_2015,`Award Level Code`=sapply(strsplit(allDegrees_2010_2015$`Award Level Code`, split='-', fixed=TRUE),function(x) (x[1])))

ipedsM<-melt(IPEDS_2010_2015_degrees,id=c("unitid","institution name","year","First or Second Major",
                                          "CIP Code","CipTitle","Award Level Code","Grand Total - Women","IDX_C"),
             value.name="count")


#####This is the melted data table with all STEM awards from 2010-2015
#save(ipedsM, file = "~/git/stem_edu/data/stem_edu/working/IPEDS_working/meltTable.RData")



####################################################
####################################################
### The next section creates heat tile plots to visualize trends in STEM degrees by race/ethnicity.
### This imports the data to plot.
load(file = "./git/stem_edu/data/stem_edu/working/meltTable.RData")

ipedsbyRace <- read.csv("./git/stem_edu/data/stem_edu/working/ipedsbyRace.csv")
associatesByRace <- read.csv("./git/stem_edu/data/stem_edu/working/associatesByRace.csv")
bachelorsByRace <- read.csv("./git/stem_edu/data/stem_edu/working/bachelorsByRace.csv")
mastersByRace <- read.csv("./git/stem_edu/data/stem_edu/working/mastersByRace.csv")
doctorsByRace <- read.csv("./git/stem_edu/data/stem_edu/working/doctorsByRace.csv")

ipedsbyRace$raceEth<-as.factor(ipedsbyRace$raceEth)
ipedsbyRace$raceEth = factor(ipedsbyRace$raceEth,levels(ipedsbyRace$raceEth)[c(5,4,1,2,3,8,6,7)])
ipedsbyRace <- filter(ipedsbyRace,raceEth!="Two or More Races")
ipedsbyRace <- filter(ipedsbyRace,raceEth!="Unknown Race/Ethnicity")

associatesByRace$raceEth<-as.factor(associatesByRace$raceEth)
associatesByRace$raceEth = factor(associatesByRace$raceEth,levels(associatesByRace$raceEth)[c(5,4,1,2,3,8,6,7)])
associatesByRace <- filter(associatesByRace,raceEth!="Two or More Races")
associatesByRace <- filter(associatesByRace,raceEth!="Unknown Race/Ethnicity")

bachelorsByRace$raceEth<-as.factor(bachelorsByRace$raceEth)
bachelorsByRace$raceEth = factor(bachelorsByRace$raceEth,levels(bachelorsByRace$raceEth)[c(5,4,1,2,3,8,6,7)])
bachelorsByRace <- filter(bachelorsByRace,raceEth!="Two or More Races")
bachelorsByRace <- filter(bachelorsByRace,raceEth!="Unknown Race/Ethnicity")

mastersByRace$raceEth<-as.factor(mastersByRace$raceEth)
mastersByRace$raceEth = factor(mastersByRace$raceEth,levels(mastersByRace$raceEth)[c(5,4,1,2,3,8,6,7)])
mastersByRace <- filter(mastersByRace,raceEth!="Two or More Races")
mastersByRace <- filter(mastersByRace,raceEth!="Unknown Race/Ethnicity")

doctorsByRace$raceEth<-as.factor(doctorsByRace$raceEth)
doctorsByRace$raceEth = factor(doctorsByRace$raceEth,levels(doctorsByRace$raceEth)[c(5,4,1,2,3,8,6,7)])
doctorsByRace <- filter(doctorsByRace,raceEth!="Two or More Races")
doctorsByRace <- filter(doctorsByRace,raceEth!="Unknown Race/Ethnicity")

### HEAT TILE MAP (shoutout to Madison "Battie" Arnsbarger for providing example code for these plots)

data_heatmap <- IPEDS_2010_2015_degrees %>% group_by(y_axis_var, x_axis_var) %>% dplyr::summarise(count = n())

png("~/git/stem_edu/output/TotalStemTrend.png", height = 1000, width = 1710)
trendAll<-ggplot() +
  geom_tile(data = ipedsbyRace,
            mapping = aes(x = year,
                          y = raceEth, fill = (percent*100)), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(11, 23), breaks= seq(11, 23, 6)) +
  scale_y_discrete(name = "Race/Ethnicity",labels = rev(c("White","Black","Asian, Native Hawaiian\nor Other Pacific Islander","American Indian\nor Alaska Native","Hispanic/Latino","Nonresident Alien")))+
  labs(title = "Percentage of Total STEM Degrees Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity")+ theme_minimal() +
  theme(legend.title=element_blank(),text=element_text(size=40),axis.text=element_text(size=25),axis.title=element_text(size=30),
        axis.text.x=element_text(angle=0,hjust=1), axis.text.y=element_text(angle=0,hjust=0.5,size=25)) +
  scale_x_continuous(breaks=ipedsbyRace$year)
print(trendAll)
dev.off()

png("~/git/stem_edu/output/MastersDegreeTrend.png", height = 1000, width = 1800)
trendAll<-ggplot() +
  geom_tile(data = mastersByRace,
            mapping = aes(x = year,
                          y = raceEth, fill = (percent*100)), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(11, 23), breaks= seq(11, 23, 6)) +
  scale_y_discrete(name = "Race/Ethnicity",labels = rev(c("White","Black","Asian, Native Hawaiian\nor Other Pacific Islander","American Indian\nor Alaska Native","Hispanic/Latino","Nonresident Alien")))+
  labs(title = "Percentage of STEM Master's Degrees Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity")+ theme_minimal() +
  theme(legend.title=element_blank(),text=element_text(size=40),axis.text=element_text(size=25),axis.title=element_text(size=30),
        axis.text.x=element_text(angle=0,hjust=1), axis.text.y=element_text(angle=0,hjust=0.5,size=25)) +
  scale_x_continuous(breaks=mastersByRace$year)
print(trendAll)
dev.off()


png("~/git/stem_edu/output/BachelorsDegreeTrend.png", height = 1000, width = 1800)
trendAll<-ggplot() +
  geom_tile(data = bachelorsByRace,
            mapping = aes(x = year,
                          y = raceEth, fill = (percent*100)), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(11, 23), breaks= seq(11, 23, 6)) +
  scale_y_discrete(name = "Race/Ethnicity",labels = rev(c("White","Black","Asian, Native Hawaiian\nor Other Pacific Islander","American Indian\nor Alaska Native","Hispanic/Latino","Nonresident Alien")))+
  labs(title = "Percentage of STEM Bachelor's Degrees Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity")+ theme_minimal() +
  theme(legend.title=element_blank(),text=element_text(size=40),axis.text=element_text(size=25),axis.title=element_text(size=30),
        axis.text.x=element_text(angle=0,hjust=1), axis.text.y=element_text(angle=0,hjust=0.5,size=25)) +
  scale_x_continuous(breaks=bachelorsByRace$year)
print(trendAll)
dev.off()

png("~/git/stem_edu/output/AssociatesDegreeTrend.png", height = 1000, width = 1850)
trendAll<-ggplot() +
  geom_tile(data = associatesByRace,
            mapping = aes(x = year,
                          y = raceEth, fill = (percent*100)), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(11, 23), breaks= seq(11, 23, 6)) +
  scale_y_discrete(name = "Race/Ethnicity",labels = rev(c("White","Black","Asian, Native Hawaiian\nor Other Pacific Islander","American Indian\nor Alaska Native","Hispanic/Latino","Nonresident Alien")))+
  labs(title = "Percentage of STEM Associate's Degrees Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity")+ theme_minimal() +
  theme(legend.title=element_blank(),text=element_text(size=40),axis.text=element_text(size=25),axis.title=element_text(size=30),
        axis.text.x=element_text(angle=0,hjust=1), axis.text.y=element_text(angle=0,hjust=0.5,size=25)) +
  scale_x_continuous(breaks=associatesByRace$year)
print(trendAll)
dev.off()

png("~/git/stem_edu/output/PHDTrend.png", height =1000, width = 1800)
trendAll<-ggplot() +
  geom_tile(data = doctorsByRace,
            mapping = aes(x = year,
                          y = raceEth, fill = (percent*100)), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(11, 25), breaks= seq(11, 25, 7)) +
  scale_y_discrete(name = "Race/Ethnicity",labels = rev(c("White","Black","Asian, Native Hawaiian\nor Other Pacific Islander","American Indian\nor Alaska Native","Hispanic/Latino","Nonresident Alien")))+
  labs(title = "Percentage of STEM PhDs Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity")+ theme_minimal() +
  theme(legend.title=element_blank(),text=element_text(size=40),axis.text=element_text(size=25),axis.title=element_text(size=30),
        axis.text.x=element_text(angle=0,hjust=1), axis.text.y=element_text(angle=0,hjust=0.5,size=25)) +
  scale_x_continuous(breaks=doctorsByRace$year)
print(trendAll)
dev.off()

######## Exploration of degree trends by gender still needs to be completed. This is
######## roughly what the final code will look like. Use the melted table (ipedsM)
######## as the source of data for "vectorToPlot"
# png("~/git/stem_edu/data/stem_edu/final/GenderTrend.png", height = 1000, width = 2000)
#
# vectorToPlotGender$percent
# ggplot() +
#   geom_tile(data = vectorToPlotGender,
#             mapping = aes(x = year,
#                           y = gender, fill = (percent*100), na.rm=TRUE)) +
#   scale_fill_gradient(low = 'white', high = 'gold', limits = c(11, 25), breaks= seq(11, 25, 7)) +
#   labs(title = "Percentage of Total STEM Degrees Earned in a Six Year Period", x = "Year",
#        y = "Gender") + theme_minimal() +
#   theme(text=element_text(size=48),axis.text=element_text(size=25),axis.title=element_text(size=42),
#        axis.text.x=element_text(angle=0,hjust=1,size=30), axis.text.y=element_text(angle=90,hjust=.5,size=30)) +
#   scale_x_continuous(breaks=vectorToPlot$year)
# dev.off()
#














### FAILED MOSAIC PLOT #########################################################################################
### A mosaic plot is not the best way to depict STEM award trends. There is too much information
### to be understandable in this format. I spent wayyyy too long creating this and it sucked.
### Don't make the same mistakes I made. You're better than that.
### See heat tile plot above.
###
# yearB <- rep(rep(c(2010, 2011, 2012, 2013, 2014, 2015),3),4)
# DegreeB <- rep(c("MW","BW","AW","MB","BB","AB","MH", "BH", "AH", "MO","BO","AO"),rep(6,12))
# DegreeB <- ordered(DegreeB,levels=c("MW","BW","AW","MB","BB","AB","MH", "BH", "AH", "MO","BO","AO"))
#
# degreeMosaic <-xtabs(vectorToPlot~yearB+DegreeB)
# mosaicplot(degreeMosaic, color=palette1, cex.axis = 0.01, las = 2, xlab="", ylab="",
#            main="STEM Degrees Awarded from 2010-2015")
#
# axis(side=3, at=c(0.055, 0.21, 0.372, 0.54, 0.71, 0.9), label=c(2010, 2011, 2012, 2013, 2014, 2015),tick=FALSE)
# axis(side=1,at=c(0.048, 0.208, 0.37, 0.55, 0.72, 0.91),label=c(sum(IPEDS_2010_degrees$`Grand Total`),sum(IPEDS_2011_degrees$`Grand Total`),sum(IPEDS_2012_degrees$`Grand Total`),sum(IPEDS_2013_degrees$`Grand Total`),sum(IPEDS_2014_degrees$`Grand Total`),sum(IPEDS_2015_degrees$`Grand Total`)),las=1,tick=FALSE)
#
# axis(side=2,at=c(0.08,0.26,0.39,0.77),label=c("Asian/Other","Hispanic/Latino","Black","White"),las=1,tick=FALSE)
#
# legend(0.17,0.5, inset=0, title="Type of Degree",
#        c("Master's","Bachelor's","Associate's"), fill=palette1, horiz=TRUE)
#####################################################################################################################
