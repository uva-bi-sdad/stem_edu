unzip("~/git/stem_edu/data/stem_edu/original/IPEDS_data_zipped/all2010.zip", exdir="~/git/stem_edu/data/stem_edu/working/IPEDS_working/")


-library(readr)
library(raster)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
palette1 <- brewer.pal(3,"Spectral")


IPEDS_2010_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2010_degrees.csv")
IPEDS_2011_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2011_degrees.csv")
IPEDS_2012_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2012_degrees.csv")
IPEDS_2013_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2013_degrees.csv")
IPEDS_2014_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2014_degrees.csv")
IPEDS_2015_degrees <- read_csv("~/git/stem_edu/data/stem_edu/working/IPEDS_working/IPEDS_2015_degrees.csv")

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
#drops <- c(colnames(IPEDS_2015_degrees)[9],colnames(IPEDS_2015_degrees)[11], colnames(IPEDS_2015_degrees)[13], colnames(IPEDS_2015_degrees)[15], colnames(IPEDS_2015_degrees)[17],
#           colnames(IPEDS_2015_degrees)[19], colnames(IPEDS_2015_degrees)[21], colnames(IPEDS_2015_degrees)[23], colnames(IPEDS_2015_degrees)[25], colnames(IPEDS_2015_degrees)[27])
#IPEDS_2015_degrees <- IPEDS_2015_degrees[ , !(names(IPEDS_2015_degrees) %in% drops)]



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


IPEDS_2010_2015_degrees <- rbind(IPEDS_2010_degrees,IPEDS_2011_degrees, IPEDS_2012_degrees, IPEDS_2013_degrees, IPEDS_2014_degrees, IPEDS_2015_degrees)
allDegrees_2010_2015 <- rbind(all2010,IPEDS_2011_degrees, IPEDS_2012_degrees, IPEDS_2013_degrees, IPEDS_2014_degrees, IPEDS_2015_degrees, phd2011,phd2012,phd2013, phd2014, phd2015)

allDegrees_2010_2015_2<-mutate(allDegrees_2010_2015,`Award Level Code`=sapply(strsplit(allDegrees_2010_2015$`Award Level Code`, split='-', fixed=TRUE),function(x) (x[1])))

######
# #IPEDS_subset_associate <- IPEDS_2010_2015_degrees[grep("Associate", IPEDS_2010_2015_degrees$`Award Level Code`),]
# #
# mat = matrix(0, nrow = 6, ncol=2)
# totalsByYear <- as.data.frame(mat)
# colnames(totalsByYear) <- c("2010", "2011", "2012", "2013", "2014","2015")
# 
# totalsByYear[1,1] <- sum(IPEDS_subset_associate[grep(2010, IPEDS_subset_associate$year), "Grand Total"])
# totalsByYear[2,1] <- sum(IPEDS_subset_associate[grep(2011, IPEDS_subset_associate$year), "Grand Total"])
# totalsByYear[3,1] <- sum(IPEDS_subset_associate[grep(2012, IPEDS_subset_associate$year), "Grand Total"])
# totalsByYear[4,1] <- sum(IPEDS_subset_associate[grep(2013, IPEDS_subset_associate$year), "Grand Total"])
# totalsByYear[5,1] <- sum(IPEDS_subset_associate[grep(2014, IPEDS_subset_associate$year), "Grand Total"])
# 
# totalsByYear[1,2] <- 2010
# totalsByYear[2,2] <- 2011
# totalsByYear[3,2] <- 2012
# totalsByYear[4,2] <- 2013
# totalsByYear[5,2] <- 2014
# 
# #ggplot(totalsByYear) + geom_line( aes(totalsByYear[,2],totalsByYear[,1])) + theme_minimal()
# 
# 
# #ggplot() +
#     geom_line(data = jobsAFAM1, aes(x = data_date, y = Percent.Change), color = "red") +
#     geom_line(data = jobsAFAM2, aes(x = data_date, y = Percent.Change), color = "blue") +
#     xlab('data_date') +
#     ylab('percent.change')

associate <- allDegrees_2010_2015_2[grep("Associate", allDegrees_2010_2015_2$`Award Level Code`),]
bachelor <- allDegrees_2010_2015_2[grep("Bachelor", allDegrees_2010_2015_2$`Award Level Code`),]
master <-allDegrees_2010_2015_2[grep("Master", allDegrees_2010_2015_2$`Award Level Code`),]
phd <- allDegrees_2010_2015_2[grep("Doctor",allDegrees_2010_2015_2$`Award Level Code`),]

associate_2010 <- associate[grep("2010", associate$year),]
associate_2011 <- associate[grep("2011", associate$year),]
associate_2012 <- associate[grep("2012", associate$year),]
associate_2013 <- associate[grep("2013",associate$year),]
associate_2014 <- associate[grep("2014", associate$year),]
associate_2015 <- associate[grep("2015", associate$year),]

bachelor_2010 <- bachelor[grep("2010", bachelor$year),]
bachelor_2011 <- bachelor[grep("2011", bachelor$year),]
bachelor_2012 <- bachelor[grep("2012",bachelor$year),]
bachelor_2013 <- bachelor[grep("2013", bachelor$year),]
bachelor_2014 <- bachelor[grep("2014", bachelor$year),]
bachelor_2015 <- bachelor[grep("2015", bachelor$year),]

master_2010 <- master[grep("2010", master$year),]
master_2011 <- master[grep("2011", master$year),]
master_2012 <- master[grep("2012", master$year),]
master_2013 <- master[grep("2013", master$year),]
master_2014 <- master[grep("2014", master$year),]
master_2015 <- master[grep("2015", master$year),]

phd_2010 <- phd[grep("2010", phd$year),]
phd_2011 <- phd[grep("2011", phd$year),]
phd_2012 <- phd[grep("2012", phd$year),]
phd_2013 <- phd[grep("2013", phd$year),]
phd_2014 <- phd[grep("2014", phd$year),]
phd_2015 <- phd[grep("2015", phd$year),]


vectorToPlot <- array(dim = 144)


vectorToPlot[1] <- sum(master_2010$`White Total`)
vectorToPlot[2] <- sum(master_2011$`White Total`)
vectorToPlot[3] <- sum(master_2012$`White Total`)
vectorToPlot[4] <- sum(master_2013$`White Total`)
vectorToPlot[5] <- sum(master_2014$`White Total`)
vectorToPlot[6] <- sum(master_2015$`White Total`)
vectorToPlot[7] <- sum(bachelor_2010$`White Total`)
vectorToPlot[8] <- sum(bachelor_2011$`White Total`)
vectorToPlot[9] <- sum(bachelor_2012$`White Total`)
vectorToPlot[10] <- sum(bachelor_2013$`White Total`)
vectorToPlot[11] <- sum(bachelor_2014$`White Total`)
vectorToPlot[12] <- sum(bachelor_2015$`White Total`)
vectorToPlot[13] <- sum(associate_2010$`White Total`)
vectorToPlot[14] <- sum(associate_2011$`White Total`)
vectorToPlot[15] <- sum(associate_2012$`White Total`)
vectorToPlot[16] <- sum(associate_2013$`White Total`)
vectorToPlot[17] <- sum(associate_2014$`White Total`)
vectorToPlot[18] <- sum(associate_2015$`White Total`)

vectorToPlot[19] <- sum(phd_2010$`White Total`)
vectorToPlot[20] <- sum(phd_2011$`White Total`)
vectorToPlot[21] <- sum(phd_2012$`White Total`)
vectorToPlot[22] <- sum(phd_2013$`White Total`)
vectorToPlot[23] <- sum(phd_2014$`White Total`)
vectorToPlot[24] <- sum(phd_2015$`White Total`)


vectorToPlot[25] <- sum(master_2010$`Black or African Americanl`)
vectorToPlot[26] <- sum(master_2011$`Black or African Americanl`)
vectorToPlot[27] <- sum(master_2012$`Black or African Americanl`)
vectorToPlot[28] <- sum(master_2013$`Black or African Americanl`)
vectorToPlot[29] <- sum(master_2014$`Black or African Americanl`)
vectorToPlot[30] <- sum(master_2015$`Black or African Americanl`)
vectorToPlot[31] <- sum(bachelor_2010$`Black or African Americanl`)
vectorToPlot[32] <- sum(bachelor_2011$`Black or African Americanl`)
vectorToPlot[33] <- sum(bachelor_2012$`Black or African Americanl`)
vectorToPlot[34] <- sum(bachelor_2013$`Black or African Americanl`)
vectorToPlot[35] <- sum(bachelor_2014$`Black or African Americanl`)
vectorToPlot[36] <- sum(bachelor_2015$`Black or African Americanl`)
vectorToPlot[37] <- sum(associate_2010$`Black or African Americanl`)
vectorToPlot[38] <- sum(associate_2011$`Black or African Americanl`)
vectorToPlot[39] <- sum(associate_2012$`Black or African Americanl`)
vectorToPlot[40] <- sum(associate_2013$`Black or African Americanl`)
vectorToPlot[41] <- sum(associate_2014$`Black or African Americanl`)
vectorToPlot[42] <- sum(associate_2015$`Black or African Americanl`)

vectorToPlot[43] <- sum(phd_2010$`Black or African Americanl`)
vectorToPlot[44] <- sum(phd_2011$`Black or African Americanl`)
vectorToPlot[45] <- sum(phd_2012$`Black or African Americanl`)
vectorToPlot[46] <- sum(phd_2013$`Black or African Americanl`)
vectorToPlot[47] <- sum(phd_2014$`Black or African Americanl`)
vectorToPlot[48] <- sum(phd_2015$`Black or African Americanl`)

vectorToPlot[49] <- sum(master_2010$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[50] <- sum(master_2011$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[51] <- sum(master_2012$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[52] <- sum(master_2013$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[53] <- sum(master_2014$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[54] <- sum(master_2015$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[55] <- sum(bachelor_2010$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[56] <- sum(bachelor_2011$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[57] <- sum(bachelor_2012$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[58] <- sum(bachelor_2013$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[59] <- sum(bachelor_2014$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[60] <- sum(bachelor_2015$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[61] <- sum(associate_2010$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[62] <- sum(associate_2011$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[63] <- sum(associate_2012$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[64] <- sum(associate_2013$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[65] <- sum(associate_2014$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[66] <- sum(associate_2015$`Hispanic or Latino/Hispanic Total`)

vectorToPlot[67] <- sum(phd_2010$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[68] <- sum(phd_2011$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[69] <- sum(phd_2012$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[70] <- sum(phd_2013$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[71] <- sum(phd_2014$`Hispanic or Latino/Hispanic Total`)
vectorToPlot[72] <- sum(phd_2015$`Hispanic or Latino/Hispanic Total`)

vectorToPlot[73] <- sum(master_2010$`American Indian or Alaska Native Total`,master_2010$Asian,na.omit(master_2010$`Two or More Races Total`),master_2010$`Race/Ethnicity Unknown Total`,master_2010$`Nonresident Alien Total`)
vectorToPlot[74] <- sum(master_2011$`American Indian or Alaska Native Total`,master_2011$Asian,master_2011$`Native Hawaiian or Other Pacific Islander Total`,na.omit(master_2011$`Two or More Races Total`),master_2011$`Race/Ethnicity Unknown Total`,master_2011$`Nonresident Alien Total`)
vectorToPlot[75] <- sum(master_2012$`American Indian or Alaska Native Total`,master_2012$Asian,master_2012$`Native Hawaiian or Other Pacific Islander Total`,na.omit(master_2012$`Two or More Races Total`),master_2012$`Race/Ethnicity Unknown Total`,master_2012$`Nonresident Alien Total`)
vectorToPlot[76] <- sum(master_2013$`American Indian or Alaska Native Total`,master_2013$Asian,master_2013$`Native Hawaiian or Other Pacific Islander Total`,na.omit(master_2013$`Two or More Races Total`),master_2013$`Race/Ethnicity Unknown Total`,master_2013$`Nonresident Alien Total`)
vectorToPlot[77] <- sum(master_2014$`American Indian or Alaska Native Total`,master_2014$Asian,master_2014$`Native Hawaiian or Other Pacific Islander Total`,na.omit(master_2014$`Two or More Races Total`),master_2014$`Race/Ethnicity Unknown Total`,master_2014$`Nonresident Alien Total`)
vectorToPlot[78] <- sum(master_2015$`American Indian or Alaska Native Total`,master_2015$Asian,master_2015$`Native Hawaiian or Other Pacific Islander Total`,na.omit(master_2015$`Two or More Races Total`),master_2015$`Race/Ethnicity Unknown Total`,master_2015$`Nonresident Alien Total`)
vectorToPlot[79] <- sum(bachelor_2010$`American Indian or Alaska Native Total`,bachelor_2010$Asian,na.omit(bachelor_2010$`Two or More Races Total`),bachelor_2010$`Race/Ethnicity Unknown Total`,bachelor_2010$`Nonresident Alien Total`)
vectorToPlot[80] <- sum(bachelor_2011$`American Indian or Alaska Native Total`,bachelor_2011$Asian,bachelor_2011$`Native Hawaiian or Other Pacific Islander Total`,na.omit(bachelor_2011$`Two or More Races Total`),bachelor_2011$`Race/Ethnicity Unknown Total`,bachelor_2011$`Nonresident Alien Total`)
vectorToPlot[81] <- sum(bachelor_2012$`American Indian or Alaska Native Total`,bachelor_2012$Asian,bachelor_2012$`Native Hawaiian or Other Pacific Islander Total`,na.omit(bachelor_2012$`Two or More Races Total`),bachelor_2012$`Race/Ethnicity Unknown Total`,bachelor_2012$`Nonresident Alien Total`)
vectorToPlot[82] <- sum(bachelor_2013$`American Indian or Alaska Native Total`,bachelor_2013$Asian,bachelor_2013$`Native Hawaiian or Other Pacific Islander Total`,na.omit(bachelor_2013$`Two or More Races Total`),bachelor_2013$`Race/Ethnicity Unknown Total`,bachelor_2013$`Nonresident Alien Total`)
vectorToPlot[83] <- sum(bachelor_2014$`American Indian or Alaska Native Total`,bachelor_2014$Asian,bachelor_2014$`Native Hawaiian or Other Pacific Islander Total`,na.omit(bachelor_2014$`Two or More Races Total`),bachelor_2014$`Race/Ethnicity Unknown Total`,bachelor_2014$`Nonresident Alien Total`)
vectorToPlot[84] <- sum(bachelor_2015$`American Indian or Alaska Native Total`,bachelor_2015$Asian,bachelor_2015$`Native Hawaiian or Other Pacific Islander Total`,na.omit(bachelor_2015$`Two or More Races Total`),bachelor_2015$`Race/Ethnicity Unknown Total`,bachelor_2015$`Nonresident Alien Total`)
vectorToPlot[85] <- sum(associate_2010$`American Indian or Alaska Native Total`,associate_2010$Asian,na.omit(associate_2010$`Two or More Races Total`),associate_2010$`Race/Ethnicity Unknown Total`,associate_2010$`Nonresident Alien Total`)
vectorToPlot[86] <- sum(associate_2011$`American Indian or Alaska Native Total`,associate_2011$Asian,associate_2011$`Native Hawaiian or Other Pacific Islander Total`,na.omit(associate_2011$`Two or More Races Total`),associate_2011$`Race/Ethnicity Unknown Total`,associate_2011$`Nonresident Alien Total`)
vectorToPlot[87] <- sum(associate_2012$`American Indian or Alaska Native Total`,associate_2012$Asian,associate_2012$`Native Hawaiian or Other Pacific Islander Total`,na.omit(associate_2012$`Two or More Races Total`),associate_2012$`Race/Ethnicity Unknown Total`,associate_2012$`Nonresident Alien Total`)
vectorToPlot[88] <- sum(associate_2013$`American Indian or Alaska Native Total`,associate_2013$Asian,associate_2013$`Native Hawaiian or Other Pacific Islander Total`,na.omit(associate_2013$`Two or More Races Total`),associate_2013$`Race/Ethnicity Unknown Total`,associate_2013$`Nonresident Alien Total`)
vectorToPlot[89] <- sum(associate_2014$`American Indian or Alaska Native Total`,associate_2014$Asian,associate_2014$`Native Hawaiian or Other Pacific Islander Total`,na.omit(associate_2014$`Two or More Races Total`),associate_2014$`Race/Ethnicity Unknown Total`,associate_2014$`Nonresident Alien Total`)
vectorToPlot[90] <- sum(associate_2015$`American Indian or Alaska Native Total`,associate_2015$Asian,associate_2015$`Native Hawaiian or Other Pacific Islander Total`,na.omit(associate_2015$`Two or More Races Total`),associate_2015$`Race/Ethnicity Unknown Total`,associate_2015$`Nonresident Alien Total`)

vectorToPlot[91] <- sum(phd_2010$`American Indian or Alaska Native Total`,phd_2010$Asian,na.omit(phd_2010$`Two or More Races Total`),phd_2010$`Race/Ethnicity Unknown Total`,phd_2010$`Nonresident Alien Total`)
vectorToPlot[92] <- sum(phd_2011$`American Indian or Alaska Native Total`,phd_2011$Asian,phd_2011$`Native Hawaiian or Other Pacific Islander Total`,na.omit(phd_2011$`Two or More Races Total`),phd_2011$`Race/Ethnicity Unknown Total`,phd_2011$`Nonresident Alien Total`)
vectorToPlot[93] <- sum(phd_2012$`American Indian or Alaska Native Total`,phd_2012$Asian,phd_2012$`Native Hawaiian or Other Pacific Islander Total`,na.omit(phd_2012$`Two or More Races Total`),phd_2012$`Race/Ethnicity Unknown Total`,phd_2012$`Nonresident Alien Total`)
vectorToPlot[94] <- sum(phd_2013$`American Indian or Alaska Native Total`,phd_2013$Asian,phd_2013$`Native Hawaiian or Other Pacific Islander Total`,na.omit(phd_2013$`Two or More Races Total`),phd_2013$`Race/Ethnicity Unknown Total`,phd_2013$`Nonresident Alien Total`)
vectorToPlot[95] <- sum(phd_2014$`American Indian or Alaska Native Total`,phd_2014$Asian,phd_2014$`Native Hawaiian or Other Pacific Islander Total`,na.omit(phd_2014$`Two or More Races Total`),phd_2014$`Race/Ethnicity Unknown Total`,phd_2014$`Nonresident Alien Total`)
vectorToPlot[96] <- sum(phd_2015$`American Indian or Alaska Native Total`,phd_2015$Asian,phd_2015$`Native Hawaiian or Other Pacific Islander Total`,na.omit(phd_2015$`Two or More Races Total`),phd_2015$`Race/Ethnicity Unknown Total`,phd_2015$`Nonresident Alien Total`)

vectorToPlot[97] <- sum(master_2010$`Grand Total - Women`)
vectorToPlot[98] <- sum(master_2011$`Grand Total - Women`)
vectorToPlot[99] <- sum(master_2012$`Grand Total - Women`)
vectorToPlot[100] <- sum(master_2013$`Grand Total - Women`)
vectorToPlot[101] <- sum(master_2014$`Grand Total - Women`)
vectorToPlot[102] <- sum(master_2015$`Grand Total - Women`)
vectorToPlot[103] <- sum(bachelor_2010$`Grand Total - Women`)
vectorToPlot[104] <- sum(bachelor_2011$`Grand Total - Women`)
vectorToPlot[105] <- sum(bachelor_2012$`Grand Total - Women`)
vectorToPlot[106] <- sum(bachelor_2013$`Grand Total - Women`)
vectorToPlot[107] <- sum(bachelor_2014$`Grand Total - Women`)
vectorToPlot[108] <- sum(bachelor_2015$`Grand Total - Women`)
vectorToPlot[109] <- sum(associate_2010$`Grand Total - Women`)
vectorToPlot[110] <- sum(associate_2011$`Grand Total - Women`)
vectorToPlot[111] <- sum(associate_2012$`Grand Total - Women`)
vectorToPlot[112] <- sum(associate_2013$`Grand Total - Women`)
vectorToPlot[113] <- sum(associate_2014$`Grand Total - Women`)
vectorToPlot[114] <- sum(associate_2015$`Grand Total - Women`)
vectorToPlot[115] <- sum(phd_2010$`Grand Total - Women`)
vectorToPlot[116] <- sum(phd_2011$`Grand Total - Women`)
vectorToPlot[117] <- sum(phd_2012$`Grand Total - Women`)
vectorToPlot[118] <- sum(phd_2013$`Grand Total - Women`)
vectorToPlot[119] <- sum(phd_2014$`Grand Total - Women`)
vectorToPlot[120] <- sum(phd_2015$`Grand Total - Women`)

vectorToPlot[121] <- sum(master_2010$`Grand Total`) - sum(master_2010$`Grand Total - Women`)
vectorToPlot[122] <- sum(master_2011$`Grand Total`) - sum(master_2011$`Grand Total - Women`)
vectorToPlot[123] <- sum(master_2012$`Grand Total`) - sum(master_2012$`Grand Total - Women`)
vectorToPlot[124] <- sum(master_2013$`Grand Total`) - sum(master_2013$`Grand Total - Women`)
vectorToPlot[125] <- sum(master_2014$`Grand Total`) - sum(master_2014$`Grand Total - Women`)
vectorToPlot[126] <- sum(master_2015$`Grand Total`) - sum(master_2015$`Grand Total - Women`)
vectorToPlot[127] <- sum(bachelor_2010$`Grand Total`) - sum(bachelor_2010$`Grand Total - Women`)
vectorToPlot[128] <- sum(bachelor_2011$`Grand Total`) - sum(bachelor_2011$`Grand Total - Women`)
vectorToPlot[129] <- sum(bachelor_2012$`Grand Total`) - sum(bachelor_2012$`Grand Total - Women`)
vectorToPlot[130] <- sum(bachelor_2013$`Grand Total`) - sum(bachelor_2013$`Grand Total - Women`)
vectorToPlot[131] <- sum(bachelor_2014$`Grand Total`) - sum(bachelor_2014$`Grand Total - Women`)
vectorToPlot[132] <- sum(bachelor_2015$`Grand Total`) - sum(bachelor_2015$`Grand Total - Women`)
vectorToPlot[133] <- sum(associate_2010$`Grand Total`) - sum(associate_2010$`Grand Total - Women`)
vectorToPlot[134] <- sum(associate_2011$`Grand Total`) - sum(associate_2011$`Grand Total - Women`)
vectorToPlot[135] <- sum(associate_2012$`Grand Total`) - sum(associate_2012$`Grand Total - Women`)
vectorToPlot[136] <- sum(associate_2013$`Grand Total`) - sum(associate_2013$`Grand Total - Women`)
vectorToPlot[137] <- sum(associate_2014$`Grand Total`) - sum(associate_2014$`Grand Total - Women`)
vectorToPlot[138] <- sum(associate_2015$`Grand Total`) - sum(associate_2015$`Grand Total - Women`)

vectorToPlot[139] <- sum(phd_2010$`Grand Total`) - sum(phd_2010$`Grand Total - Women`)
vectorToPlot[140] <- sum(phd_2011$`Grand Total`) - sum(phd_2011$`Grand Total - Women`)
vectorToPlot[141] <- sum(phd_2012$`Grand Total`) - sum(phd_2012$`Grand Total - Women`)
vectorToPlot[142] <- sum(phd_2013$`Grand Total`) - sum(phd_2013$`Grand Total - Women`)
vectorToPlot[143] <- sum(phd_2014$`Grand Total`) - sum(phd_2014$`Grand Total - Women`)
vectorToPlot[144] <- sum(phd_2015$`Grand Total`) - sum(phd_2015$`Grand Total - Women`)

vectorToPlot <- as.data.frame(vectorToPlot)
vectorToPlot$year <- NA
vectorToPlot$race <- NA
vectorToPlot$percent <- NA
vectorToPlot$total <- NA
vectorToPlot$gender <- NA
vectorToPlot$degree <- NA
vectorToPlot$race[1:24] <- "White"
vectorToPlot$race[25:48] <- "Black"
vectorToPlot$race[49:72] <- "Hispanic/Latino"
vectorToPlot$race[73:96] <- "Asian/Other"
vectorToPlot$gender[97:120] <-"Female"
vectorToPlot$gender[121:144] <-"Male"

sixes <- seq(1,139,6)
years <- c(2010, 2011, 2012, 2013, 2014, 2015)
for(i in 2:144)
{
  vectorToPlot$year[i+1] <- years[(i%%6)+1]
}
vectorToPlot$year[1] <- 2010
vectorToPlot$year[2] <- 2011
degreeList <- c("Master's", "Bachelor's", "Associate's", "PhD")


  vectorToPlot$degree[121:144] <- c(rep(degreeList[1], 6), rep(degreeList[2],6), rep(degreeList[3], 6), rep(degreeList[4],6))


for(i in 1:length(sixes))
{
  vectorToPlot$total[sixes[i]] <- sum(vectorToPlot$vectorToPlot[(sixes[i]):(sixes[i]+5)])
}

for(i in 1:length(sixes))
{
  vectorToPlot$percent[sixes[i]:(sixes[i]+5)] <- (((vectorToPlot$vectorToPlot[sixes[i]:(sixes[i]+5)]))/vectorToPlot$total[sixes[i]])
}

vectorToPlotMasters <- rbind(vectorToPlot[1:6,], vectorToPlot[25:30,], vectorToPlot[49:54,], vectorToPlot[73:78,])
vectorToPlotBach <- rbind(vectorToPlot[7:12,], vectorToPlot[31:36,], vectorToPlot[55:60,], vectorToPlot[79:84,])
vectorToPlotAss <- rbind(vectorToPlot[13:18,], vectorToPlot[37:42,], vectorToPlot[61:66,], vectorToPlot[85:90,])
vectorToPlotPHD <- rbind(vectorToPlot[19:24,], vectorToPlot[43:48,], vectorToPlot[67:72,], vectorToPlot[91:96,])

vectorToPlotGender <- vectorToPlot[97:144,]


for (i in 1:length(sixes))
     {
  vectorToPlot$year[sixes[i]] = 2015

}

ggplot(vectorToPlot) + geom_line( aes(totalsByYear[,2],totalsByYear[,1])) + theme_minimal()


# FAILED MOSAIC PLOT #########################################################################################
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




##MADDIES HEAT MAP
data_heatmap <- IPEDS_2010_2015_degrees %>% group_by(y_axis_var, x_axis_var) %>% dplyr::summarise(count = n()) 

png("~/git/stem_edu/data/stem_edu/final/TotalStemTrend.png", height = 1000, width = 2000)
ggplot() +
  geom_tile(data = vectorToPlot[1:96,], 
            mapping = aes(x = year, 
                          y = race, fill = percent), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(.11, .25), breaks= seq(.11, .25, .07)) +
  labs(title = "Percentage of Total STEM Degrees", subtitle= "Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity")+ theme_minimal() +
  theme(text=element_text(size=48),axis.text=element_text(size=25),axis.title=element_text(size=42),
        axis.text.x=element_text(angle=0,hjust=1), axis.text.y=element_text(angle=90,hjust=0,size=30)) +
  scale_x_continuous(breaks=vectorToPlot$year)
dev.off()

png("~/git/stem_edu/data/stem_edu/final/MastersDegreeTrend.png", height = 1000, width = 2000)
ggplot() +
  geom_tile(data = vectorToPlotMasters, 
            mapping = aes(x = year, 
                          y = race, fill = percent), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(.11, .25), breaks= seq(.11, .25, .07)) +
  labs(title = "Percentage of Total STEM Master's Degrees Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity") +theme_minimal() +
  theme(text=element_text(size=48),axis.text=element_text(size=25),axis.title=element_text(size=42),
        axis.text.x=element_text(angle=0,hjust=1), axis.text.y=element_text(angle=90,hjust=0,size=30)) +
  scale_x_continuous(breaks=vectorToPlot$year) 
dev.off()


png("~/git/stem_edu/data/stem_edu/final/BachelorsDegreeTrend.png", height = 1000, width = 2000)
ggplot() +
  geom_tile(data = vectorToPlotBach, 
            mapping = aes(x = year, 
                          y = race, fill = percent), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(.11, .25), breaks= seq(.11, .25, .07)) +
  labs(title="Percentage of Total STEM Bachelor's Degrees Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity") + theme_minimal() +
  theme(text=element_text(size=48),axis.text=element_text(size=25),axis.title=element_text(size=42),
        axis.text.x=element_text(angle=0,hjust=1), axis.text.y=element_text(angle=90,hjust=0,size=30)) +
  scale_x_continuous(breaks=vectorToPlot$year) 
dev.off()

png("~/git/stem_edu/data/stem_edu/final/AssociatesDegreeTrend.png", height = 1000, width = 2000)
ggplot() +
  geom_tile(data = vectorToPlotAss, 
            mapping = aes(x = year, 
                          y = race, fill = percent), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(.11, .25), breaks= seq(.11, .25, .07)) +
  labs(title = "Percentage of Total STEM Associate's Degrees Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity") + theme_minimal() +
  theme(text=element_text(size=48),axis.text=element_text(size=25),axis.title=element_text(size=42),
        axis.text.x=element_text(angle=0,hjust=1), axis.text.y=element_text(angle=90,hjust=.5,size=30)) +
  scale_x_continuous(breaks=vectorToPlot$year) 
dev.off()

png("~/git/stem_edu/data/stem_edu/final/PHDTrend.png", height =1000, width = 2000)
ggplot() +
  geom_tile(data = vectorToPlotPHD, 
            mapping = aes(x = year, 
                          y = race, fill = percent), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(.11, .25), breaks= seq(.11, .25, .07)) +
  labs(title = "Percentage of Total STEM PhDs Earned in a Six Year Period", x = "Year",
       y = "Race/Ethnicity") + theme_minimal() +
  theme(text=element_text(size=48),axis.text=element_text(size=25),axis.title=element_text(size=42),
        axis.text.x=element_text(angle=0,hjust=1, size=30), axis.text.y=element_text(angle=90,hjust=0,size=30)) +
  scale_x_continuous(breaks=vectorToPlot$year) 
dev.off()


png("~/git/stem_edu/data/stem_edu/final/GenderTrend.png", height = 1000, width = 2000)

vectorToPlotGender$percent
ggplot() +
  geom_tile(data = vectorToPlotGender, 
            mapping = aes(x = year, 
                          y = gender, fill = percent), na.rm=TRUE) +
  scale_fill_gradient(low = 'white', high = 'red', limits = c(.11, .25), breaks= seq(.11, .25, .07)) +
  labs(title = "Percentage of Total STEM Degrees Earned in a Six Year Period", x = "Year",
       y = "Gender") + theme_minimal() +
  theme(text=element_text(size=48),axis.text=element_text(size=25),axis.title=element_text(size=42),
       axis.text.x=element_text(angle=0,hjust=1,size=30), axis.text.y=element_text(angle=90,hjust=0,size=30)) +
  scale_x_continuous(breaks=vectorToPlot$year) 
dev.off()









ipedsM<-melt(IPEDS_2010_2015_degrees,id=c("unitid","institution name","year","First or Second Major",
                                         "CIP Code","CipTitle","Award Level Code","Grand Total - Women","IDX_C"),
             value.name="count")

save(ipedsM, file = "~/git/stem_edu/data/stem_edu/working/IPEDS_working/meltTable.RData")
