#########################################################
###          IPEDS Data - Exploratory Code            ###
###                   Emily Sheen                     ###
#########################################################

################## Old IPEDS Data #######################

library("dplyr")
unzip("./data/stem_edu/original/IPEDS_data_zipped/IPEDS_2015_degrees.zip",
      exdir = "./data/stem_edu/original/DSPG2018DataDiscovery/IPEDS")

pathfile <- "./data/stem_edu/original/DSPG2018DataDiscovery/IPEDS/CSV_7112017-625.csv"
degrees15 <- read.csv(pathfile)

degrees15 <- as.data.frame(degrees15)
levels(degrees15$C2015_A.Award.Level.code)

assoc <- degrees15 %>% filter(C2015_A.Award.Level.code =="Associate's degree")
View(assoc)

levels(assoc$CipTitle)


