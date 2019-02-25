#Generating a population counts for educational attainment and then sankey diagrams
library(dplyr)
library(data.table)
library(stringr)
library(igraph)
library(networkD3)
#load in data
load('./data/stem_edu/original/ATES/ates_pu_pert.rdata')
orig_ates <- data.table(ates_pu_pert)
rm(ates_pu_pert)

#extract the variables that we want
ates <- select(orig_ates, eduattn, CNFIELD1)

#make table of frequency
table(ates)

#####start replacing numbers with names####
ates$eduattn <- as.character(ates$eduattn)
ates$eduattn <- str_replace(ates$eduattn, '^1$', "No GED")
ates$eduattn <-str_replace(ates$eduattn, '^2$', "High School")
ates$eduattn <-str_replace(ates$eduattn, '^3$', "GED/alt HS")
ates$eduattn <-str_replace(ates$eduattn, '^4$', "< 1 year College")
ates$eduattn <-str_replace(ates$eduattn, '^5$', "1+ year College")
ates$eduattn <-str_replace(ates$eduattn, '^6$', "Associate Degree")
ates$eduattn <-str_replace(ates$eduattn, '^7$', "Bachelor's Degree")
ates$eduattn <-str_replace(ates$eduattn, '^8$', "Master's Degree")
ates$eduattn <-str_replace(ates$eduattn, '^9$', "Professional past Bachelor")
ates$eduattn <-str_replace(ates$eduattn, '^10$', "Doctorate")

#####label again####
ates$CNFIELD1 <- as.character(ates$CNFIELD1)
ates$CNFIELD1 <- str_replace(ates$CNFIELD1, '^1$', "Architecture")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^2$', "Engineering")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^3$', "Computers and IT")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^4$', "Other science and math")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^5$', "Accounting")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^6$', "Other Business")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^7$', "Finance and Insurance")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^8$', "Real Estate")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^9$', "Basic life support")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^10$', "Health practitioner")
ates$CNFIELD1 <- str_replace(ates$CNFIELD1, '^11$', "Nursing")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^12$', "Other health care")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^13$', "Cosmetology")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^14$', "Childcare")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^15$', "Other personal care")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^16$', "Law and legal support")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^17$', "Public Safety")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^18$', "Social work and counseling")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^19$', "Environmental, water, and food safety")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^20$', "Other public and social services")
ates$CNFIELD1 <- str_replace(ates$CNFIELD1, '^21$', "K-12 teaching")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^22$', "Other instruction and training")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^23$', "Construction")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^24$', "Vehicle Maintenance")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^25$', "Transportation and materials moving")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^26$', "Other trades")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^27$', "Other fields")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^\\-8$', "Uncodable response")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^\\-9$', "Missing")
ates$CNFIELD1 <-str_replace(ates$CNFIELD1, '^\\-1$', "Valid Skip")

#population counts across education
tab1 <- table(ates$eduattn)
tab1



#edgelist again
tabs <- data.frame(table(ates))
View(tabs)
colnames(tabs) <- c('Edu_Attain', 'Cert_Type','Freq')

links <- tabs
nodes <- data.frame(node = c(1:40),
                    name = c(unique(ates$eduattn),unique(ates$CNFIELD1)))
#reformat data to match between links and nodes dataframes
links$IDsource=match(links$Edu_Attain, nodes$name)-1
links$IDtarget=match(links$Cert_Type, nodes$name)-1
#sankey diagram
networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                         Source = 'IDsource',
                         Target = 'IDtarget',
                         Value = 'Freq',
                         NodeID = 'name')
#remove the valid skips
tabs_noskip <-data.frame(table(filter(ates,ates$CNFIELD1 != "Valid Skip")))
#doing this step reduces the amount of rows from 47,744 to 11,744

#take out higher education
set_2 <- tabs_noskip
colnames(set_2) <- c('Edu_Attain', 'Cert_Type','Freq')
set_2 <- subset(set_2, set_2$Edu_Attain != "1+ year College")
set_2 <- subset(set_2, set_2$Edu_Attain != "Associate Degree")
set_2 <- subset(set_2, set_2$Edu_Attain != "Master's Degree")
set_2 <- subset(set_2, set_2$Edu_Attain != "Bachelor's Degree")
set_2 <- subset(set_2, set_2$Edu_Attain != "Professional past Bachelor")
set_2 <- subset(set_2, set_2$Edu_Attain != "Doctorate")

#doing this step reduces the amount of rows from 11,744 to 2151
colnames(set_2) <- c('Edu_Attain', 'Cert_Type','Freq')

#only look at stem jobs
stem_jobs <- c("Engineering","Computers and IT", "Other science and math","Health practitioner","Nursing",
               "Other healthcare","Environmental, water, and food safety")
set_2 <- set_2[set_2$Cert_Type %in% stem_jobs,]
#this drops us to 185 observations

links <- set_2
nodes <- data.frame(node = c(1:10),
                    name = c(unique(as.character(set_2$Edu_Attain)),unique(as.character(set_2$Cert_Type))))
links$IDsource=match(links$Edu_Attain, nodes$name)-1
links$IDtarget=match(links$Cert_Type, nodes$name)-1
networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                         Source = 'IDsource',
                         Target = 'IDtarget',
                         Value = 'Freq',
                         NodeID = 'name', fontSize = 15)
#produce a table from the last diagram without skips
tabs3 <- table(set_2$Edu_Attain, set_2$Cert_Type)

#old network plotting
# network <- graph_from_data_frame(tabs,directed = F)
# E(network)$Freq
#
# E(network)$width <- 1+E(network)$Freq/500
# plot(network)

