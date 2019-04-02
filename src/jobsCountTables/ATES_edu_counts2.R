#repeat sankey diagram for Post-secondary certificate type
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
ates <- select(orig_ates, eduattn, psfos)

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
ates$psfos <- as.character(ates$psfos)
ates$psfos <- str_replace(ates$psfos, '^1$', "Accounting, finance, insurance, real est")
ates$psfos <-str_replace(ates$psfos, '^2$', "Administrative support")
ates$psfos <-str_replace(ates$psfos, '^3$', "Agriculture")
ates$psfos <-str_replace(ates$psfos, '^4$', "Audio, broadcasting, multimedia")
ates$psfos <-str_replace(ates$psfos, '^5$', "Business management, administration")
ates$psfos <-str_replace(ates$psfos, '^6$', "Computer science or information technology")
ates$psfos <-str_replace(ates$psfos, '^7$', "Construction trades")
ates$psfos <-str_replace(ates$psfos, '^8$', "Cosmetology")
ates$psfos <-str_replace(ates$psfos, '^9$', "Culinary arts")
ates$psfos <-str_replace(ates$psfos, '^10$', "Education")
ates$psfos <- str_replace(ates$psfos, '^11$', "Engineering technologies or drafting")
ates$psfos <-str_replace(ates$psfos, '^12$', "Fine arts or music")
ates$psfos <-str_replace(ates$psfos, '^13$', "Funeral service or mortuary science")
ates$psfos <-str_replace(ates$psfos, '^14$', "Healthcare")
ates$psfos <-str_replace(ates$psfos, '^15$', "Law enforcement, security, or firefighting")
ates$psfos <-str_replace(ates$psfos, '^16$', "Law or legal studies")
ates$psfos <-str_replace(ates$psfos, '^17$', "Liberal arts")
ates$psfos <-str_replace(ates$psfos, '^18$', "Manufacturing or production")
ates$psfos <-str_replace(ates$psfos, '^19$', "Mechanic or repair technologies")
ates$psfos <-str_replace(ates$psfos, '^20$', "Transportation")
ates$psfos <- str_replace(ates$psfos, '^21$', "Other")
ates$psfos <-str_replace(ates$psfos, '^\\-1$', "Valid Skip")

#population counts across education
tab1 <- table(ates$eduattn)
tab1

#take out valid skips
tabs_noskip <-data.frame(table(filter(ates,ates$psfos != "Valid Skip")))
#down from 47744 to 6676

#take out higher education
set_2 <- tabs_noskip
colnames(set_2) <- c('Edu_Attain', 'Post_Cert_Type','Freq')

set_2 <- subset(set_2, set_2$Edu_Attain != "1+ year College")
set_2 <- subset(set_2, set_2$Edu_Attain != "Associate Degree")
set_2 <- subset(set_2, set_2$Edu_Attain != "Master's Degree")
set_2 <- subset(set_2, set_2$Edu_Attain != "Bachelor's Degree")
set_2 <- subset(set_2, set_2$Edu_Attain != "Professional past Bachelor")
set_2 <- subset(set_2, set_2$Edu_Attain != "Doctorate")

#doing this step reduces the amount of rows from 6676 to 2220
colnames(set_2) <- c('Edu_Attain', 'Post_Cert_Type','Freq')

#only look at stem jobs
stem_jobs <- c("Engineering technologies or drafting",
               "Computer science or information technology",
               "Healthcare",
               "Mechanic or repair technologies")
set_2 <- set_2[set_2$Post_Cert_Type %in% stem_jobs,]
#this drops us to 814 observations

#diagram time
links <- set_2
nodes <- data.frame(node = c(1:8),
                    name = c(unique(as.character(set_2$Edu_Attain)),unique(as.character(set_2$Post_Cert_Type))))
links$IDsource=match(links$Edu_Attain, nodes$name)-1
links$IDtarget=match(links$Post_Cert_Type, nodes$name)-1
networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                         Source = 'IDsource',
                         Target = 'IDtarget',
                         Value = 'Freq',
                         NodeID = 'name', fontSize = 15)

