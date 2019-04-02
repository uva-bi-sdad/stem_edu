#repeat sankey diagram for Work experience programs
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
ates <- select(orig_ates, eduattn, wefolp)

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
ates$wefolp <- as.character(ates$wefolp)
ates$wefolp <- str_replace(ates$wefolp, '^1$', "Carpenter")
ates$wefolp <-str_replace(ates$wefolp, '^2$', "Electrician")
ates$wefolp <-str_replace(ates$wefolp, '^3$', "Plumber or pipefitter")
ates$wefolp <-str_replace(ates$wefolp, '^4$', "Sheet metal worker or steel worker")
ates$wefolp <-str_replace(ates$wefolp, '^5$', "Other building and construction trades")
ates$wefolp <-str_replace(ates$wefolp, '^6$', "Medical doctor")
ates$wefolp <-str_replace(ates$wefolp, '^7$', "Nursing or nursing assistant")
ates$wefolp <-str_replace(ates$wefolp, '^8$', "Other healthcare")
ates$wefolp <-str_replace(ates$wefolp, '^9$', "Accounting, finance, or insurance")
ates$wefolp <-str_replace(ates$wefolp, '^10$', "Chef, cook, or food preparation")
ates$wefolp <- str_replace(ates$wefolp, '^11$', "Computer networking or IT")
ates$wefolp <-str_replace(ates$wefolp, '^12$', "Cosmetology")
ates$wefolp <-str_replace(ates$wefolp, '^13$', "Driving, piloting, other transportation")
ates$wefolp <-str_replace(ates$wefolp, '^14$', "Engineering or architecture")
ates$wefolp <-str_replace(ates$wefolp, '^15$', "Funeral service or mortuary science")
ates$wefolp <-str_replace(ates$wefolp, '^16$', "Law enforcement, security, firefighting")
ates$wefolp <-str_replace(ates$wefolp, '^17$', "Legal practice")
ates$wefolp <-str_replace(ates$wefolp, '^18$', "Machinist or tool and die maker")
ates$wefolp <-str_replace(ates$wefolp, '^19$', "Management or administration")
ates$wefolp <-str_replace(ates$wefolp, '^20$', "Mechanic or repair work")
ates$wefolp <- str_replace(ates$wefolp, '^21$', "Printing")
ates$wefolp <- str_replace(ates$wefolp, '^22$', "Social work, counseling, or religious ")
ates$wefolp <- str_replace(ates$wefolp, '^23$', "Teaching")
ates$wefolp <- str_replace(ates$wefolp, '^24$', "Utility or telecommunications technician")
ates$wefolp <- str_replace(ates$wefolp, '^25$', "Other")
ates$wefolp <- str_replace(ates$wefolp, '^26$', "TV, radio, and broadcasting")
ates$wefolp <-str_replace(ates$wefolp, '^\\-1$', "Valid Skip")

#population counts across education
tab1 <- table(ates$eduattn)
tab1

#take out valid skips
tabs_noskip <-data.frame(table(filter(ates,ates$wefolp != "Valid Skip")))
#down from 47744 to 10,931

#take out higher education
set_2 <- tabs_noskip
colnames(set_2) <- c('Edu_Attain', 'work_exp_pgm','Freq')

set_2 <- subset(set_2, set_2$Edu_Attain != "1+ year College")
set_2 <- subset(set_2, set_2$Edu_Attain != "Associate Degree")
set_2 <- subset(set_2, set_2$Edu_Attain != "Master's Degree")
set_2 <- subset(set_2, set_2$Edu_Attain != "Bachelor's Degree")
set_2 <- subset(set_2, set_2$Edu_Attain != "Professional past Bachelor")
set_2 <- subset(set_2, set_2$Edu_Attain != "Doctorate")

#doing this step reduces the amount of rows from 10,931 to 1186
colnames(set_2) <- c('Edu_Attain', 'work_exp_pgm','Freq')

#only look at stem jobs
stem_jobs <- c("Engineering or architecture",
               "Computer networking or IT",
               "Electrician",
               "Medical Doctor",
               "Nursing or Nursing Assistant",
               "Other healthcare",
               "Mechanic or repair work")
set_2 <- set_2[set_2$work_exp_pgm %in% stem_jobs,]
#this drops us to 354 observations

#diagram time
links <- set_2
nodes <- data.frame(node = c(1:9),
                    name = c(unique(as.character(set_2$Edu_Attain)),unique(as.character(set_2$work_exp_pgm))))
links$IDsource=match(links$Edu_Attain, nodes$name)-1
links$IDtarget=match(links$work_exp_pgm, nodes$name)-1
networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                         Source = 'IDsource',
                         Target = 'IDtarget',
                         Value = 'Freq',
                         NodeID = 'name', fontSize = 15)

