# make sankey of nces f3stu data

F3A05 # type of hs credential
F3A13B # degree last pursued
# degree earned??
F3ONET2CURR # current job 2006

# load data
load("data/stem_edu/original/NCES/els_02_12_byf3stu_v1_0.rdata") # byf3sch (student)
f3stu <- els_02_12_byf3stu_v1_0
rm(els_02_12_byf3stu_v1_0)

unique(f3stu$F3A05) # 1, 3
sort(unique(f3stu$F3A13B)) # 1  2  3  4  5  7  8 10
unique(f3stu$F3ONET2CURR) # 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55

library(dplyr)
nces <- f3stu[, c("F3A05","F3A13B","F3ONET2CURR")]
nces <- nces %>% filter(F3A05 %in% c(1,3) &
                                  F3A13B %in% c(1,2,3,4,5,7,8,10) &
                                  F3ONET2CURR %in% c(11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55))
nces[nces$F3A05==1,1] <- "Diploma"
nces[nces$F3A05==3,1] <- "GED"
nces[nces$F3A13B==1,2] <- "Undergrad license/cert"
nces[nces$F3A13B==2,2] <- "Associate's degree"
nces[nces$F3A13B==3,2] <- "Bachelor's degree"
nces[nces$F3A13B==4,2] <- "Post-bacc certificate"
nces[nces$F3A13B==5,2] <- "Master's degree"
nces[nces$F3A13B==7,2] <- "PhD (research/scholarship)"
nces[nces$F3A13B==8,2] <- "PhD (professional practice"
nces[nces$F3A13B==10,2] <- "Not seeking degree"

table_sankey1 <- as.data.frame(table(as.character(nces$F3A05),
                                     as.character(nces$F3A13B)), stringsAsFactors = FALSE)
table_sankey2 <- as.data.frame(table(as.character(nces$F3A13B),
                                     as.character(nces$F3ONET2CURR)), stringsAsFactors = FALSE)
links <- rbind(table_sankey1, table_sankey2)

codes <- as.data.frame(as.integer(seq(0,27,1)))
colnames(codes) <- "number"
codes$name <- c("Diploma", "GED", unique(links$Var2))

links <- merge(links, codes, by.x = "Var1", by.y = "name", all.x = TRUE)
colnames(links)[4] <- "source"
links <- merge(links, codes, by.x = "Var2", by.y = "name", all.x = TRUE)
colnames(links)[5] <- "target"

name <- as.data.frame(codes$name, stringsAsFactors = FALSE)
colnames(name) <- "name"

library(networkD3)
sankey_data <- list(nodes = name, links = links)

sankeyNetwork(Links = sankey_data$links, Nodes = sankey_data$nodes, Source = "source",
              Target = "target", Value = "Freq", NodeID = "name",
              units = "freq", fontSize = 16, nodeWidth = 30)
