library(readxl)
library(dplyr)
#https://www.dhp.virginia.gov/Boards/Nursing/
pass <- read_excel("src/sarah/pass-rates.xls", sheet = c(1), col_names = TRUE)
pass2 <- read_excel("src/sarah/pass-rates.xls", sheet = c(2), col_names = TRUE)

pass <- select(pass, c("X__1", "X__6", "2014", "X__7", "X__9", "2015", "X__10", "X__12", "2016", "X__13", "X__15", "2017", "X__16", "X__18", "20.18", "X__19"))
pass2 <- select(pass2, c("X__1", "X__5", "2014", "X__6", "X__8", "2015", "X__9", "X__11", "2016", "X__12", "X__14", "2017", "X__15", "X__17", "2018", "X__18"))

names(pass) <- c("institution", "pass.2014", "cand.2014", "per.2014", "pass.2015", "cand.2015", "per.2015", "pass.2016", "cand.2016", "per.2016", "pass.2017", "cand.2017", "per.2017", "pass.2018", "cand.2018", "per.2018")
names(pass2) <- c("institution", "pass.2014", "cand.2014", "per.2014", "pass.2015", "cand.2015", "per.2015", "pass.2016", "cand.2016", "per.2016", "pass.2017", "cand.2017", "per.2017", "pass.2018", "cand.2018", "per.2018")

pass <- rbind(pass, pass2)
pass <-pass[rowSums(is.na(pass)) != ncol(pass), ]

institutions <- c("Bryant & Stratton College US28403500", "ECPI University,  ADN - Richmond US28401500", "Fortis College, Richmond US28402300", "Fortis College Richmond - US28408900", "J. Sargeant Reynolds Community Col  US28406300", "J. Sargeant Reynolds CC LPN to RN US28401100", "J. Sargeant Reynolds CC LPN to ADN US28401200", "John Tyler Community Col  US28407300", "John Tyler Comm Col, Hybrid US28404600", "New River Community College US28406100", "Southside Regional Medical Center US28400700")

pass <- pass %>% filter(institution %in% institutions)
pass$per.2018 <- as.numeric(pass$per.2018)

pass$institution[pass$institution == "Bryant & Stratton College US28403500"] <- "Bryant & Stratton \nCollege"
pass$institution[pass$institution == "ECPI University,  ADN - Richmond US28401500"] <- "ECPI University, \nADN - Richmond"
pass$institution[pass$institution == "Fortis College, Richmond US28402300"] <- "Fortis College, \nRichmond"
pass$institution[pass$institution == "Fortis College Richmond - US28408900"] <- "Fortis College, \nRichmond"
pass$institution[pass$institution == "J. Sargeant Reynolds Community Col  US28406300"] <- "J. Sargeant Reynolds CC"
pass$institution[pass$institution == "J. Sargeant Reynolds CC LPN to RN US28401100"] <- "J. Sargeant Reynolds \nCC LPN to RN"
pass$institution[pass$institution == "J. Sargeant Reynolds CC LPN to ADN US28401200"] <- "J. Sargeant Reynolds \nCC LPN to ADN"
pass$institution[pass$institution == "John Tyler Community Col  US28407300"] <- "John Tyler CC"
pass$institution[pass$institution == "John Tyler Comm Col, Hybrid US28404600"] <- "John Tyler CC, Hybrid"
pass$institution[pass$institution == "New River Community College US28406100"] <- "New River CC"
pass$institution[pass$institution == "Southside Regional Medical Center US28400700"] <- "Southside Regional \nMedical Center"

pass <- pass[-9,]

pass$type = c("For-Profit", "For-Profit","For-Profit", "For-Profit", "Community College", "Community College", "Community College", "Community College", "Community College", "For-Profit")


library(ggplot2)
ggplot(data=pass, aes(x= reorder(institution, -per.2018), y=per.2018, fill = type))+
  geom_bar(stat= "identity")+
  theme(
    plot.title= element_text(size=20, , face="bold", hjust= 0.5),
    axis.text.x = element_text(size=10, angle = 10),
    axis.title = element_text(size=14),
    legend.position = "bottom",
    legend.title = element_text(size=0))+
  labs(title = "NCLEX-RN Passage Rates for Associate's Degrees \nin Nursing in Richmond and Blacksburg", x= "Institution/Program", y="NCLEX-RN Passage")

