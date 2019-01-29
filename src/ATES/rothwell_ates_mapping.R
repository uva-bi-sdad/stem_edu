library(readr)
library(readxl)
library(dplyr)
library(magrittr)

ates_code_values <- read_csv("data/stem_edu/original/ATES/ates_code_values.csv")
load("~/git/stem_edu/data/stem_edu/original/ATES/ates_pu_pert.rdata")
rothwell_probabilities <- read_excel("data/stem_edu/original/ATES/Rothwell Probabilities.xlsx", sheet = "Prob")
View(ates_code_values)
View(head(ates_pu_pert))

ates_pu_pert %>%
  group_by(cnmain) %>%
  summarise(n = n())  # 1 = Yes

colnames(ates_code_values) <- c("varname", "varcode", "Description")
ates_pu_pert$eduattn <- as.character(ates_pu_pert$eduattn)
ates_pu_pert$CNFIELD1 <- as.character(ates_pu_pert$CNFIELD1)
rothwell_probabilities$varcode <- as.character(rothwell_probabilities$varcode)

summary <- ates_pu_pert %>%
  group_by(cnmain, eduattn, CNFIELD1) %>%
  summarise(n = n()) %>%
  left_join((ates_code_values %>% filter(varname == "EDUATTNF")), by = c("eduattn" = "varcode")) %>%
  left_join((ates_code_values %>% filter(varname == "CNFIELD1F")), by = c("CNFIELD1" = "varcode")) %>%
  arrange(as.integer(eduattn), as.integer(CNFIELD1)) %>%
  mutate(cert = recode(cnmain,
                       `1` = "Active Certificate",
                       `2` = "None")) %>%
  select(cnmain, cert, eduattn ,  Description.x, CNFIELD1, Description.y, n)  %>%
  left_join(rothwell_probabilities, by = c("CNFIELD1" = "varcode")) %>%
  mutate(Prob_Perc = as.numeric(Probability)/100,
         Size = n*Prob_Perc)

write_csv(summary, path = "data/stem_edu/working/Rothwell_ATES_attempt.csv")


