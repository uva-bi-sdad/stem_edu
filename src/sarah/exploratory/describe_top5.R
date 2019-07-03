library(dplyr)
library(ggplot2)

#Read in top5 data
b_tot <- read.csv("data/stem_edu/working/skills_by_occupation/top5_rb_separate/b_top5-stw-jobskill.csv")
r_tot <- read.csv("data/stem_edu/working/skills_by_occupation/top5_rb_separate/r_top5-stw-jobskill.csv")

#Onetnames of top 5 for blacksburg and richmond
b_onetnames <- unique(b_tot$onetname)
r_onetnames <- unique(r_tot$onetname)


