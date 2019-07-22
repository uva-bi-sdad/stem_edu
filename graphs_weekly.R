library(dplyr)
library(data.table)
main <- read.csv("data/stem_edu/working/resume_with_bachelors/resume_with_bachelors_b")

after <- main %>% group_by(bachelors) %>% summarise(count = n())

edu <- fread("../stem_edu/data/stem_edu/working/MSA_Resumes/blacksburg_edu.txt")




