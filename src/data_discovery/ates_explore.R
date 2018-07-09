#########################################################
###           ATES Data - Exploratory Code            ###
###                   Emily Sheen                     ###
#########################################################

load("./data/stem_edu/original/ATES/ates_pu_pert.rdata")

ates <- ates_pu_pert
View(ates)

colnames(ates)
levels(as.factor(ates$path))
levels(as.factor(ates$CNINVALID1))
length(which(ates$CNINVALID1 == 1))

#Analysis of first (most important) certifications
