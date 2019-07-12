library(irr)
library(data.table)
library(tidyverse)

skillcluster <- fread("data/stem_edu/working/hard_soft_skills/for_validation/skillcluster_classified_together.csv")
skillcluster_verify <- skillcluster[,c("Alyssa","Calvin","Sarah")]

kappam.fleiss(skillcluster_verify)

skillclusterfamily <- fread("data/stem_edu/working/hard_soft_skills/for_validation/skillclusterfamily_classified_together.csv")
skillclusterfamily_verify <- skillclusterfamily[,c("Alyssa","Calvin","Sarah")]

kappam.fleiss(skillclusterfamily_verify)

unique(skillclusterfamily[,10])

cautiousFamily <- which(skillclusterfamily[,10] == "More cautious than original")
additionalClusterFamily <- skillclusterfamily[cautiousFamily,"skillclusterfamily"]

cautiousCluster <- which(skillcluster[,9] == "More cautious")
additionalCluster <- skillcluster[cautiousCluster,"skillcluster"]

####going withe THE most cautious rating at the cluster family/cluster level; will need to come to agreement
####at skill level. So what I need to do is, pull out the skill clusters from the cluster families that
####need a second look, get feedback on those, then pull out the skills from those + any original clusters
####that wanted a second look. (It might be a lot! We'll see! Hopefully it will be more reasonable than
####TEN THOUSAND categorizations.)
