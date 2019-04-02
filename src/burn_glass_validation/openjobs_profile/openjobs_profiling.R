#script for open jobs data profiling
library(DataExplorer)
library(data.table)

ojobs <- fread('./data/stem_edu/working/allOpenjobsParsed.csv')
introduce(ojobs)
