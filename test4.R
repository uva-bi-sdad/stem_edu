print("GOOD MORNING")
library(R.utils)

gunzip("100_certification_info.csv.gz")
print(file.info("100_certification_info.csv"))
cert <- read.csv("100_certification_info.csv")
