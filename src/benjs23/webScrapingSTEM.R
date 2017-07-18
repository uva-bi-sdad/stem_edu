library(rvest)
library(XML)
library(stringr)
library(rio)
IPEDSdata <- import("~/git/stem_edu/data/stem_edu/original/usa_00002.dat")
data = read.csv(file="~/git/stem_edu/data/stem_edu/original/IPEDS_data_exploration_table.csv", sep=",", header=T)
write.table(data, file ="~/git/stem_edu/data/stem_edu/original/IPEDS_data_exploration_table")




#Step 1 Get URL
hrefExtractor<-read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&co=US&cb=jt&start=1"))


#Step 2
href <- hrefExtractor %>%
  html_nodes(".app_link") %>% html_attr("href")
href[35]

#step 3 -- href string was formatted in a friendly way (no cleaning necessary)


#Step 1
resumeURL <- read_html(paste0("https://www.indeed.com", href[35]))

#Step 2
workDescription <- resumeURL %>%
  html_nodes(".work_description") %>% html_text()

print(workDescription)

#Step 3 --- ???


workDescription <- as.array(workDescription)

workDescription[1]
