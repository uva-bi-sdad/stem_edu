library(RCurl)
library(rvest)
library(XML)


#### Creating the Data Frame
coursera_df <- data.frame(NA, NA, NA, NA, NA, NA, stringsAsFactors = FALSE)
colnames(coursera_df)[1] <- "type"
colnames(coursera_df)[2] <- "title"
colnames(coursera_df)[3] <- "language"
colnames(coursera_df)[4] <- "description"
colnames(coursera_df)[5] <- "skills"
colnames(coursera_df)[6] <- "school"

#### Webscraping for information
#### Type = Software Development

#remember to add Sys.sleep to not get blocked

cate_url <- read_html("https://www.coursera.org/browse/computer-science/software-development?languages=en")
courses <- cate_url %>% html_nodes('href') %>% html_text()

for(i in length(courses))
{
  course_url <- paste0('https://www.coursera.org', courses[i])
}
