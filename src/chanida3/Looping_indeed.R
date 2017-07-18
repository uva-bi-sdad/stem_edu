library(rvest)
library(XML)
library(stringr)
library(RCurl)

hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start="))

href <- hrefExtractor %>%
  html_nodes(".app_link") %>% html_attr("href")

href
resumeURL <- read_html(paste0("https://www.indeed.com", href[1]))
resumeURL

work_des <- resumeURL %>%
  html_nodes("#work-experience-items") %>% html_text()
work_des

Education <- resumeURL %>%
  html_nodes("#education-items") %>% html_text()
Education

skills <- resumeURL %>%
  html_nodes(".skill-text") %>% html_text()
skills

certifications <- resumeURL %>%
  html_nodes("#certification-items") %>% html_text()
certifications

#start of looping code
education_df <- data.frame(stringsAsFactors = FALSE)

#adding columns
education_df[1,1] <- NA
education_df[1,2] <- NA
education_df[1,3] <- NA
education_df[1,4] <- NA

#change column names
names(education_df)[1] <- paste("education_description")
names(education_df)[2] <- paste("skills")
names(education_df)[3] <- paste("Work_description")
names(education_df)[4] <- paste("Certifications/Licenses")

#rerun this when starting over completely
pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:1)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start=", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:1)
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))

    if(length(resumeURL %>% html_nodes("#education-items") %>% html_text()) != 0)
    {
      education_df[r,1] <- resumeURL %>% html_nodes("#education-items") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes("#skills-items .data_display ") %>% html_text()) != 0)
    {
      education_df[r,2] <- resumeURL %>% html_nodes("#skills-items .data_display ") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes("#work-experience-items") %>% html_text()) != 0)
    {
      education_df[r,3] <- resumeURL %>% html_nodes("#work-experience-items") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes("#certification-items") %>% html_text()) != 0)
    {
      education_df[r,4] <- resumeURL %>% html_nodes("#certification-items") %>% html_text()
    }
    r <- r + 1
  }
  pageNum <- pageNum + 50
}


#string r function to clean (DOES NOT WORK)
# edu_cleaning <- function(row_data)
# {
#   str <- row_data['education_description']
#   stringr::str_split(education_df[1,1], )[[1]][1]
# }
# education_df[1,1] <- apply(education_df, 1, edu_cleaning)


