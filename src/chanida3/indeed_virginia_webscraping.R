#scrapes indeed.com for job resumes for dental hygienists in Virginia
#output: virginia_res.csv (saved to ~/stem_edu/data/stem_edu/original/chanida3/)

#loading libraries
library(rvest)
library(XML)
library(stringr)
library(RCurl)

#Making a data frame for Virginia Resumes
virginia_res <- data.frame(stringsAsFactors = FALSE)

#Created 1 blank row and 6 blank columns
virginia_res[1,1] <- NA
virginia_res[1,2] <- NA
virginia_res[1,3] <- NA
virginia_res[1,4] <- NA
virginia_res[1,5] <- NA
virginia_res[1,6] <- NA

#change column names to the specific descriptions
names(virginia_res)[1] <- paste("education_description")
names(virginia_res)[2] <- paste("skills")
names(virginia_res)[3] <- paste("Work_description")
names(virginia_res)[4] <- paste("Certifications/Licenses")
names(virginia_res)[5] <- paste("href")
names(virginia_res)[6] <- paste("city")

#rerun this when starting over completely (at the first page)
pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:12)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Virginia&co=US&cb=jt&start=", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:length(href))
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))
    virginia_res[r,5] <- href[b]
    if(length(resumeURL %>% html_nodes(".education-content") %>% html_text()) != 0)
    {
      virginia_res[r,1] <- resumeURL %>% html_nodes(".education-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".skills-content") %>% html_text()) != 0)
    {
      virginia_res[r,2] <- resumeURL %>% html_nodes(".skills-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".workExperience-content") %>% html_text()) != 0)
    {
      virginia_res[r,3] <- resumeURL %>% html_nodes(".workExperience-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".certification-content") %>% html_text()) != 0)
    {
      virginia_res[r,4] <- resumeURL %>% html_nodes(".certification-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes("#headline_location") %>% html_text()) != 0)
    {
      virginia_res[r,6] <- resumeURL %>% html_nodes("#headline_location") %>% html_text()
    }
    r <- r + 1

    Sys.sleep(sample(1:10, 1))
    print(sprintf('done resume %s', b))
  }
  pageNum <- pageNum + 50
  print(sprintf('done page %s', a))
}

write.csv(virginia_res, "~/git/stem_edu/data/stem_edu/original/chanida3/virginia_res.csv")
