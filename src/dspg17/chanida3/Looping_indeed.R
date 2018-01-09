library(rvest)
library(XML)
library(stringr)
library(RCurl)

# hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start="))
#
# href <- hrefExtractor %>%
#   html_nodes(".app_link") %>% html_attr("href")
#
# href
# resumeURL <- read_html(paste0("https://www.indeed.com", href[1]))
# resumeURL
#
# work_des <- resumeURL %>%
#   html_nodes("#work-experience-items") %>% html_text()
# work_des
#
# Education <- resumeURL %>%
#   html_nodes("#education-items") %>% html_text()
# Education
#
# skills <- resumeURL %>%
#   html_nodes(".skill-text") %>% html_text()
# skills
#
# certifications <- resumeURL %>%
#   html_nodes("#certification-items") %>% html_text()
# certifications

#start of looping code
education_df <- data.frame(stringsAsFactors = FALSE)

#adding columns
education_df[1,1] <- NA
education_df[1,2] <- NA
education_df[1,3] <- NA
education_df[1,4] <- NA
education_df[1,5] <- NA
education_df[1,6] <- NA

#change column names
names(education_df)[1] <- paste("education_description")
names(education_df)[2] <- paste("skills")
names(education_df)[3] <- paste("Work_description")
names(education_df)[4] <- paste("Certifications/Licenses")
names(education_df)[5] <- paste("href")
names(education_df)[6] <- paste("city")

#rerun this when starting over completely
pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:1)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Virginia&co=US&cb=jt&start=0", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:length(href))
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))
    education_df[r,5] <- href[b]
    if(length(resumeURL %>% html_nodes(".education-content") %>% html_text()) != 0)
    {
      education_df[r,1] <- resumeURL %>% html_nodes(".education-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".skills-content") %>% html_text()) != 0)
    {
      education_df[r,2] <- resumeURL %>% html_nodes(".skills-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".workExperience-content") %>% html_text()) != 0)
    {
      education_df[r,3] <- resumeURL %>% html_nodes(".workExperience-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".certification-content") %>% html_text()) != 0)
    {
      education_df[r,4] <- resumeURL %>% html_nodes(".certification-content") %>% html_text()
    }
    r <- r + 1
    #Sys.sleep(time = runif(1) * 3)
    Sys.sleep(sample(1:10, 1))
    print(sprintf('done resume %s', b))
  }
  pageNum <- pageNum + 50
  print(sprintf('done page %s', a))
}


#####NEW TABLE FOR SECOND PAGE
education_df2 <- data.frame(stringsAsFactors = FALSE)

#adding columns
education_df2[1,1] <- NA
education_df2[1,2] <- NA
education_df2[1,3] <- NA
education_df2[1,4] <- NA
education_df2[1,5] <- NA

#change column names
names(education_df2)[1] <- paste("education_description")
names(education_df2)[2] <- paste("skills")
names(education_df2)[3] <- paste("Work_description")
names(education_df2)[4] <- paste("Certifications/Licenses")
names(education_df2)[5] <- paste("href")

#rerun this when starting over completely
pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:1)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start=", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:50)
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))
    education_df2[r,5] <- href[b]
    if(length(resumeURL %>% html_nodes(".education-content") %>% html_text()) != 0)
    {
      education_df2[r,1] <- resumeURL %>% html_nodes(".education-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".skills-content") %>% html_text()) != 0)
    {
      education_df2[r,2] <- resumeURL %>% html_nodes(".skills-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".workExperience-content") %>% html_text()) != 0)
    {
      education_df2[r,3] <- resumeURL %>% html_nodes(".workExperience-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".certification-content") %>% html_text()) != 0)
    {
      education_df2[r,4] <- resumeURL %>% html_nodes(".certification-content") %>% html_text()
    }
    r <- r + 1
    #Sys.sleep(time = runif(1) * 3)
    Sys.sleep(sample(1:10, 1))
    print(sprintf('done resume %s', b))
  }
  pageNum <- pageNum + 50
  print(sprintf('done page %s', a))
}


########PAGE 3
education_df3 <- data.frame(stringsAsFactors = FALSE)

#adding columns
education_df3[1,1] <- NA
education_df3[1,2] <- NA
education_df3[1,3] <- NA
education_df3[1,4] <- NA
education_df3[1,5] <- NA

#change column names
names(education_df3)[1] <- paste("education_description")
names(education_df3)[2] <- paste("skills")
names(education_df3)[3] <- paste("Work_description")
names(education_df3)[4] <- paste("Certifications/Licenses")
names(education_df3)[5] <- paste("href")

#rerun this when starting over completely
pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:1)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start=", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:50)
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))
    education_df3[r,5] <- href[b]
    if(length(resumeURL %>% html_nodes(".education-content") %>% html_text()) != 0)
    {
      education_df3[r,1] <- resumeURL %>% html_nodes(".education-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".skills-content") %>% html_text()) != 0)
    {
      education_df3[r,2] <- resumeURL %>% html_nodes(".skills-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".workExperience-content") %>% html_text()) != 0)
    {
      education_df3[r,3] <- resumeURL %>% html_nodes(".workExperience-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".certification-content") %>% html_text()) != 0)
    {
      education_df3[r,4] <- resumeURL %>% html_nodes(".certification-content") %>% html_text()
    }
    r <- r + 1
    #Sys.sleep(time = runif(1) * 3)
    Sys.sleep(sample(1:10, 1))
    print(sprintf('done resume %s', b))
  }
  pageNum <- pageNum + 50
  print(sprintf('done page %s', a))
}



###### PAGE 4
education_df4 <- data.frame(stringsAsFactors = FALSE)

#adding columns
education_df4[1,1] <- NA
education_df4[1,2] <- NA
education_df4[1,3] <- NA
education_df4[1,4] <- NA
education_df4[1,5] <- NA

#change column names
names(education_df4)[1] <- paste("education_description")
names(education_df4)[2] <- paste("skills")
names(education_df4)[3] <- paste("Work_description")
names(education_df4)[4] <- paste("Certifications/Licenses")
names(education_df4)[5] <- paste("href")

#rerun this when starting over completely
#pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:1)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start=", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:50)
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))
    education_df4[r,5] <- href[b]
    if(length(resumeURL %>% html_nodes(".education-content") %>% html_text()) != 0)
    {
      education_df4[r,1] <- resumeURL %>% html_nodes(".education-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".skills-content") %>% html_text()) != 0)
    {
      education_df4[r,2] <- resumeURL %>% html_nodes(".skills-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".workExperience-content") %>% html_text()) != 0)
    {
      education_df4[r,3] <- resumeURL %>% html_nodes(".workExperience-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".certification-content") %>% html_text()) != 0)
    {
      education_df4[r,4] <- resumeURL %>% html_nodes(".certification-content") %>% html_text()
    }
    r <- r + 1
    #Sys.sleep(time = runif(1) * 3)
    Sys.sleep(sample(1:10, 1))
    print(sprintf('done resume %s', b))
  }
  pageNum <- pageNum + 50
  print(sprintf('done page %s', a))
}

#######page 5
education_df5 <- data.frame(stringsAsFactors = FALSE)

#adding columns
education_df5[1,1] <- NA
education_df5[1,2] <- NA
education_df5[1,3] <- NA
education_df5[1,4] <- NA
education_df5[1,5] <- NA

#change column names
names(education_df5)[1] <- paste("education_description")
names(education_df5)[2] <- paste("skills")
names(education_df5)[3] <- paste("Work_description")
names(education_df5)[4] <- paste("Certifications/Licenses")
names(education_df5)[5] <- paste("href")

#rerun this when starting over completely
pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:1)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start=", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:50)
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))
    education_df5[r,5] <- href[b]
    if(length(resumeURL %>% html_nodes(".education-content") %>% html_text()) != 0)
    {
      education_df5[r,1] <- resumeURL %>% html_nodes(".education-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".skills-content") %>% html_text()) != 0)
    {
      education_df5[r,2] <- resumeURL %>% html_nodes(".skills-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".workExperience-content") %>% html_text()) != 0)
    {
      education_df5[r,3] <- resumeURL %>% html_nodes(".workExperience-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".certification-content") %>% html_text()) != 0)
    {
      education_df5[r,4] <- resumeURL %>% html_nodes(".certification-content") %>% html_text()
    }
    r <- r + 1
    #Sys.sleep(time = runif(1) * 3)
    Sys.sleep(sample(1:10, 1))
    print(sprintf('done resume %s', b))
  }
  pageNum <- pageNum + 50
  print(sprintf('done page %s', a))
}

######Page 6
education_df6 <- data.frame(stringsAsFactors = FALSE)

#adding columns
education_df6[1,1] <- NA
education_df6[1,2] <- NA
education_df6[1,3] <- NA
education_df6[1,4] <- NA
education_df6[1,5] <- NA

#change column names
names(education_df6)[1] <- paste("education_description")
names(education_df6)[2] <- paste("skills")
names(education_df6)[3] <- paste("Work_description")
names(education_df6)[4] <- paste("Certifications/Licenses")
names(education_df6)[5] <- paste("href")

#rerun this when starting over completely
pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:1)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start=", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:50)
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))
    education_df6[r,5] <- href[b]
    if(length(resumeURL %>% html_nodes(".education-content") %>% html_text()) != 0)
    {
      education_df6[r,1] <- resumeURL %>% html_nodes(".education-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".skills-content") %>% html_text()) != 0)
    {
      education_df6[r,2] <- resumeURL %>% html_nodes(".skills-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".workExperience-content") %>% html_text()) != 0)
    {
      education_df6[r,3] <- resumeURL %>% html_nodes(".workExperience-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".certification-content") %>% html_text()) != 0)
    {
      education_df6[r,4] <- resumeURL %>% html_nodes(".certification-content") %>% html_text()
    }
    r <- r + 1
    #Sys.sleep(time = runif(1) * 3)
    Sys.sleep(sample(1:10, 1))
    print(sprintf('done resume %s', b))
  }
  pageNum <- pageNum + 50
  print(sprintf('done page %s', a))
}

######Page 7

education_df7 <- data.frame(stringsAsFactors = FALSE)

#adding columns
education_df7[1,1] <- NA
education_df7[1,2] <- NA
education_df7[1,3] <- NA
education_df7[1,4] <- NA
education_df7[1,5] <- NA

#change column names
names(education_df7)[1] <- paste("education_description")
names(education_df7)[2] <- paste("skills")
names(education_df7)[3] <- paste("Work_description")
names(education_df7)[4] <- paste("Certifications/Licenses")
names(education_df7)[5] <- paste("href")

#rerun this when starting over completely
pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:1)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start=", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:50)
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))
    education_df[r,5] <- href[b]
    if(length(resumeURL %>% html_nodes(".education-content") %>% html_text()) != 0)
    {
      education_df7[r,1] <- resumeURL %>% html_nodes(".education-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".skills-content") %>% html_text()) != 0)
    {
      education_df7[r,2] <- resumeURL %>% html_nodes(".skills-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".workExperience-content") %>% html_text()) != 0)
    {
      education_df7[r,3] <- resumeURL %>% html_nodes(".workExperience-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".certification-content") %>% html_text()) != 0)
    {
      education_df7[r,4] <- resumeURL %>% html_nodes(".certification-content") %>% html_text()
    }
    r <- r + 1
    #Sys.sleep(time = runif(1) * 3)
    Sys.sleep(sample(1:10, 1))
    print(sprintf('done resume %s', b))
  }
  pageNum <- pageNum + 50
  print(sprintf('done page %s', a))
}

######page 8

education_df8 <- data.frame(stringsAsFactors = FALSE)

#adding columns
education_df8[1,1] <- NA
education_df8[1,2] <- NA
education_df8[1,3] <- NA
education_df8[1,4] <- NA
education_df8[1,5] <- NA

#change column names
names(education_df8)[1] <- paste("education_description")
names(education_df8)[2] <- paste("skills")
names(education_df8)[3] <- paste("Work_description")
names(education_df8)[4] <- paste("Certifications/Licenses")
names(education_df8)[5] <- paste("href")

#rerun this when starting over completely
#pageNum = 0
#r is the position of the row you are in (in the data frame)
r = 1

for(a in 1:1)
{
  hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start=", pageNum))
  href <- hrefExtractor %>% html_nodes(".app_link") %>% html_attr("href")

  for(b in 1:50)
  {
    resumeURL <- read_html(paste0("https://www.indeed.com", href[b]))
    education_df8[r,5] <- href[b]
    if(length(resumeURL %>% html_nodes(".education-content") %>% html_text()) != 0)
    {
      education_df8[r,1] <- resumeURL %>% html_nodes(".education-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".skills-content") %>% html_text()) != 0)
    {
      education_df8[r,2] <- resumeURL %>% html_nodes(".skills-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".workExperience-content") %>% html_text()) != 0)
    {
      education_df8[r,3] <- resumeURL %>% html_nodes(".workExperience-content") %>% html_text()
    }
    if(length(resumeURL %>% html_nodes(".certification-content") %>% html_text()) != 0)
    {
      education_df8[r,4] <- resumeURL %>% html_nodes(".certification-content") %>% html_text()
    }
    r <- r + 1
    #Sys.sleep(time = runif(1) * 3)
    Sys.sleep(sample(1:10, 1))
    print(sprintf('done resume %s', b))
  }
  pageNum <- pageNum + 50
  print(sprintf('done page %s', a))
}

#string r function to clean (DOES NOT WORK)
# edu_cleaning <- function(row_data)
# {
#   str <- row_data['education_description']
#   stringr::str_split(education_df[1,1], )[[1]][1]
# }
# education_df[1,1] <- apply(education_df, 1, edu_cleaning)


