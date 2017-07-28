library(rvest)
library(XML)
library(stringr)

# STEP 1: =1 can be items 50,100,150 etc, getting url and passing it to a variable
hrefExtractor <- read_html(paste0("https://www.indeed.com/resumes?q=dental+hygienist&l=Arlington%2C+VA&cb=jt&start=1"))

# STEP 2: pipping url to get href; a node on the main page with different resumes
href <- hrefExtractor %>%
  html_nodes(".app_link") %>% html_attr("href")

href
href[35]

# STEP 1: DO this every time for a new page
resumeURL <- read_html(paste0("https://www.indeed.com", href[35]))

# STEP 2 part 2: a node for finding the type of info you want to collect on a resume page
workDescription <- resumeURL %>%
  html_nodes(".work_description") %>% html_text()

workDescription

# STEP 3: then use stringr package to clean it





#LATER
typeof(workDescription)
class(workDescription)

as.array(workDescription)
workDescription <- as.array(workDescription)
workDescription[1]





