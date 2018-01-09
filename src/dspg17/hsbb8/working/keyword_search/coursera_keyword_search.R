#keyword searching through the coursera web scraped data
#input:
  #coursera_comp_sci (all of the web scraped data from Coursera.com under the Computer Science category)
  #keywords_list (all of the keywords we are searching for in the coursera data)

#output:
  #coursera_keyword_counts (counts of the keywords for the entire data file of Coursera courses)

##########
#SEARCHING SKILLS AND DESCRIPTION INFO FROM COURSERA DATA
##########

#upload data from web scraping Coursera computer science coursers (what we will be searching in)
cs_coursera <- read.csv('~/git/stem_edu/data/stem_edu/final/webscraping_data/coursera_comp_sci.csv')

#upload terms we will be searching the Coursera courses for
keywords <- read.csv('~/git/stem_edu/data/stem_edu/original/hsbb8/keywords_list.csv')

terms <- as.character(unique(keywords$Search.Term))
desc <- as.character(paste(cs_coursera$description, cs_coursera$skills))

counts2 <- data.frame(terms, 0, stringsAsFactors = FALSE)
colnames(counts2)[2] <- "count"
counts2[41,1] <- " C "

i = 1
j = 1

for (i in 1:118)
{
  for (j in 1:length(desc))
  {
    if (grepl(terms[i], desc[j], fixed = TRUE))
    {
      counts2[i,2] <- counts2[i,2] + 1
    }
    j <- j + 1
  }
  j = 1
  i <- i + 1
}


counts2[41, 2] <- 8
counts2$percentage <- (counts2$count/333) * 100

write.csv(counts2, file = '~/git/stem_edu/data/stem_edu/final/webscraping_data/coursera_keyword_counts.csv')
