library(readr)
library(dplyr)
library(ggplot2)

#load coursera keyword count data
coursera_counts <- read.csv('~/git/stem_edu/src/hsbb8/final/new_final_counts.csv')
coursera_counts <- data.frame(coursera_counts[, c(3,5)])
coursera_counts$type <- "Coursera"
names(coursera_counts) <- c('terms', 'percentage', 'type')

#load indeed keyword count data
indeed_counts <- read.csv('~/git/stem_edu/src/hsbb8/final/indeed_data.csv')
indeed_counts <- data.frame(indeed_counts[,2:3])
indeed_counts$type <- "Indeed"
names(indeed_counts) <- c('terms', 'percentage', 'type')


#format data
dat <- rbind(coursera_counts, indeed_counts)


#plotting


#coursera_counts <- order(coursera_counts$percentage, decreasing = TRUE)

#coursera_counts <- subset(coursera_counts, percentage!=0 )
coursera_counts <- coursera_counts[,c(1,3:4)]

names(indeed_counts) <- c("terms", "percentage", "type")
df2 <- rbind(indeed_counts, coursera_counts)



df2 <- left_join(coursera_counts, indeed_counts, by = c("terms" = 'Term'))
df2[is.na(df2)] <- 0

df2$Total <- df2$percentage + df2$NumPostings
df2 <- subset(df2, Total != 0)


p1 <- df2[, c('terms', 'percentage', 'type.x')]
names(p1) <- c('terms', 'percentage', 'type')

p2 <- df2[, c('terms', 'NumPostings', 'type.y')]
names(p2) <- c('terms', 'percentage', 'type')

dat <- rbind(p1, p2)

######

dat[64,3] <- "Indeed"
dat[78,3] <- "Indeed"
dat[84,3] <- "Indeed"


ggplot(dat, aes(x = terms, y = percentage, fill = type)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45))
