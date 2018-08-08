# INPUT
#        "~/stem_edu/data/stem_edu/original/DSPG2018DataDiscovery/EDITED_harvardx_mitx/Year 4 Appendices - Data Tables_UPDATED.csv"
#        "~/stem_edu/data/stem_edu/original/DSPG2018DataDiscovery/EDITED_harvardx_mitx/Year 4 Appendices - Data Tables-2_UPDATED.csv"
#        "~/stem_edu/data/stem_edu/original/DSPG2018DataDiscovery/EDITED_harvardx_mitx/Year 4 Appendices - Data Tables-3_UPDATED.csv"


library(ggplot2)
library(dplyr)
ds1 <- read.csv('~/stem_edu/data/stem_edu/original/DSPG2018DataDiscovery/EDITED_harvardx_mitx/Year 4 Appendices - Data Tables_UPDATED.csv')
ds2 <- read.csv('~/stem_edu/data/stem_edu/original/DSPG2018DataDiscovery/EDITED_harvardx_mitx/Year 4 Appendices - Data Tables-2_UPDATED.csv')
ds3 <- read.csv('~/stem_edu/data/stem_edu/original/DSPG2018DataDiscovery/EDITED_harvardx_mitx/Year 4 Appendices - Data Tables-3_UPDATED.csv')

ds_12 <- cbind(ds1, ds2)

cols <- c("Institution", "Course.Number", "Short.Title1", "Course.Title", "Instructors2", "Course.Launch.Date", "STEM.NON_STEM", "Curricular.Area2", "Year..1.4.", "Offers.Honor.Certificates", "Participants", "Explorers", "Certified", "X..Female", "X..Bachelor.s.", "Median.Age3")

ds_1and2 <- ds_12[,cols]

ds_1and2$percentMale <- 100 - ds_1and2$X..Female

ds_1and2$X..Female <- ds_1and2$X..Female/100

ds_1and2$Participants_Int <- gsub(",", "", ds_1and2$Participants)
ds_1and2$Participants_Int <- as.numeric(ds_1and2$Participants_Int)

ds_1and2$numFemale <- as.numeric(ds_1and2$Participants_Int) * ds_1and2$X..Female

ds_1and2$X..Bachelor.s. <- ds_1and2$X..Bachelor.s./100
ds_1and2$numBachelor <- as.numeric(ds_1and2$Participants_Int) * ds_1and2$X..Bachelor.s.

ds_1and2$Certified <- gsub(",", "", ds_1and2$Certified)
ds_1and2$Certified <- as.numeric(ds_1and2$Certified)
ds_1and2$Explorers <- gsub(",", "", ds_1and2$Explorers)
ds_1and2$Explorers <- as.numeric(ds_1and2$Explorers)

stem_female_agg <- group_by(ds_1and2, STEM.NON_STEM) %>%
  summarize(participants_total = sum(Participants_Int), explored_total = sum(Explorers),
            certified_total = sum(Certified), female_total = sum(numFemale), bachelors_total = sum(numBachelor))

total_ptp <- sum(ds_1and2$Participants_Int)
stem_female_agg$Percent_Participants <- stem_female_agg$participants_total/total_ptp
stem_female_agg$Percent_Explored <- stem_female_agg$explored_total/stem_female_agg$participants_total
stem_female_agg$Percent_Certified <- stem_female_agg$certified_total/stem_female_agg$participants_total
stem_female_agg$Percent_Female <- stem_female_agg$female_total/stem_female_agg$participants_total
stem_female_agg$Percent_Bachelors <- stem_female_agg$bachelors_total/stem_female_agg$participants_total




# number of females
# sum
ggplot(data = ds_1and2) +
  geom_bar(mapping = aes(x = STEM.NON_STEM, fill = X..Female), position = 'dodge')

ggplot(data = ds_1and2) +
  geom_bar(mapping = )

ggplot(data = ds_1and2) +
  geom_point(mapping = aes(x = Participants, y = Certified, color = STEM.NON_STEM))



ds1_sort <- as.data.frame(ds1$Institution)
names(ds1_sort)[1] <- paste("Institution")
ds2_sort <- as.data.frame(ds2$Institution)
names(ds2_sort)[1] <- paste("Institution")
ds3_sort <- as.data.frame(ds3$Category1)
names(ds3_sort)[1] <- paste("Institution")

ds_123 <- as.data.frame(rbind(ds1_sort, ds2_sort))
ds_123 <- as.data.frame(rbind(ds_123, ds3_sort))

ggplot(data = ds_123) +
  geom_bar(mapping = aes(x = Institution))

ggplot(data = ds2) +
  geom_bar(mapping = aes(x = Institution, fill = Curricular.Area2), position = 'dodge')



# ### GEOM_BAR
#
# # Bar plot: x axis is cut, y is frequency count (default)
# ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
#
# # Bar plot: x axis is cut, y is frequency count (default)
# # *** arguments by order
# ggplot(diamonds) + geom_bar(aes(x = cut))
#
# # Bar plot: x axis is cut, fill is clarity
# ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity))
#
# # Bar plot: x axis is cut, y is frequency count (default), position is fill
# ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), position = 'fill')
#
# # Bar plot: x axis is cut, y is frequency count
# ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), position = 'dodge')
