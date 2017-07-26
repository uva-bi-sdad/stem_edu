## indeed.com company reviews
library(reshape2)
library(dplyr)
library(ggplot2)

# dental hygienists, Virginia
reviews<-indeed_companies_dental_hygienist

# make long
reviews<-select(reviews,comp_name,job_location,benefit_rating,culture_rating,jsecurity_rating,mgmt_rating,overall_rating,
                wl_bal_rating)
reviews<-melt(reviews,id=c("comp_name","job_location"),na.rm=T,value.name="rating")

reviews$variable <- as.factor(reviews$variable)
levels(reviews$variable)

reviews$variable = factor(reviews$variable,levels(reviews$variable)[c(1:4,6,5)])

reviews$variable <- factor(reviews$variable,
                           labels = c("Compensation/Benefits", "Culture", "Job Security/Advancement", "Management", 
                                      "Work/Life Balance", "Overall"))

fill <- "#4271AE"
line <- "#1F3552"

png("~/git/stem_edu/output/coReviews_dentalHygienist.png",
    height=600,width=800)
plt<-ggplot(reviews, aes(x = variable, y = rating)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_x_discrete(name = "Review Categories",labels = c("Compensation/ \nBenefits","Culture","Job Security/ \nAdvancement","Management","Work/ \nLife Balance","Overall")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 20),
        plot.title=element_text(size = 30,hjust=.5),
        axis.text.y=element_text(size = 20),
        axis.title.y=element_text(size = 20),
        axis.title.x=element_text(size = 20)) + 
  
 
  scale_y_continuous(name = "Rating") +
  ggtitle("Employee Reviews for Companies Hiring Dental Hygienists")
print(plt)
dev.off()
