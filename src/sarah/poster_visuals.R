library(dplyr)
library(ggplot2)


#####RICHMOND#####
#ad data for nurses
r_ads <- read.csv("data/stem_edu/final/dspg19_analysis/FINAL_RICHMOND_NURSE_AD.csv")
r_res <- read.csv("data/stem_edu/final/dspg19_analysis/FINAL_RICHMOND_NURSE_RESUME.csv")

colnames(r_ads)[colnames(r_ads)=="bgtjobid"] <- "id"
colnames(r_res)[colnames(r_res)=="bgtresid"] <- "id"

#boxplot Richmond Ads
box_r_ads <-  r_ads %>%
  select(id, skill) %>%
  group_by(id,) %>%
  summarize(skill=n_distinct(skill))

#boxplot Richmond
box_r_res <-  r_res %>%
  select(id, skill) %>%
  group_by(id,) %>%
  summarize(skill=n_distinct(skill))

box_r_ads$variable = "Ads"
box_r_res$variable = "Resumes"
box_r_ads$variable <- as.factor(box_r_ads$variable)
box_r_res$variable <- as.factor(box_r_res$variable)

box_r <- rbind(box_r_res, box_r_ads)

ggplot(box_r, aes(x=variable, y=skill, fill= variable, color=variable)) +
  geom_point(color= "black", fill= "#A9D5A5", alpha= .1, stroke = 1, shape = 21,  position = position_jitterdodge(jitter.height = 0.4, jitter.width = 1.5)) +
  geom_boxplot(outlier.shape=NA, color="black", alpha=0) +
  labs(title="Number of Skills Provided on \nResumes and Job Ads in Richmond", x= "", y="Number of Skills")+
  theme_classic()+
  theme(legend.position = "none", plot.title = element_text( size=20, face="bold", hjust=0.5),  axis.title.y = element_text(size = 20), axis.text.x = element_text(size= 20), axis.text.y = element_text(size= 16))+
  scale_y_continuous(breaks= seq(0, 130, 10))+
  expand_limits(y = 130)





#####BLACKSBURG#####
#ad data for nurses
b_ads <- read.csv("data/stem_edu/final/dspg19_analysis/FINAL_BLACKSBURG_NURSE_AD.csv")
b_res <- read.csv("data/stem_edu/final/dspg19_analysis/FINAL_BLACKSBURG_NURSE_RESUME.csv")

colnames(b_ads)[colnames(b_ads)=="bgtjobid"] <- "id"
colnames(b_res)[colnames(b_res)=="bgtresid"] <- "id"

#boxplot Richmond Ads
box_b_ads <-  b_ads %>%
  select(id, skill) %>%
  group_by(id,) %>%
  summarize(skill=n_distinct(skill))

#boxplot Richmond
box_b_res <-  b_res %>%
  select(id, skill) %>%
  group_by(id,) %>%
  summarize(skill=n_distinct(skill))

box_b_ads$variable = "Ads"
box_b_res$variable = "Resumes"
box_b_ads$variable <- as.factor(box_b_ads$variable)
box_b_res$variable <- as.factor(box_b_res$variable)

box_b <- rbind(box_b_res, box_b_ads)
ggplot(box_b, aes(x=variable, y=skill, fill= variable, alpha= .1)) +
  geom_point(color= "#6DD4DB", position = position_jitterdodge(jitter.height = 0.4, jitter.width = 1.5))+
  geom_boxplot(outlier.shape=NA, color="black", alpha=0)+
  labs(title="Number of Skills Provided on \nResumes and Job Ads in Blacksburg", x= "", y="Number of Skills")+
  #scale_color_manual(values=c("#6DD4DB"))+
  theme_classic()+
  theme(legend.position = "none", plot.title = element_text( size=20, face="bold", hjust=0.5),  axis.title.y = element_text(size = 20), axis.text.x = element_text(size= 20), axis.text.y = element_text(size= 16))+
  scale_y_continuous(breaks= seq(0, 130, 10))+
  expand_limits(y = 130)


