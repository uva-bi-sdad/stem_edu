library(readxl)
library(tidyverse)
library(data.table)

###Rothwell definition knowledge levels


knowledge <- read_xlsx("data/stem_edu/original/ONET_survey/Knowledge.xlsx")
colnames(knowledge) <- c("onet", "title", "elementID", "elementName", "scaleID", "scaleName",
                         "dataValue", "N", "se", "lowerCI", "upperCI", "recommendSuppress",
                         "notRelevant", "date", "domainSource")

knowledge_level <- filter(knowledge, scaleName == "Level")

length(unique(knowledge$elementName))

knowledge_level$elementName <- factor(knowledge_level$elementName, levels = c("Biology", "Building and Construction",
                          "Chemistry", "Computers and Electronics", "Design", "Economics and Accounting",
                        "Engineering and Technology", "Food Production", "Mathematics",
                        "Mechanical", "Medicine and Dentistry", "Physics",
                        "Production and Processing", "Telecommunications", unique(knowledge_level$elementName)[!stw_element]))

stw_knowledge <- c("Biology", "Building and Construction",
                   "Chemistry", "Computers and Electronics", "Design", "Economics and Accounting",
                   "Engineering and Technology", "Food Production", "Mathematics",
                   "Mechanical", "Medicine and Dentistry", "Physics",
                   "Production and Processing", "Telecommunications")


non_stw_knowledge <- unique(knowledge_level$elementName[!(knowledge_level$elementName %in% stw_knowledge)])

knowledge_level_1 <- knowledge_level %>% filter(elementName %in% stw_knowledge, dataValue >= 4.5) %>% mutate(group = 1)
knowledge_level_2 <- knowledge_level %>% filter(elementName %in% stw_knowledge, dataValue < 4.5) %>% mutate(group = 2)
knowledge_level_3 <- knowledge_level %>% filter(elementName %in% non_stw_knowledge) %>% mutate(group = 3)

knowledge_level_g <- rbind(knowledge_level_1, knowledge_level_2, knowledge_level_3)
knowledge_level_g$group <- factor(knowledge_level_g$group)


ggplot(knowledge_level_g) + geom_jitter(aes(x = reorder(elementName,
  desc(elementName)), y = dataValue, color = group), alpha = .2, stroke = 0, width = .25) +
  geom_hline(yintercept = 4.5, color = "red") + coord_flip() + theme_minimal() +
  scale_color_manual(values = c("#eb6123","#ffe4b2","gray")) +
  ggtitle("O*NET Knowledge Levels Across Occupations") + ylab("Rating") + xlab("Knowledge Type")+
  theme(legend.position = "none")

stw_knowledge_crit <- subset(knowledge_level_g$onet, knowledge_level_g$group == 1)

###Education by occupation

education <- read_excel("data/stem_edu/original/ONET_survey/Education, Training, and Experience.xlsx")

colnames(education) <- c("soc_code","title","element_id","element_name","scale_id",
                         "scale_name", "category", "data_value", "n","se","lower_ci",
                         "upper_ci", "recommend_suppress", "date", "domain_source")

education <- filter(education, scale_id == "RL")

edu_cast <- dcast(education, formula = soc_code + title ~ category, value.var = "data_value")

colnames(edu_cast) <- c("onet","onet_name","lessHighSchool","highSchool","postSecCert","someCollege",
                        "associate","bachelor","postBachCert", "master", "postMasterCert","profDegree","phd","postPhDtraining")

edu_cast$sub_bach <- 0

for(i in 1:nrow(edu_cast)){
  edu_cast[i,"sub_bach"] <- sum(edu_cast[i,3:7])
}

edu_cast_k <- filter(edu_cast, edu_cast$soc_code %in% stw_knowledge_crit)
edu_cast_k$bach_up <- round(100 - edu_cast_k$sub_bach,0)

edu_cast_k_1 <- edu_cast_k %>% filter(bach_up <= 50) %>% mutate(group = "1")
edu_cast_k_2 <- edu_cast_k %>% filter(bach_up > 50) %>% mutate(group = "2")

edu_cast_k_g <- rbind(edu_cast_k_1, edu_cast_k_2)

ggplot(edu_cast_k_g)+
  geom_dotplot(aes(x=bach_up, fill = group, color = group), binwidth = 1, method = "histodot",
                  dotsize = .5, stackratio = 2) + coord_fixed(ratio = 100)  +
  scale_y_continuous(labels = NULL) + theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ylab("") +
  ggtitle("Workers in Each Occupation with Bachelor's Degree or Above") +
  xlab("% of workers in occupation with bachelor's degree or above") +
  scale_color_manual(values = c("#eb6123","#ffe4b2")) +
  theme(legend.position = "none")


View(edu_cast_k %>% group_by(bach_up) %>% summarise(count = n()) %>% arrange(desc(count)))

nrow(edu_cast_k %>% filter(bach_up < 50))
