
library(tidyverse)
library(data.table)
library(readxl)
library(reshape2)

setwd("/home/af5ug/stem_edu")

knowledge <- read_excel("data/stem_edu/original/ONET_survey/Knowledge.xlsx")
education <- read_excel("data/stem_edu/original/ONET_survey/Education, Training, and Experience.xlsx")
titles <- read_excel("data/stem_edu/original/ONET_survey/Occupation Data.xlsx")
edu_cat <- read_excel("data/stem_edu/original/ONET_survey/Education, Training, and Experience Categories.xlsx")

###Trimming down knowledge data to what we actually need

colnames(knowledge) <- c("soc_code","title","element_id","element_name","scale_id",
                      "scale_name","data_value","n","se","lower_ci","upper_ci",
                      "recommend_suppress", "not_relevant", "date",
                      "domain_source")

knowledge <- filter(knowledge, scale_name == "Level")

all_know_cast <- dcast(knowledge, formula = soc_code + title ~ element_name, value.var = "data_value")

colnames(all_know_cast) <- c("onet","title", colnames(all_know_cast)[3:35])


knowledge <- filter(knowledge, element_name %in%
                      c("Biology", "Building and Construction", "Chemistry",
                        "Computers and Electronics", "Design", "Economics and Accounting",
                        "Engineering and Technology", "Food Production", "Mathematics",
                        "Mechanical", "Medicine and Dentistry", "Physics",
                        "Production and Processing", "Telecommunications"))

####looking at distributions of knowledge scores for each relevant skillset

ggplot(knowledge, aes(x=data_value))+geom_histogram()+facet_grid(cols = vars(element_name))
#Rothwell looked at slightly-above-average score on a 1-7 scale
#(but is average actually at 4?) not at all normally distributed at any level--
#particularly bad for individual knowledge, but also bad for all technical knowledge
#and also all knowledge, even when excluding info that ONET says should be suppressed

ggplot(knowledge)+geom_histogram(aes(x=data_value))

ggplot(knowledge[knowledge$recommend_suppress == "N",])+geom_histogram(aes(x=data_value))
mean(knowledge$data_value, na.rm = TRUE)
median(knowledge$data_value, na.rm = TRUE)

knowledge %>% group_by(element_name) %>% summarise(mean = mean(data_value)) %>% ggplot(aes(x=mean))+geom_histogram(bins = 10)

k_quantile <- knowledge %>% group_by(element_name) %>% summarise(min = quantile(data_value)[1],
              q1 = quantile(data_value)[2], mean = quantile(data_value)[3],
              q3 = quantile(data_value)[4],
              perc90 = quantile(data_value, probs = .9),
              perc95 = quantile(data_value, probs = .95),
              max = quantile(data_value)[5])
#how many occupations have an STW skill in the 95th percentile?


#seeing technical knowledge levels for each occupation

tech_know_cast <- dcast(knowledge, formula = soc_code + title ~ element_name, value.var = "data_value")
colnames(tech_know_cast) <- c("onet","title", colnames(tech_know_cast)[3:16])

##high enough levels of knowledge

knowledge <- filter(knowledge, scale_name == "Level", data_value >= 4.5)

##O*NET codes for occupations that require high levels of technical knowledge

know_occ <- unique(knowledge$soc_code)

####Filtering down to the education data we actually want

colnames(education) <- c("soc_code","title","element_id","element_name","scale_id",
                         "scale_name", "category", "data_value", "n","se","lower_ci",
                         "upper_ci", "recommend_suppress", "date", "domain_source")
#education %>% filter(soc_code == "11-1011.00", scale_id == "RL") %>% group_by(soc_code) %>% summarise(sum = sum(data_value))

education <- filter(education, scale_id == "RL")

edu_cast <- dcast(education, formula = soc_code + title ~ category, value.var = "data_value")

colnames(edu_cast) <- c("onet","onet_name","lessHighSchool","highSchool","postSecCert","someCollege",
                        "associate","bachelor","postBachCert", "master", "postMasterCert","profDegree","phd","postPhDtraining")

edu_cast$sub_bach <- 0

for(i in 1:nrow(edu_cast)){
  edu_cast[i,"sub_bach"] <- sum(edu_cast[i,3:7])
}

edu_occ <- unique(filter(edu_cast, sub_bach >= 50)$onet)
length(edu_occ)
nrow(edu_cast)


###Which occupation codes match both the education and knowledge criteria?

both_occ <- edu_occ[which(edu_occ %in% know_occ)]

onlyedu_occ <- edu_occ[-which(edu_occ %in% know_occ)]

###What are the actual job titles and information for these occupations?

colnames(titles) <- c("soc_code","title","description")

#joining everything together into one table

stw <- titles[which(titles$soc_code %in% both_occ),]
colnames(stw) <- c("onet","onet_name","description")
stw <- left_join(stw, edu_cast[,c(1,15,3:14)], by = "onet")
head(stw)
nrow(stw)

onlyedu_detail <- titles[which(titles$soc_code %in% onlyedu_occ),]
colnames(onlyedu_detail) <- c("onet","onet_name","description")
onlyedu_detail <- left_join(onlyedu_detail, edu_cast[,c(1,15)], by = "onet")
onlyedu_detail <- left_join(onlyedu_detail, all_know_cast[,c(1,3:35)], by = "onet")

#checking up on some of the STW definitions
#artists: which of their skills are related here?
knowledge[which(knowledge$soc_code == "27-1013.00"),]
#education of accountants:
edu_cast[edu_cast$soc_code == "13-2011.01",]

##what are the skills (in the BGT data) that go with Rothwell-defined STW occupations?
bgt_skills <- readRDS("~/stem_edu/data/stem_edu/working/burning_glass/ads_skills_2017_51.RDS")
bgt_jobs <- readRDS("~/stem_edu/data/stem_edu/working/burning_glass/ads_main_2017_51.RDS")

bgt_stw_id <- bgt_jobs[bgt_jobs$onet %chin% stw$onet,"bgtjobid"]

bgt_stw_skills <- bgt_skills[bgt_skills$bgtjobid %chin% bgt_stw_id]

bgt_stw_skill_rank <- bgt_stw_skills %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count))

bgt_stw_skills %>% group_by(skill) %>% summarise(count = n()) %>% arrange(desc(count)) %>% ggplot(aes(x=count))+geom_histogram(bins = 75)

bgt_stw_skill_rank_many <- bgt_stw_skills %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(count = n()) %>% filter(count >=100) %>% arrange(desc(count))

###how common are each of the STW occupations across all of Virginia?
v_job_stw <- bgt_jobs[bgt_jobs$onet %chin% stw$onet,]
nrow(v_job_stw)/nrow(bgt_jobs)
v_stw_perc_total <- v_job_stw %>% group_by(onet, onetname) %>% summarise(count = n(),
                      perc_total = count/nrow(bgt_jobs), perc_stw = count/nrow(v_job_stw))
stw_perc_total <- left_join(r_job_edu[,c("onet","onetname","total")], v_stw_perc_total[,c("onet","count","perc_total","perc_stw")],
                            by = "onet")
colnames(stw_perc_total) <- c("onet","onetname","r_total","v_total","v_perc_all","v_perc_stw")
stw_perc_total <- left_join(stw_perc_total, b_job_edu[,c("onet","total")], by = "onet")
colnames(stw_perc_total) <- c("onet","onetname","r_total","v_total","v_perc_all","v_perc_stw","b_total")
stw_perc_total <- select(stw_perc_total, "onet","onetname","r_total","b_total","v_total","v_perc_all","v_perc_stw")
stw_perc_total$r_perc_all <- stw_perc_total$r_total/nrow(r_job)
stw_perc_total$r_perc_stw <- stw_perc_total$r_total/nrow(r_job_stw)
stw_perc_total$b_perc_all <- stw_perc_total$b_total/nrow(b_job)
stw_perc_total$b_perc_stw <- stw_perc_total$b_total/nrow(b_job_stw)

stw_perc_total[,6:11] <- round(stw_perc_total[,6:11], 4)

stw_perc_total$r_stw_diff <- stw_perc_total$r_perc_stw-stw_perc_total$v_perc_stw
stw_perc_total$b_stw_diff <- stw_perc_total$b_perc_stw-stw_perc_total$v_perc_stw
stw_perc_total$rb_stw_diff <- stw_perc_total$r_perc_stw-stw_perc_total$b_perc_stw

stw_perc_total_many <- stw_perc_total[stw_perc_total$v_perc_stw >= .005,]

stw_perc_total_many <- arrange(stw_perc_total_many, desc(v_perc_stw))
ggplot(stw_perc_total_many)+geom_point(aes(x=onetname, y=v_perc_stw))+coord_flip()

ggplot(stw_perc_total_many)+geom_point(aes(x=reorder(onetname, v_total), y=v_perc_stw), size = 2)+
  geom_point(aes(x=reorder(onetname, v_total), y=r_perc_stw), color = "blue", size = 1)+
  geom_point(aes(x=reorder(onetname, v_total), y= b_perc_stw), color = "red", size = 1)+
  coord_flip()+ggtitle("Occupations As Percent of STW Workforce")

###Finding how many jobs in Richmond and Blacksburg match this definition

b_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/blacksburg_jobs.csv")
r_job <- fread("data/stem_edu/working/Team_SA_job_skills_filter/rich_jobs.csv")

b_job_stw <- b_job[onet %chin% stw$onet]
r_job_stw <- r_job[onet %chin% stw$onet]

##some basic information about each

#number of STW positions
nrow(b_job_stw)
nrow(r_job_stw)


#STW positions as percentage of all positions
nrow(b_job_stw)/nrow(b_job)
nrow(r_job_stw)/nrow(r_job)

#number of unique STW occupations
length(unique(b_job_stw$onet))
length(unique(r_job_stw$onet))

#STW occupations as percentage of all occupations
length(unique(b_job_stw$onet))/length(unique(b_job$onet))
length(unique(r_job_stw$onet))/length(unique(r_job$onet))

####writing out Rothwell definitions
write.csv(stw, "src/ONET_define/Rothwell_STW_list.csv")

###writing out Blacksburg/Richmond Rothwell job ads
#write.csv(r_job_stw, "data/stem_edu/working/Team_SA_job_skills_filter/rothwell_richmond_stw_job.csv")
#write.csv(b_job_stw, "data/stem_edu/working/Team_SA_job_skills_filter/rothwell_blacksburg_stw_job.csv")

##how many ads for these occupations require bachelor's degrees?
r_job_edu <- r_job_stw %>% group_by(degree,onetname,onet) %>% summarise(counts = n())
r_job_edu <- dcast(r_job_edu, formula = onetname + onet ~ degree)
colnames(r_job_edu) <- c("onetname","onet","associate","bachelor","highschool","master","phd","NA")
r_job_edu <- select(r_job_edu, "onetname", "onet", "NA", "highschool", "associate","bachelor","master","phd")

r_job_edu$subBach <- apply(r_job_edu[,c('highschool', 'associate','NA')], 1, function(x) sum(x, na.rm = TRUE))
r_job_edu$total <- apply(r_job_edu[,3:8], 1, function(x) sum(x, na.rm = TRUE))
r_job_edu$subBachPerc <- round(r_job_edu$subBach/r_job_edu$total,2)
r_job_edu_many <- filter(r_job_edu, total >= 10)

b_job_edu <- b_job_stw %>% group_by(degree,onetname,onet) %>% summarise(counts = n())
b_job_edu <- dcast(b_job_edu, formula = onetname + onet ~ degree)
colnames(b_job_edu) <- c("onetname","onet","associate","bachelor","highschool","master","phd","NA")
b_job_edu <- select(b_job_edu, "onetname", "onet","NA", "highschool", "associate","bachelor","master","phd")

b_job_edu$subBach <- apply(b_job_edu[,c('highschool', 'associate','NA')], 1, function(x) sum(x, na.rm = TRUE))
b_job_edu$total <- apply(b_job_edu[,3:8], 1, function(x) sum(x, na.rm = TRUE))
b_job_edu$subBachPerc <- round(b_job_edu$subBach/b_job_edu$total,2)
b_job_edu_many <- filter(b_job_edu, total >= 10)

#combining to compare % sub bach from O*NET to MSA:
r_job_edu_many <- left_join(r_job_edu_many, edu_cast[,c("onet","sub_bach")], by = c("onet" = "onet"))
excol <- colnames(r_job_edu_many)
colnames(r_job_edu_many) <- c(excol[1:11],"ONETsubBachPerc")
r_job_edu_many$ONETsubBachPerc <- round(r_job_edu_many$ONETsubBachPerc/100,2)
r_job_edu_many$subBachPercDiff <- r_job_edu_many$ONETsubBachPerc - r_job_edu_many$subBachPerc

ggplot(r_job_edu_many, aes(x = reorder(onetname, subBachPercDiff), y = subBachPercDiff)) + geom_col() + coord_flip()


b_job_edu_many <- left_join(b_job_edu_many, edu_cast[,c("onet","sub_bach")], by = c("onet" = "onet"))
excol <- colnames(b_job_edu_many)
colnames(b_job_edu_many) <- c(excol[1:11],"ONETsubBachPerc")
b_job_edu_many$ONETsubBachPerc <- round(b_job_edu_many$ONETsubBachPerc/100,2)
b_job_edu_many$subBachPercDiff <- b_job_edu_many$ONETsubBachPerc - b_job_edu_many$subBachPerc

ggplot(b_job_edu_many, aes(x = reorder(onetname, subBachPercDiff), y = subBachPercDiff)) + geom_col() + coord_flip()

comb_job_edu_many <- rbind(cbind(r_job_edu_many,place = "richmond"), cbind(b_job_edu_many, place = "blacksburg"))

ggplot(comb_job_edu_many, aes(x = reorder(onetname, subBachPercDiff), y = subBachPercDiff)) + geom_col(aes(group = place, fill = place), position = "dodge") + coord_flip()

#####getting list of all skills associated with an STW
bgt_job_unique <- bgt_jobs %>% group_by(bgtjobid, onet, onetname) %>% summarise(count = n())

bgt_stw_skills_onet <- left_join(bgt_stw_skills, bgt_jobs[,c("bgtjobid", "onet", "onetname")], by = "bgtjobid")
bgt_stw_count <- bgt_jobs %>% filter(bgtjobid %in% bgt_stw_id) %>% group_by(onet, onetname) %>% summarise(count = n())
bgt_stw_skills_onet <- left_join(bgt_stw_skills_onet, bgt_stw_count[,c("onet","count")], by = "onet")
colnames(bgt_stw_skills_onet) <- c(colnames(bgt_stw_skills_onet)[1:11],"total_positions")
bgt_stw_skills_onet_unique <- bgt_stw_skills_onet %>% group_by(skill, skillcluster, skillclusterfamily, isspecialized, isbaseline,
                              issoftware, onet, onetname, total_positions) %>% summarise(positions_with_skill = n())
bgt_stw_skills_onet_unique$perc_pos_w_skill <- round(bgt_stw_skills_onet_unique$positions_with_skill / bgt_stw_skills_onet_unique$total_positions, 4)*100
bgt_stw_skills_onet_unique <- select(bgt_stw_skills_onet_unique, "onet","onetname","skill","skillcluster","skillclusterfamily",
                                     "isspecialized", "isbaseline", "issoftware", "total_positions", "positions_with_skill",
                                     "perc_pos_w_skill") %>% arrange(desc(total_positions), desc(perc_pos_w_skill))
write.csv(bgt_stw_skills_onet_unique, "src/ONET_define/v_stw_skills.csv")
