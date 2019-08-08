library(data.table)
library(tidyverse)
library(ggrepel)

loc <- "data/stem_edu/final/dspg19_analysis"

#######################################################
################## LOADING DATA FILES #################
#### please customize for your analysis by putting ####
####### the right MSA name in the 4 file paths. #######
#######################################################

#ad main file (already filtered to specific MSA)
ad_main_base <- fread(file.path(loc, "blacksburg_top5_stw_jobs_main.csv"), drop = "V1")

#ad skill file (already filtered to specific MSA)
ad_skill_base <- fread(file.path(loc, "blacksburg_top5_stw_jobs_all_skills.csv"), drop = "V1")

#resume main file (already filtered to specific MSA, with bachelor's/not-bachelor's flag)
res_main_base <- fread(file.path(loc, "resume_with_bachelors_b_main.csv"), drop = "V1")
res_main_base <- select(res_main_base, colnames(res_main_base)[colnames(res_main_base) != "V1"])
colnames(res_main_base) <- c("bgtresid", "statename", "cityname", "msa", "zipcode", "gender", "noofschooldegrees",
                             "noofcertifications", "noofjobs", "bachelor")

#resume skill file (already filtered to specific MSA, with bachelor's/not-bachelor's flag)
res_skill_base <- fread(file.path(loc, "resume_with_bachelors_b_skill.csv"), drop = "V1")
res_skill_base <- select(res_skill_base, colnames(res_skill_base)[colnames(res_skill_base) != "V1"])
colnames(res_skill_base) <- c("bgtresid", "skillid", "skill", "skillcluster", "skillclusterfamily",
                              "isbaseline", "issoftware", "isspecialized", "bachelor")

#list of near-duplicate skills (note: "word processing" and "spreadsheets" are in the skill cluster for Microsoft
#Office products, making them essentially duplicates of Word and Excel rather than overall types of software)
dupe_list <- as.data.frame(rbind(c("Critical Care", "Critical Care Nursing"),
                                 c("Neonatal Intensive Care Unit", "Neonatal Intensive Care Unit (NICU)"),
                                 c("Neonatal Intensive Care", "Neonatal Intensive Care Unit (NICU)"),
                                 c("Surgery", "Surgical Services"),
                                 c("Environmental Laws and Regulations", "Environmental Regulations"),
                                 c("PC Installation", "Computer Installation and Setup"),
                                 c("Preventive Maintenance", "Predictive / Preventative Maintenance"),
                                 c("ADLs Assistance", "Activities of Daily Living (ADLS)"),
                                 c("Hipaa Compliance", "Health Insurance Portability and Accountability Act (HIPAA)"),
                                 c("Bathing", "Patient Bathing"),
                                 c("Statistical Analysis", "Statistics"),
                                 c("Spreadsheets", "Microsoft Excel"),
                                 c("Word Processing", "Microsoft Word")),
                           stringsAsFactors = FALSE)


#######################################################
################ PREPPING AD SKILL FILE ###############
###### customize to choose specific ONET code #########
#######################################################

###Filtering to only ads of interest (occupation name, requires bachelor +/does not require bachelor/all)
###ADJUST THIS CODE TO MEET YOUR PARTICULAR ANALYSIS

ad_main <- filter(ad_main_base, onetname == "Critical Care Nurses", edu <= 14 | is.na(edu) == TRUE)

###Filtering to only skills of interest (hard/soft/all)

ad_skill_hs <- filter(ad_skill_base, hard_soft %in% c("hard", "soft"))

###Finding skills related to the ads we want

ad_skill <- filter(ad_skill_hs, bgtjobid %in% unique(ad_main$bgtjobid))

###Cleaning (removing near-duplicates)

skill_clean <- function(dupe_list, skill_file){
  for(i in 1:nrow(dupe_list)){
    skill_file[skill_file$skill == dupe_list[i,1],"skill"] <- dupe_list[i,2]
  }
  skill_file <- filter(skill_file, skill != "na")
  skill_file
}

ad_skill <- skill_clean(dupe_list, ad_skill)

###Summary of skills for job (only including skills that appear in at least 3% of job ads)
ad_skill_sum <- ad_skill %>% group_by(skillclusterfamily, skillcluster, skill, hard_soft) %>%
  summarise(count = length(unique(bgtjobid)), perc = length(unique(bgtjobid))/length(unique(ad_skill$bgtjobid))) %>% arrange(desc(count))

skill_for_eval <- filter(ad_skill_sum, perc >= .03)

ad_eval <- filter(ad_skill, skill %in% skill_for_eval$skill)

###Making contingency table

ad_contingency <- as.data.frame.matrix(table(ad_eval[,c("bgtjobid", "skill")]))
colnames(ad_contingency) <- make.names(colnames(ad_contingency))
ad_contingency[ad_contingency > 1] <- 1

#######################################################
################ PREPPING RESUME SKILL FILE ###########
##### please customize the education level of the #####
########## candidates you are intereseted in ##########
#######################################################

###Filtering only to candidates of interest (has bachelor+/does not have bachelor+/all)

res_main_int <- filter(res_main_base, bachelor %in% c(0,1) | is.na(bachelor == TRUE))

###Clean resume skills (rename near-duplicates)

res_skill_base_clean <- skill_clean(dupe_list, res_skill_base)

###Finding the "critical" resume skills that are included in at least 30% of job ads, and getting the unique
###resume IDs.

skill_for_candidate <- filter(ad_skill_sum, perc >= .25, hard_soft == "hard")

################################################
####### MANUAL DATA CHECK NEEDED HERE ##########
################################################

##Look through that skill list--is there anything that is a common skill, but not specific to/sufficient for the job?
##Put in the exclude_c_skill list below. Note down the final essential skills for this part of the analysis.

View(skill_for_candidate)

exclude_c_skill <- c("Building Effective Relationships", "Teaching", "Customer Service", "Customer Contact", "Bilingual",
                     "English", "Scheduling", "Multilingual", "Research", "Microsoft Windows", "Microsoft Office",
                     "Computer Literacy", "Microsoft Excel", "Lifting Ability", "Microsoft Word")

skill_for_candidate <- filter(skill_for_candidate, ! skill %in% exclude_c_skill)

res_id_include <- unique(filter(res_skill_base_clean, skill %in% skill_for_candidate$skill)$bgtresid)

###Finding the potential candidates, who have at least one of the hard skills
###included in (some threshhold) of jobs above

res_main <- filter(res_main_int, bgtresid %in% res_id_include)

###Finding all the skills related to potential candidates, which we are actually tracking for the match measure

res_skill <- filter(res_skill_base_clean, bgtresid %in% res_main$bgtresid, skill %in% skill_for_eval$skill)

###Summary of match skills of potential candidates

res_skill_sum <- res_skill %>% group_by(skillclusterfamily, skillcluster, skill) %>% summarise(count = length(unique(bgtresid)),
                                                                                               perc = round(length(unique(bgtresid))/length(unique(res_skill$bgtresid)), 3)) %>% arrange(desc(perc))

#######################################################
################ MAKING CONTINGENCY TABLES ############
##### your input will be needed to make sure the ######
#################### tables match #####################
#######################################################

res_contingency <- as.data.frame.matrix(table(res_skill[,c("bgtresid", "skill")]))
colnames(res_contingency) <- make.names(colnames(res_contingency))
res_contingency[res_contingency > 1] <- 1

################################################
####### MANUAL DATA CHECK NEEDED HERE ##########
################################################

###Are there any skills in the ad contingency table that are NOT in the resume contingency table?
###If the following line returns "0," there are not, and you can continue to the measuring
###mismatch section:
sum(!colnames(ad_contingency) %in% colnames(res_contingency))

#if there ARE skills missing, what are they?
colnames(ad_contingency)[which(!colnames(ad_contingency) %in% colnames(res_contingency))]

###IF THERE IS ONLY ONE MISSING COLUMN, DO THIS:
#replace COLNAME_GOES_HERE with the name that the previous line returned
res_contingency$COLNAME_GOES_HERE <- 0
res_contingency <- select(res_contingency, colnames(ad_contingency))


###IF THERE ARE MULTIPLE MISSING COLUMNS, JUST RUN THROUGH THIS:
missing <- as.matrix(ad_contingency[1:nrow(res_contingency),which(!colnames(ad_contingency) %in% colnames(res_contingency))])
missing[missing > 0] <- 0
res_contingency<- cbind(res_contingency, missing)
res_contingency <- select(res_contingency, colnames(ad_contingency))

##Check to make sure that it worked: this should return a 0
sum(!colnames(ad_contingency) %in% colnames(res_contingency))
##This should return a one:
sum(colnames(ad_contingency) == colnames(res_contingency))/length(ad_contingency)

#######################################################
################ MISMATCH MATRIX AND SCORE ############
##### no manual input needed, but you should note #####
############# down the value of mis_score #############
#######################################################

###make mismatch matrix. this might take a while.

mismatch_matrix <- function(ad_skill, res_skill){
  apply(ad_skill, MARGIN = 1, function(y){apply(res_skill, MARGIN = 1,
                                                function(x){sum(x[x == y], na.rm = TRUE)/sum(y, na.rm = TRUE)})})
}

mis_matrix <- mismatch_matrix(ad_contingency, res_contingency)





#####plots!

#heat map

ad_melt <- melt(mis_matrix, id.vars = colnames(mis_matrix), value.name = "match")
colnames(ad_melt) <- c("resume", "ad", "match")
ad_melt$ad <- as.character(ad_melt$ad)
ad_melt_lim <- filter(ad_melt, match > 0)
ad_melt_desc <- ad_melt_lim %>% group_by(ad) %>% summarise(max = max(match), min = min(match)) %>% arrange(desc(max))
ad_melt_desc$order <- 1:nrow(ad_melt_desc)
ad_melt_lim <- left_join(ad_melt_lim, ad_melt_desc[,c("ad", "order")])

ad_melt <- left_join(ad_melt, ad_melt_desc[,c("ad", "order")])
ad_melt_desc_all <- ad_melt %>% group_by(order) %>% summarise(max = max(match), min = min(match))


heat_3 <- data.frame(ad = colnames(mis_matrix), median = NA, one = NA, two = NA, three = NA)
heat_3$zero <- sapply(heat_3$ad, function(x){nrow(filter(ad_melt, ad == x, match == 0))})
heat_3$one <- sapply(heat_3$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match <= .33))})
heat_3$two <- sapply(heat_3$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .33, match <=.66))})
heat_3$three <- sapply(heat_3$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .66))})
heat_3$median <- sapply(heat_3$ad, function(x){median(filter(ad_melt, ad == x)$match)})
heat_3$median_lim <- sapply(heat_3$ad, function(x){median(filter(ad_melt_lim, ad == x)$match)})
heat_3$max <- sapply(heat_3$ad, function(x){max(filter(ad_melt_lim, ad == x)$match)})
heat_3$maxcount <- sapply(heat_3$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match == heat_3[heat_3$ad == x,"max"]))})
ad_skill$bgtjobid_c <- as.character(ad_skill$bgtjobid)
heat_3$nskill <- sapply(heat_3$ad, function(x){length(unique(filter(ad_skill, bgtjobid_c == x, skill %in% skill_for_eval$skill)$skill))})


zero_one <- filter(heat_3, one == 0, two == 0)
zero_one$ad_c <- as.character(zero_one$ad)
ad_skill$bgtjobid_c <- as.character(ad_skill$bgtjobid)
zero_one$nskill <- sapply(zero_one$ad_c, function(x){length(unique(filter(ad_skill, bgtjobid_c == x, skill %in% skill_for_eval$skill)$skill))})

zero_two_three <- filter(heat_3, one == 0, two != 0, zero != 0, three !=0)
zero_two_three$ad_c <- as.character(zero_two_three$ad)
ad_skill$bgtjobid_c <- as.character(ad_skill$bgtjobid)
zero_two_three$nskill <- sapply(zero_two_three$ad_c, function(x){length(unique(filter(ad_skill, bgtjobid_c == x, skill %in% skill_for_eval$skill)$skill))})

#heat_3 <- arrange(heat_3, max, maxcount)
#heat_3 <- arrange(heat_3, round(three_perc, 1), round(median, 1), round(zero_perc, 1))
#heat_3 <- arrange(heat_3, round(median, 1), round(three_perc, 1), round(zero_perc, 1))

heat_3 <- arrange(heat_3, median_lim, desc(round(three_perc, 1)), round(zero_perc, 1))
#heat_3 <- arrange(heat_3, round(zero_perc,1), desc(round(three_perc, 1)), median_lim)


#heat_3 <- arrange(heat_3, median_lim, round(max,1))
#heat_3 <- arrange(heat_3, max, maxcount, median_lim)

heat_3$zero_perc <- heat_3$zero/203
heat_3$one_perc <- heat_3$one/203
heat_3$two_perc <- heat_3$two/203
heat_3$three_perc <- heat_3$three/203


heat_3$order <- nrow(heat_3):1

heat_3_melt <- melt(heat_3, id.vars = "order", measure.vars = c("zero_perc","one_perc", "two_perc", "three_perc"))
heat_3_melt$match_level <- factor(heat_3_melt$variable, levels = c("three_perc", "two_perc", "one_perc", "zero_perc"))


ggplot(heat_3_melt, aes(x = variable, y = order, z = value))+geom_tile(aes(fill = value))+
  theme_classic()+scale_fill_gradient(low="white", high="blue", limits = c(0,1))+scale_x_discrete(labels = c("0","0.01-0.33", "0.34-0.66", "0.67-1.0"))+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  labs(title = "Match Scores: Critical Care Nurses in Richmond, VA", subtitle = "arranged by median and maximum match", y = "Job Ads (sorted by median match)", x = "Match score")+
  geom_vline(xintercept = c(.5, 1.5, 2.5, 3.5, 4.5), alpha = .1)


ggplot(heat_3, aes(x = max, y = median))+geom_jitter(alpha = .1)+theme_classic()

ggplot(heat_3_melt, aes(x = order, y = value, color = match_level, fill = match_level))+
  geom_bar(stat = 'identity')+theme_classic()+
  scale_color_manual(values = c("#283B5B", "#6A8AC1", "#B5C6E5", "#FFC58C"))+
  scale_fill_manual(values = c("#283B5B", "#6A8AC1", "#B5C6E5", "#FFC58C"))+
  labs(title = "Match scores for Critical Care Nurse ads in Blacksburg, VA", subtitle = "sorted by median")


heat_3 <- arrange(heat_3, nskill)
heat_3$order <- 1:nrow(heat_3)


heat_3_melt <- melt(heat_3, id.vars = "order", measure.vars = c("zero_perc","one_perc", "two_perc", "three_perc"))
heat_3_melt$match_level <- factor(heat_3_melt$variable, levels = c("three_perc", "two_perc", "one_perc", "zero_perc"))

heat_3 %>% group_by(nskill) %>% summarise(count = n(), perc = n()/nrow(heat_3))

ggplot(heat_3_melt, aes(x = order, y = value, color = match_level, fill = match_level))+
  geom_bar(stat = 'identity')+theme_classic()+
  scale_color_manual(values = c("#283B5B", "#6A8AC1", "#B5C6E5", "#FFC58C"))+
  scale_fill_manual(values = c("#283B5B", "#6A8AC1", "#B5C6E5", "#FFC58C"))+
  labs(title = "Match scores for Critical Care Nurse ads in Blacksburg, VA", subtitle = "sorted by number of skills per ad") +
  geom_vline(xintercept = c(min(which(heat_3$nskill > 1)), min(which(heat_3$nskill > 2)),
                            min(which(heat_3$nskill > 3)), min(which(heat_3$nskill > 4)),
                            min(which(heat_3$nskill > 5)), min(which(heat_3$nskill > 6)),
                            min(which(heat_3$nskill > 7)), min(which(heat_3$nskill > 8)),
                            min(which(heat_3$nskill > 9)), min(which(heat_3$nskill > 10)),
                            min(which(heat_3$nskill > 11)), min(which(heat_3$nskill > 12)),
                           min(which(heat_3$nskill > 13)))-.5)



                           , min(which(heat_3$nskill > 14)),
                          min(which(heat_3$nskill > 15))))




heat_5 <- data.frame(ad = colnames(mis_matrix), median = NA, one = NA, two = NA, three = NA, four = NA, five = NA)
heat_5$one <- sapply(heat_5$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match <= .2))})
heat_5$two <- sapply(heat_5$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .2, match <=.4))})
heat_5$three <- sapply(heat_5$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .4, match <= .6))})
heat_5$four <- sapply(heat_5$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .6, match <= .8))})
heat_5$five <- sapply(heat_5$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .8))})
heat_5$median <- sapply(heat_5$ad, function(x){median(filter(ad_melt_lim, ad == x)$match)})
heat_5$max <- sapply(heat_5$ad, function(x){max(filter(ad_melt_lim, ad == x)$match)})
heat_5$maxcount <- sapply(heat_5$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .6, match == max(filter(ad_melt_lim, ad == x)$match)))})


heat_5 <- arrange(heat_5, median)
heat_5$order <- 1:nrow(heat_5)

heat_5_melt <- melt(heat_5, id.vars = "order", measure.vars = c("one", "two", "three", "four", "five"))

ggplot(heat_5_melt, aes(x = variable, y = order, z = value))+geom_tile(aes(fill = value))+
  theme_classic()+scale_fill_gradient(low="white", high="blue")+scale_x_discrete(labels = c("0.01-0.20", "0.21-0.40", "0.41-0.60", "0.61-0.80", "0.81-1.0"))+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  labs(title = "Match Scores: Critical Care Nurses in Blacksburg, VA", y = "Job Ads (sorted by median match)", x = "Match score")+
  geom_vline(xintercept = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5), alpha = .1)

heat_10 <- data.frame(ad = colnames(mis_matrix), one = NA, two = NA, three = NA, four = NA, five = NA, six = NA, seven = NA, eight = NA, nine = NA, ten = NA)
heat_10$one <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match <= .1))})
heat_10$two <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .1, match <=.2))})
heat_10$three <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .2, match <=.3))})
heat_10$four <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .3, match <=.4))})
heat_10$five <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .4, match <=.5))})
heat_10$six <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .5, match <=.6))})
heat_10$seven <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .6, match <=.7))})
heat_10$eight <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .7, match <=.8))})
heat_10$nine <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .8, match <=.9))})
heat_10$ten <- sapply(heat_10$ad, function(x){nrow(filter(ad_melt_lim, ad == x, match > .9))})
heat_10$median <- sapply(heat_10$ad, function(x){median(filter(ad_melt_lim, ad == x)$match)})

heat_10 <- arrange(heat_10, median)
heat_10$order <- 1:nrow(heat_10)

heat_10_melt <- melt(heat_10, id.vars = "order", measure.vars = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"))

ggplot(heat_10_melt, aes(x = variable, y = order, z = value))+geom_tile(aes(fill = value))+
  scale_fill_gradient(low="white", high="blue")+labs(title = "Non-Zero Matches Between Ads and Candidates: Critical Care Nurses in Blacksburg, Virginia",
                                                     y = "Job Ads (one row per ad, sorted by median match score)", x = "Match Score") + scale_x_discrete(labels = c("0.01-0.1", "0.11-0.2", "0.21-0.3",
                                                                                                                                                                    "0.31-0.4", "0.41-0.5", "0.51-0.6", "0.61-0.7", "0.71-0.8", "0.81-0.9", "0.91-1.0"))+ scale_y_continuous(labels = NULL, breaks = NULL)+
  geom_vline(xintercept = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5), alpha = .1)+theme_classic()




##skill training impact


match_change <- function(t){
  change_v <- numeric()

  for(i in 1:nrow(t)){

    skill_name <- make.names(t[i,"skill"])


    ad_skillcount <- as.data.frame(apply(ad_contingency[ad_contingency[,colnames(ad_contingency) == skill_name]==1,], MARGIN = 1, sum))
    colnames(ad_skillcount) <- c("skillcount")
    ad_skillcount$ad <- rownames(ad_skillcount)
    ad_skillcount$skillperc <- 1/ad_skillcount$skillcount

    ad_melt$resume_c <- as.character(ad_melt$resume)

    skill_change <- filter(ad_melt, ad %in% ad_skillcount$ad, resume_c %in% rownames(res_contingency[res_contingency[,colnames(res_contingency) == skill_name]==0,]))
    skill_change <- left_join(skill_change, ad_skillcount[,c("ad","skillperc")])
    sum(skill_change$skillperc)
    change <- sum(skill_change$skillperc)/nrow(ad_melt)

    change_v <- c(change_v,change)
  }
  change_v
}

skill_for_eval$match_change <- match_change(skill_for_eval)

ggplot(skill_for_eval, aes(x = perc, y = match_change, label = skill))+
  geom_smooth(method = "lm", se = FALSE, alpha = .25, color = "#D3D3D3")+
  geom_point(data = skill_for_eval[1:10,], aes(x = perc, y = match_change, label = skill))+theme_classic()+
  labs(title = "Potential Impact of Skill Training", subtitle = "Critical Care Nurses in Blacksburg, VA",
       y = "Average change in match score if all candidates trained in skill",
       x = "Percent of ads calling for skill")+
  geom_text_repel(data = skill_for_eval[1:10,], aes(x = perc, y = match_change, label = skill), size = 4)+
  scale_y_continuous(limits = c(0,.2))

m1 <- lm(skill_for_eval$match_change~skill_for_eval$perc)
skill_for_eval$resid <- resid(m1)

ggplot(skill_for_eval[1:10,], aes(x = perc, y = resid, label = skill))+
  geom_rect(aes(ymin = -.08, ymax = -0.02, xmin = 0, xmax = .9), fill = "blue", alpha = 0.001)+
  geom_rect(aes(ymin = -0.02, ymax = 0.02, xmin = 0, xmax = .9), fill = "blue", alpha = 0.005)+
  geom_rect(aes(ymin = 0.02, ymax = 0.08, xmin = 0, xmax = .9), fill = "blue", alpha = 0.01)+
  geom_rect(aes(ymin = -.08, ymax = 0.08, xmin = 0, xmax = 0.3), fill = "blue", alpha = 0.001)+
  geom_rect(aes(ymin = -.08, ymax = 0.08, xmin = .3, xmax = 0.6), fill = "blue", alpha = 0.005)+
  geom_rect(aes(ymin = -.08, ymax = 0.08, xmin = .6, xmax = 0.9), fill = "blue", alpha = 0.01)+
  geom_point()+geom_segment(aes(xend = perc, yend = 0))+
  geom_text_repel(size = 3)+geom_hline(yintercept = 0)+labs(y = "Impact of training on match", x = "Percent of ads calling for skill",
                                                            title = "Impact of additional skill training on matches to Critical Care Nursing jobs", subtitle = "Richmond, VA")+theme_classic()+
  scale_y_continuous(breaks = NULL, labels = NULL, limits = c(-0.08, 0.08))

ggplot(ad_melt, aes(x=match))+geom_density(aes(y=..scaled..), fill = "blue", alpha = .5, color = "blue")+theme_classic()+
  labs(x = "Match score", y = "Density", title = "All match scores for Critical Care Nurses in Richmond, VA")

###all match dot/bar chart

ad_melt_dot <- ad_melt %>% group_by(order, match) %>% summarise(count = n())


ggplot()+geom_jitter(data = ad_melt, aes(y=match, x=order), alpha = .05, size = .25, width = .005, fill = "blue", color = "blue")+
  geom_segment(data = ad_melt_desc_all, aes(y=min, yend=max, x=order, xend=order), size = .25, alpha = .1, color = "blue")+
  theme_classic()+coord_flip()+
  labs(title = "Critical Care Nurses in Blacksburg, Virginia",
       subtitle = "All Matches between Ads and Candidates",
       x = "Ads (sorted by maximum match score)", y = "Match score")+scale_x_discrete(labels = NULL)


ggplot()+geom_point(data = ad_melt_dot, aes(y=match, x=order, color = count), size = .1,)+scale_color_gradient(low="yellow", high="red")+
  geom_segment(data = ad_melt_desc_all, aes(y=min, yend=max, x=order, xend=order), size = .1, alpha = .5, color = "yellow")+
  theme_classic()+coord_flip()+
  labs(title = "Critical Care Nurses in Blacksburg, Virginia",
       subtitle = "All Matches between Ads and Candidates",
       x = "Ads (sorted by maximum match score)", y = "Match score")+scale_x_discrete(labels = NULL)

