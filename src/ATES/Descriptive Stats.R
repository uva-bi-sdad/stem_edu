library(readxl)
library(ggplot2)
#Color blind palette with grey
cbPalette<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#FILE FOR HIGH SCHOOL DIPLOMA/GED or HIGH SCHOOL DIPLOMA/GED
load("~/git/stem_edu/data/stem_edu/original/ATES/ates_pu_pert.rdata")
setwd("~/git/stem_edu/src/VAL")
ATES<-ates_pu_pert; remove(ates_pu_pert); table(ATES$eduatt)
#Read in Census Occupation Codes, descriptions, and STW designation
COC<-read_excel("~/git/stem_edu/data/stem_edu/original/COC_STW.xls")
COC$STW<-ifelse(COC$STW==0,"NonSTW","STW");COC$STW<-as.factor(COC$STW)

#Credentials by regular education level
#sum the three credential types to get % no credential by education level
WP<-ifelse(ATES$weprog==3,1,0);LC<-ifelse(ATES$cnmain==1,1,0);CR<-ifelse(ATES$certprog==1,1,0)
Credentials<-WP+LC+CR
(data.frame(table(ATES$eduatt,Credentials))[1:10,3]/data.frame(table(ATES$eduatt))[,2])*100
#% with certification/license 
data.frame(table(ATES$eduatt,ATES$cnmain))[1:10,3]
round((data.frame(table(ATES$eduatt,ATES$cnmain))[1:10,3])/((data.frame(table(ATES$eduatt))[,2]))*100,1)
#% with certificate
data.frame(table(ATES$eduatt,ATES$certprog))[1:10,3]
round((data.frame(table(ATES$eduatt,ATES$certprog))[1:10,3])/((data.frame(table(ATES$eduatt))[,2]))*100,1)
#% with certificate
data.frame(table(ATES$eduatt,ATES$weprog))[21:30,3]
round((data.frame(table(ATES$eduatt,ATES$weprog))[21:30,3])/((data.frame(table(ATES$eduatt))[,2]))*100,1)




