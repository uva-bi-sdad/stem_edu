#Run the code under the NoCollege and HighSchool files to get the data
#sets for each credential type - then combine the six data sets

###########################################################################
#NO COLLEGE
#Analyses for certification/license
#Select those that have a current certification/license
T1<-NC[NC$cnmain==1,]; dim(T1) #1440
CL<-data.frame(T1[,c(12,356,21,92)]);names(CL)<-c("MOG","STW","CURRENTJOB","EARN") #1440
#Remove MOGs that are "Missing" (93) or "Uncodeable" (15)
temp<-CL[CL$MOG!="Missing", ]; temp<-temp[temp$MOG!="Uncodeable", ]; temp$MOG<-factor(temp$MOG); CL<-temp
CL$CURRENTJOB<-ifelse(CL$CURRENTJOB==3,"YES",ifelse(CL$CURRENTJOB==1,"Not Working","NO")); rm(T1)
###########################################################################
#Analyses for certificates
#Select those that have a certificate
T1<-NC[NC$certprog==1,]; dim(T1)  #1284
#Recode preparation Question 32
CE<-data.frame(T1[,c(51,356,58,92)]);names(CE)<-c("MOG","STW","CURRENTJOB","EARN")
CE$CURRENTJOB<-ifelse(CE$CURRENTJOB==1, "Not Working", ifelse(CE$CURRENTJOB==2,"NO", ifelse(CE$CURRENTJOB==-1,"Valid Skip", "YES"))); rm(T1)
###########################################################################
#Analyses for work experience credential
#Select those that have a work experimence credential
T1<-NC[NC$weprog==3,]; dim(T1) #780
WE<-data.frame(T1[,c(63,356,75,92)]);names(WE)<-c("MOG","STW","CURRENTJOB","EARN") #1440
WE$CURRENTJOB<-ifelse(WE$CURRENTJOB==1, "Not Working", ifelse(WE$CURRENTJOB==2,"NO", ifelse(WE$CURRENTJOB==-1,"Valid Skip", "YES"))); rm(T1)

library(dplyr)
NCCL<-data.frame(Credential=rep("Certification/License",dim(CL)[1]),CL)
NCCE<-data.frame(Credential=rep("Certificate",dim(CE)[1]),CE)
NCWE<-data.frame(Credential=rep("Work Experience Program",dim(WE)[1]),WE)
NCPD<-data.frame(rbind(NCCL,NCCE,NCWE));NCPD$MOG<-as.character(NCPD$MOG)

#Recode the MOG based on the percentage STW
#21-0000,25-0000,31-0000,39-0000,41-0000 are all 0%
#11-0000,13-0000,23-0000,35-0000,53-0000,43-0000 are all <3%
NCPD$MOG<-(if_else(NCPD$MOG=="21-0000" | NCPD$MOG=="25-0000" | NCPD$MOG=="31-0000" | NCPD$MOG=="39-0000" | NCPD$MOG=="41-0000",
                   "Not STW", if_else(NCPD$MOG=="11-0000" | NCPD$MOG=="13-0000" | NCPD$MOG=="23-0000" | NCPD$MOG=="35-0000" |
                                        NCPD$MOG=="53-0000" | NCPD$MOG=="43-0000", "< 3% STW", NCPD$MOG)))
NCPD$MOG<-ordered(NCPD$MOG, levels=c("49-0000","47-0000","29-0000","17-0000","51-0000","15-0000","27-0000","45-0000","33-0000",
                                     "19-0000","< 3% STW","Not STW","OTHER"))
dim(NCPD)
NCPDyes<-NCPD[NCPD$CURRENTJOB=="YES",];dim(NCPDyes);NCPDyes<-NCPDyes[complete.cases(NCPDyes), ];dim(NCPDyes)
NCPDyes<-NCPDyes[order(NCPDyes$MOG),]
(table(NCPDyes$MOG)/dim(NCPDyes)[1])*100
as.data.frame(table(NCPDyes$MOG,NCPDyes$Credential))
table(NCPDyes$STW,NCPDyes$MOG,NCPDyes$Credential);
dim(NCPDyes)

rm(CL,CE,WE)
###########################################################################
#HIGH SCHOOL
#Analyses for certification/license
#Select those that have a current certification/license
T1<-HS[HS$cnmain==1,]; dim(T1) #1440
CL<-data.frame(T1[,c(12,356,21,92)]);names(CL)<-c("MOG","STW","CURRENTJOB","EARN") #1440
#Remove MOGs that are "Missing" (93) or "Uncodeable" (15)
temp<-CL[CL$MOG!="Missing", ]; temp<-temp[temp$MOG!="Uncodeable", ]; temp$MOG<-factor(temp$MOG); CL<-temp
CL$CURRENTJOB<-ifelse(CL$CURRENTJOB==3,"YES",ifelse(CL$CURRENTJOB==1,"Not Working","NO")); rm(T1)
###########################################################################
#Analyses for certificates
#Select those that have a certificate
T1<-HS[HS$certprog==1,]; dim(T1)  #1284
#Recode preparation Question 32
CE<-data.frame(T1[,c(51,356,58,92)]);names(CE)<-c("MOG","STW","CURRENTJOB","EARN")
CE$CURRENTJOB<-ifelse(CE$CURRENTJOB==1, "Not Working", ifelse(CE$CURRENTJOB==2,"NO", ifelse(CE$CURRENTJOB==-1,"Valid Skip", "YES"))); rm(T1)
###########################################################################
#Analyses for work experience credential
#Select those that have a work experimence credential
T1<-HS[HS$weprog==3,]; dim(T1) #780
WE<-data.frame(T1[,c(63,356,75,92)]);names(WE)<-c("MOG","STW","CURRENTJOB","EARN") #1440
WE$CURRENTJOB<-ifelse(WE$CURRENTJOB==1, "Not Working", ifelse(WE$CURRENTJOB==2,"NO", ifelse(WE$CURRENTJOB==-1,"Valid Skip", "YES"))); rm(T1)

library(dplyr)
HSCL<-data.frame(Credential=rep("Certification/License",dim(CL)[1]),CL)
HSCE<-data.frame(Credential=rep("Certificate",dim(CE)[1]),CE)
HSWE<-data.frame(Credential=rep("Work Experience Program",dim(WE)[1]),WE)
HSPD<-data.frame(rbind(HSCL,HSCE,HSWE));HSPD$MOG<-as.character(HSPD$MOG)

#Recode the MOG based on the percentage STW
#21-0000,25-0000,31-0000,39-0000,41-0000 are all 0%
#11-0000,13-0000,23-0000,35-0000,53-0000,43-0000 are all <3%
HSPD$MOG<-(if_else(HSPD$MOG=="21-0000" | HSPD$MOG=="25-0000" | HSPD$MOG=="31-0000" | HSPD$MOG=="39-0000" | HSPD$MOG=="41-0000",
                   "Not STW", if_else(HSPD$MOG=="11-0000" | HSPD$MOG=="13-0000" | HSPD$MOG=="23-0000" | HSPD$MOG=="35-0000" |
                                        HSPD$MOG=="53-0000" | HSPD$MOG=="43-0000", "< 3% STW", HSPD$MOG)))
HSPD$MOG<-ordered(HSPD$MOG, levels=c("49-0000","47-0000","29-0000","17-0000","51-0000","15-0000","27-0000","45-0000","33-0000",
                                     "19-0000","< 3% STW","Not STW","OTHER"))
dim(HSPD)
HSPDyes<-HSPD[HSPD$CURRENTJOB=="YES",]
(table(HSPDyes$MOG)/dim(HSPDyes)[1])*100
table(HSPDyes$MOG,HSPDyes$Credential)
table(HSPDyes$STW,HSPDyes$MOG,HSPDyes$Credential)

rm(CL,CE,WE)
###########################################################################
#Associates Degree
#Analyses for certification/license
#Select those that have a current certification/license
T1<-AD[AD$cnmain==1,]; dim(T1) #1440
CL<-data.frame(T1[,c(12,356,21,92)]);names(CL)<-c("MOG","STW","CURRENTJOB","EARN") #1440
#Remove MOGs that are "Missing" (93) or "Uncodeable" (15)
temp<-CL[CL$MOG!="Missing", ]; temp<-temp[temp$MOG!="Uncodeable", ]; temp$MOG<-factor(temp$MOG); CL<-temp
CL$CURRENTJOB<-ifelse(CL$CURRENTJOB==3,"YES",ifelse(CL$CURRENTJOB==1,"Not Working","NO")); rm(T1)
###########################################################################
#Analyses for certificates
#Select those that have a certificate
T1<-AD[AD$certprog==1,]; dim(T1)  #1284
#Recode preparation Question 32
CE<-data.frame(T1[,c(51,356,58,92)]);names(CE)<-c("MOG","STW","CURRENTJOB","EARN")
CE$CURRENTJOB<-ifelse(CE$CURRENTJOB==1, "Not Working", ifelse(CE$CURRENTJOB==2,"NO", ifelse(CE$CURRENTJOB==-1,"Valid Skip", "YES"))); rm(T1)
###########################################################################
#Analyses for work experience credential
#Select those that have a work experimence credential
T1<-AD[AD$weprog==3,]; dim(T1) #780
WE<-data.frame(T1[,c(63,356,75,92)]);names(WE)<-c("MOG","STW","CURRENTJOB","EARN") #1440
WE$CURRENTJOB<-ifelse(WE$CURRENTJOB==1, "Not Working", ifelse(WE$CURRENTJOB==2,"NO", ifelse(WE$CURRENTJOB==-1,"Valid Skip", "YES"))); rm(T1)

library(dplyr)
ADCL<-data.frame(Credential=rep("Certification/License",dim(CL)[1]),CL)
ADCE<-data.frame(Credential=rep("Certificate",dim(CE)[1]),CE)
ADWE<-data.frame(Credential=rep("Work Experience Program",dim(WE)[1]),WE)
ADPD<-data.frame(rbind(ADCL,ADCE,ADWE));ADPD$MOG<-as.character(ADPD$MOG)

#Recode the MOG based on the percentage STW
#21-0000,25-0000,31-0000,39-0000,41-0000 are all 0%
#11-0000,13-0000,23-0000,35-0000,53-0000,43-0000 are all <3%
ADPD$MOG<-(if_else(ADPD$MOG=="21-0000" | ADPD$MOG=="25-0000" | ADPD$MOG=="31-0000" | ADPD$MOG=="39-0000" | ADPD$MOG=="41-0000",
                   "Not STW", if_else(ADPD$MOG=="11-0000" | ADPD$MOG=="13-0000" | ADPD$MOG=="23-0000" | ADPD$MOG=="35-0000" |
                                        ADPD$MOG=="53-0000" | ADPD$MOG=="43-0000", "< 3% STW", ADPD$MOG)))
ADPD$MOG<-ordered(ADPD$MOG, levels=c("49-0000","47-0000","29-0000","17-0000","51-0000","15-0000","27-0000","45-0000","33-0000",
                                     "19-0000","< 3% STW","Not STW","OTHER"))
dim(ADPD)
ADPDyes<-ADPD[ADPD$CURRENTJOB=="YES",]
(table(ADPDyes$MOG)/dim(ADPDyes)[1])*100
table(ADPDyes$MOG,ADPDyes$Credential)
table(ADPDyes$STW,ADPDyes$MOG,ADPDyes$Credential)

rm(CL,CE,WE)
###########################################################################

TOTAL<-data.frame(Education=rep(c("HS","NC","AD"),c(dim(HSPDyes)[1],dim(NCPDyes)[1],dim(ADPDyes)[1])),
                       rbind(HSPDyes,NCPDyes,ADPDyes))

dim(TOTAL)
TOTAL<-TOTAL[TOTAL$CURRENTJOB=="YES",];dim(TOTAL);TOTAL<-TOTAL[complete.cases(TOTAL), ];dim(TOTAL)
TOTAL<-TOTAL[order(TOTAL$MOG),]
  #Percentage by MOG
  round((table(TOTAL$MOG)/dim(TOTAL)[1])*100,1)
  #Credential Percentage within MOG
  MOGbyCred<-as.data.frame(table(TOTAL$MOG,TOTAL$Credential))
    names(MOGbyCred)<-c("MOG","Credential","Freq")
    MOGbyCred<-MOGbyCred[with(MOGbyCred, order(MOG)), ]
    DEN<-aggregate(Freq~MOG, data=MOGbyCred, sum);DEN<-rep(DEN$Freq, rep(3,13))
    MOGbyCred$Freq<-round((MOGbyCred$Freq/DEN)*100,1);
  #STEM Percentage within Credential within MOG
  MOGbyCredbySTW<-as.data.frame(table(TOTAL$MOG,TOTAL$Credential,TOTAL$STW))
    names(MOGbyCredbySTW)<-c("MOG","Credential","STW","Freq")
    MOGbyCredbySTW<-MOGbyCredbySTW[with(MOGbyCredbySTW, order(MOG, Credential, STW)), ]
    DEN<-aggregate(Freq~MOG+Credential, data=MOGbyCredbySTW, sum)
    den<-DEN[with(DEN, order(MOG, Credential)), ];den<-rep(DEN$Freq, rep(2,39))
    MOGbyCredbySTW$Freq<-round((MOGbyCredbySTW$Freq/den)*100,1)

#Recode Earnings over the last 12 months Q62
    TOTAL$EARN<-ifelse(TOTAL$EARN==1,5000,ifelse(TOTAL$EARN==2,15000,ifelse(TOTAL$EARN==3,25000,ifelse(TOTAL$EARN==4,35000,
                 ifelse(TOTAL$EARN==5,45000,ifelse(TOTAL$EARN==6,55000,ifelse(TOTAL$EARN==7,67500,ifelse(TOTAL$EARN==8,112500,
                  ifelse(TOTAL$EARN==9,175000,ifelse(TOTAL$EARN==-1,NA,TOTAL$EARN))))))))))



MN<-aggregate(EARN~MOG+Credential+STW, data=TOTAL, mean, na.rm=TRUE)
MN$Credential<-ordered(MN$Credential,levels=c("Certification/License","Certificate","Work Experience Program"))
MN$STW<-ordered(MN$STW,levels=c("STW","NonSTW"))
MN<-MN[with(MN, order(MOG, Credential, STW)),]
MN<-MN[c(1:51),]

#Check the number of observations in the aggregation
aggregate(rep(1,dim(TOTAL)[1])~MOG+Credential+STW, data=TOTAL, sum)
#Standard error function
STDERR<-function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}
SE<-aggregate(EARN~MOG+Credential+STW, data=TOTAL, STDERR)
SE$Credential<-ordered(SE$Credential,levels=c("Certification/License","Certificate","Work Experience Program"))
SE$STW<-ordered(SE$STW,levels=c("STW","NonSTW"))
SE<-SE[with(SE, order(SE$MOG, SE$Credential, SE$STW)),]
SE<-SE[c(1:51),]

#Create data set to plot
PLOT<-data.frame(EARN=c(MN$EARN[c(1:24)],0,0,MN$EARN[c(25:34)],0,0,MN$EARN[c(35,36)],0,MN$EARN[c(37:41)],0,0,MN$EARN[c(42:51)],0,0),
                 SE=c(SE$EARN[c(1:24)],0,0,SE$EARN[c(25:34)],0,0,SE$EARN[c(35,36)],0,SE$EARN[c(37:41)],0,0,SE$EARN[c(42:51)],0,0),
                 MOG=factor(rep(unique(MN$MOG),rep(6,10))))

#Color blind palette with grey
cbPalette<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#Assign colors to each MOG
MOGcolor<-rep(NA, length=length(PLOT$MOG))
MOGcolor[which(PLOT$MOG=="49-0000")]="#E69F00"
MOGcolor[which(PLOT$MOG=="47-0000")]="#0072B2"
MOGcolor[which(PLOT$MOG=="29-0000")]="#D55E00"
MOGcolor[which(PLOT$MOG=="17-0000")]="#CC79A7"
MOGcolor[which(PLOT$MOG=="51-0000")]="#999999"
MOGcolor[which(PLOT$MOG=="15-0000")]="#56B4E9"
MOGcolor[which(PLOT$MOG=="27-0000")]="#F0E442"
MOGcolor[which(PLOT$MOG=="45-0000")]="#009E73"
MOGcolor[which(PLOT$MOG=="33-0000")]="#E69F00"
MOGcolor[which(PLOT$MOG=="19-0000")]="#0072B2"

SALARY<-ggplot(PLOT, aes(EARN, c(60:1))) +
  geom_segment(aes(x=EARN-SE, y=c(60:1), xend=EARN+SE, yend=c(60:1)), colour=MOGcolor[25], size=3, alpha=0.5, data=PLOT) +
  geom_point(colour="black", size=5) +
  geom_point(colour=MOGcolor, size=4) +
  scale_y_continuous(breaks=c(1:60), labels=c(rep("",60))) +
  scale_x_continuous(breaks=seq(0,140000, by=20000)) +
  scale_color_manual(values=MOGcolor) +
  theme_val() + xlab("") + ylab("") +
  theme(axis.text.x=element_text(angle=90)) +
  theme(legend.position="none")

ggsave("SALARY.pdf", width=3, height=12)












#########################################################################################################
#Plot for STW by credential by MOG
library(ggalluvial)
#Color blind palette with grey
cbPalette<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

HSPath<-as.data.frame(table(HSPD$MOG[HSPD$CURRENTJOB=="YES"],HSPD$STW[HSPD$CURRENTJOB=="YES"]));names(HSPath)<-c("MOG","STW","FREQ")
HSPath<-HSPath[with(HSPath, order(MOG, STW)), ]


ggplot(data=HSPath,
       aes(y=FREQ, axis1=MOG, axis2=STW)) +
  geom_alluvium(aes(fill=MOG), width=0, knot.pos=0, reverse=FALSE) +
  guides(fill=FALSE) +
  theme_val() +
  geom_stratum(width=1/8, reverse=FALSE) +
  scale_fill_manual(values=c(cbPalette,cbPalette,cbPalette)) +
  geom_text(stat="stratum", label.strata=TRUE, reverse=FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("MOG", "Use", "STW")) +
  ggtitle("High School or Less")

