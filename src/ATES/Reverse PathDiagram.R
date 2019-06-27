#Look at everyone Associate's Degree (AD) and Less
#Responded to EDUATTN - Highest Degree or level of school completion
#Salary dot plot by education level, credential, whether credential for current job, and whether STW or nonSTW job
##################################################################################################################
library(readxl)
library(ggplot2)

load("~/git/stem_edu/data/stem_edu/original/ATES/ates_pu_pert.rdata")
setwd("~/git/stem_edu/src/VAL")
ATES<-ates_pu_pert; remove(ates_pu_pert); table(ATES$eduatt)
#Read in Census Occupation Codes, descriptions, and STW designation
COC<-read_excel("~/git/stem_edu/data/stem_edu/original/COC_STW.xls")
COC$STW<-ifelse(COC$STW==0,"NonSTW","STW");COC$STW<-as.factor(COC$STW)
PATH<-merge(ATES, COC, by.x="empocc", by.y="Code", all.x=TRUE, all.y=FALSE)

#Separate AD and less (AD=29223 / 61% of the respondents)
AD<-PATH[PATH$eduattn<7,c(1:97,355:359)]; dim(AD); dim(AD)[1]/dim(ATES)[1]; table(ATES$eduattn); table(AD$eduattn)

#Only keep the people with reported earnings over the last 12 months Q62 (PSC=20751)
#8472 people were removed
PSC<-AD[AD$eeearn>0,]; dim(PSC)

#Check the occurrence of multiple credentials
#First recode so that 1 indictes the credential and 0 no credential
#cnmain=1/CNMAIN2=1/CNMAIN3=1/certprog=1/weprog=3
PSC$cnmain<-ifelse(PSC$cnmain==1,1,0)
PSC$CNMAIN2<-ifelse(PSC$CNMAIN2==1,1,0)
PSC$CNMAIN3<-ifelse(PSC$CNMAIN3==1,1,0)
PSC$certprog<-ifelse(PSC$certprog==1,1,0)
PSC$weprog<-ifelse(PSC$weprog==3,1,0)
#View(PSC[,c(12,28,43,52,64)])

#Create a variable to identify a responder with no credential
#14027 have no credential or 14027/29223 or 48% of those with an AD or less have no credential
NoCredential<-PSC$cnmain+PSC$CNMAIN2+PSC$CNMAIN3+PSC$certprog+PSC$weprog

#Recode the education variable into 3 levels
#Created a new education variable Associates Degree=3556, Some College=8117, High School or Less=9078
Education<-ifelse(PSC$eduattn==6, "Associates Degree", ifelse((PSC$eduattn==5 | PSC$eduattn==4),
                  "Some College", "High School or Less"))

#Recode the earnings data from intervals into the median
#Create a new salary variable for earnings over the last 12 months Q62
Salary<-ifelse(PSC$eeearn==1,5000,ifelse(PSC$eeearn==2,15000,ifelse(PSC$eeearn==3,25000,ifelse(PSC$eeearn==4,35000,
               ifelse(PSC$eeearn==5,45000,ifelse(PSC$eeearn==6,55000,ifelse(PSC$eeearn==7,67500,
                      ifelse(PSC$eeearn==8,112500,ifelse(PSC$eeearn==9,175000,NA)))))))))
#(20751x104)
PSC<-data.frame(NoCredential, Education, Salary, PSC); dim(PSC)

#Create variables that states whether the credential is for the current job
#>1=credential for current job (5458); 0=no credential
#Primary Certification/license for current job Q14 (3360)
PSC$CNCURRJOB1<-ifelse(PSC$CNCURRJOB1==3,1,0)
#Second Certification/license for current job Q23 (1027)
PSC$CNCURRJOB2<-ifelse(PSC$CNCURRJOB2==3,1,0)
CLcurrent<-PSC$CNCURRJOB1+PSC$CNCURRJOB2 #(0=17311 / 1=2493 / 2=947)
#Certificate for current job Q37 (0=18940 / 1=1811)
CTcurrent<-ifelse((PSC$lccurrjob==3 | PSC$lccurrjob==4),1,0)
#Work Experience Program for current job Q47 (0=19235 / 1=1516)
WEcurrent<-ifelse((PSC$wecurjo==3 | PSC$wecurjo==4),1,0)

FINALjob<-data.frame(CLcurrent, CTcurrent, WEcurrent, PSC[,c(1:3,13,53,65,105)], Tally=rep(1,length(Salary))); dim(FINALjob)
  FINALjob$STW<-ordered(FINALjob$STW, levels=c("STW","NonSTW"))
  FINALjob$Education<-ordered(FINALjob$Education, levels=c("High School or Less","Some College","Associates Degree"))

#Datasets where the credential is for the job
CL<-FINALjob[FINALjob$cnmain==1 & FINALjob$CLcurrent>0, ];dim(CL) #3440
View(CL)
CT<-FINALjob[FINALjob$certprog==1 & FINALjob$CTcurrent>0, ];dim(CT) #1811
View(CT)
WE<-FINALjob[FINALjob$weprog==1 & FINALjob$WEcurrent>0, ];dim(WE) #1516
View(WE)
ForJob<-data.frame(Credential=rep(c("CL","CT","WE"),c(3440,1811,1516)),
                   Education=c(as.character(CL[,5]), as.character(CT[,5]), as.character(WE[,5])),
                   Salary=c(CL[,6], CT[,6], WE[,6]),
                   STW=c(as.character(CL[,10]), as.character(CT[,10]), as.character(WE[,10])),
                   Tally=c(CL[,11], CT[,11], WE[,11]))
ForJob$Education<-ordered(ForJob$Education, levels=c("High School or Less","Some College","Associates Degree"))
ForJob$STW<-ordered(ForJob$STW, levels=c("STW","NonSTW"))

#Datasets where the credential is NOT for the job
CLnfj<-FINALjob[FINALjob$cnmain==1 & FINALjob$CLcurrent==0, ];dim(CLnfj) #886
View(CLnfj)
CTnfj<-FINALjob[FINALjob$certprog==1 & FINALjob$CTcurrent==0, ];dim(CTnfj) #1731
View(CTnfj)
WEnfj<-FINALjob[FINALjob$weprog==1 & FINALjob$WEcurrent==0, ];dim(WEnfj) #1004
View(WEnfj)
NotForJob<-data.frame(Credential=rep(c("CL","CT","WE"),c(886,1731,1004)),
                      Education=c(as.character(CLnfj[,5]), as.character(CTnfj[,5]), as.character(WEnfj[,5])),
                      Salary=c(CLnfj[,6], CTnfj[,6], WEnfj[,6]),
                      STW=c(as.character(CLnfj[,10]), as.character(CTnfj[,10]), as.character(WEnfj[,10])),
                      Tally=c(CLnfj[,11], CTnfj[,11], WEnfj[,11]))
NotForJob$Education<-ordered(NotForJob$Education, levels=c("High School or Less","Some College","Associates Degree"))
NotForJob$STW<-ordered(NotForJob$STW, levels=c("STW","NonSTW"))

#Dataset for nocredential
NC<-FINALjob[NoCredential==0, ];dim(NC) #13003
View(NC)


#Credential for Job
    ForJobmn<-aggregate(Salary~STW+Credential+Education, data=ForJob, mean)
    ForJobse<-aggregate(Salary~STW+Credential+Education, data=ForJob, std.error)
    ForJobn<-aggregate(Tally~STW+Credential+Education, data=ForJob, sum)
PLOTforJob<-data.frame(Education=as.factor(ForJobmn$Education), Credential=ForJobmn$Credential, STW=ForJobmn$STW,
                       Mean=ForJobmn$Salary, StdError=ForJobse$Salary, Sample=ForJobn$Tally)
#Credential NOT for Job
    NotforJobmn<-aggregate(Salary~STW+Credential+Education, data=NotForJob, mean)
    NotforJobse<-aggregate(Salary~STW+Credential+Education, data=NotForJob, std.error)
    NotforJobn<-aggregate(Tally~STW+Credential+Education, data=NotForJob, sum)
PLOTnotforJob<-data.frame(Education=NotforJobmn$Education, Credential=NotforJobmn$Credential, STW=NotforJobmn$STW,
                          Mean=NotforJobmn$Salary, StdError=NotforJobse$Salary, Sample=NotforJobn$Tally)
#No Credential
     NOcredmn<-aggregate(Salary~STW+Education, data=NC, mean)
     NOcredse<-aggregate(Salary~STW+Education, data=NC, std.error)
     NOcredn<-aggregate(Tally~STW+Education, data=NC, sum)
PLOTNOcred<-data.frame(Education=NOcredmn$Education, Credential=rep("NC",dim(NOcredmn)[1]), STW=NOcredmn$STW,
                       Mean=NOcredmn$Salary, StdError=NOcredse$Salary, Sample=NOcredn$Tally)

#Combine the credential for and not for job
FINALplot<-data.frame(PLOTforJob, Mean.nfj=NotforJobmn$Salary, StdError.nfj=NotforJobse$Salary, Sample.nfj=NotforJobn$Tally)

#Color blind palette with grey
cbPalette<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#Assign colors to each Education Level
CREcolor<-rep(NA, length=length(FINALplot$Credential))
CREcolor[which(FINALplot$Credential=="CL")]="#E69F00"
CREcolor[which(FINALplot$Credential=="CT")]="#0072B2"
CREcolor[which(FINALplot$Credential=="WE")]="#D55E00"
#CREcolor[which(FINALplot$Credential=="NoCredential")]="#999999"

SALARY<-ggplot(FINALplot, aes(Mean, c(18:1))) +
  annotate("rect", xmin=40354, xmax=42333, ymin=13, ymax=18, alpha=0.1, fill="#009E73") +
  annotate("rect", xmin=30547, xmax=31284, ymin=13, ymax=18, alpha=0.1, fill="#009E73") +
  annotate("rect", xmin=51694, xmax=54773, ymin=7, ymax=12, alpha=0.1, fill="#009E73") +
  annotate("rect", xmin=36132, xmax=37297, ymin=7, ymax=12, alpha=0.1, fill="#009E73") +
  annotate("rect", xmin=58227, xmax=62821, ymin=1, ymax=6, alpha=0.1, fill="#009E73") +
  annotate("rect", xmin=39780, xmax=41692, ymin=1, ymax=6, alpha=0.1, fill="#009E73") +
  geom_segment(aes(x=Mean-StdError, y=c(18:1), xend=Mean+StdError, yend=c(18:1)), colour="grey", size=4, alpha=0.5, data=FINALplot) +
  geom_segment(aes(x=Mean.nfj-StdError.nfj, y=c(18:1), xend=Mean.nfj+StdError.nfj, yend=c(18:1)), colour="grey", size=4, alpha=0.5, data=FINALplot) +
  geom_point(aes(Mean.nfj, c(18:1)), colour="grey", size=5) +
  geom_point(aes(Mean, c(18:1)), colour="black", size=5) +
  geom_point(aes(Mean.nfj, c(18:1)), colour=CREcolor, size=4) +
  geom_point(aes(Mean, c(18:1)), colour=CREcolor, size=4) +
  scale_y_continuous(breaks=c(1:18), labels=c("","Work Experience Program","","Credential","","Certification/License",
                                              "","Work Experience Program","","Credential","","Certification/License",
                                              "","Work Experience Program","","Credential","","Certification/License")) +
  scale_x_continuous(breaks=seq(15000, 75000, by=5000)) +
  scale_color_manual(values=CREcolor) +
  theme_val() + xlab("") + ylab("") +
  theme(axis.text.x=element_text(angle=90)) +
  theme(legend.position="none",
        axis.text.y=element_text(size=12, hjust=1.1, vjust=3)) +
  annotate("text", x=17500, y=3.5, label="Associates Degree", adj=0.5, angle=90, size=4) +
  annotate("text", x=17500, y=9.5, label="Some College", adj=0.5, angle=90, size=4) +
  annotate("text", x=17500, y=15.5, label="High School or Less", angle=90, adj=0.5, size=4) +
  geom_hline(yintercept=6.5, linetype=2) +
  geom_hline(yintercept=12.5, linetype=2) +
  annotate("text", x=15000, y=seq(1,17,2), label="nonSTW", adj=0.5, size=3, colour="#A0A0A3") +
  annotate("text", x=15000, y=seq(2,18,2), label="STW", adj=0.5, size=3, colour="#A0A0A3")
SALARY


ggsave("SALARY.pdf", width=6, height=10)

    std.error<-function(x) sd(x)/sqrt(length(x))





















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

