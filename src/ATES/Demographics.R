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
DEM<-merge(ATES, COC, by.x="empocc", by.y="Code", all.x=TRUE, all.y=FALSE)
#Separate AD and less (AD=29223 / 61% of the respondents)
DEM<-DEM[DEM$eduattn<7,]; dim(DEM); dim(DEM)[1]/dim(ATES)[1]

#Check the occurrence of multiple credentials
#First recode so that 1 indictes the credential and 0 no credential
#cnmain=1/CNMAIN2=1/CNMAIN3=1/certprog=1/weprog=3
DEM$cnmain<-ifelse(DEM$cnmain==1,1,0)
DEM$CNMAIN2<-ifelse(DEM$CNMAIN2==1,1,0)
DEM$CNMAIN3<-ifelse(DEM$CNMAIN3==1,1,0)
DEM$certprog<-ifelse(DEM$certprog==1,1,0)
DEM$weprog<-ifelse(DEM$weprog==3,1,0)

#Create a variable to identify a responder with no credential
#14027 have no credential or 14027/29223 or 48% of those with an AD or less have no credential
NoCredential<-DEM$cnmain+DEM$CNMAIN2+DEM$CNMAIN3+DEM$certprog+DEM$weprog

#Recode the education variable into 3 levels
#Created a new education variable Associates Degree=3556, Some College=8117, High School or Less=9078
Education<-ifelse(DEM$eduattn==6, "Associates Degree", ifelse((DEM$eduattn==5 | DEM$eduattn==4),
                                                              "Some College", "High School or Less"))

#Demographics for respondents with an AD or less
table(DEM$xxinthome,Education)
table(DEM$xxsex,Education)
summary(DEM$xxage[Education=="Associates Degree"])
  summary(DEM$xxage[Education=="Some College"])
  summary(DEM$xxage[Education=="High School or Less"])
  
#Create variables that states whether the credential is for the current job
#>1=credential for current job (5458); 0=no credential
#Primary Certification/license for current job Q14 (3360)
DEM$CNCURRJOB1<-ifelse(DEM$CNCURRJOB1==3,1,0)
#Second Certification/license for current job Q23 (1027)
DEM$CNCURRJOB2<-ifelse(DEM$CNCURRJOB2==3,1,0)
CLcurrent<-DEM$CNCURRJOB1+DEM$CNCURRJOB2 #(0=17311 / 1=2493 / 2=947)
#Certificate for current job Q37 (0=18940 / 1=1811)
CTcurrent<-ifelse((DEM$lccurrjob==3 | DEM$lccurrjob==4),1,0)
#Work Experience Program for current job Q47 (0=19235 / 1=1516)
WEcurrent<-ifelse((DEM$wecurjo==3 | DEM$wecurjo==4),1,0)

          DEMfinal<-data.frame(CLcurrent, CTcurrent, WEcurrent, NoCredential, Education, DEM, Tally=rep(1, dim(DEM)[1])); dim(DEMfinal)
      DEMfinal$STW<-ordered(DEMfinal$STW, levels=c("STW","NonSTW")) 
DEMfinal$Education<-ordered(DEMfinal$Education, levels=c("High School or Less","Some College","Associates Degree"))

#Datasets where the credential is for the job
CL<-DEMfinal[DEMfinal$cnmain==1 & DEMfinal$CLcurrent>0, ];dim(CL) #3549
#View(CL)
CT<-DEMfinal[DEMfinal$certprog==1 & DEMfinal$CTcurrent>0, ];dim(CT) #1920
#View(CT)
WE<-DEMfinal[DEMfinal$weprog==1 & DEMfinal$WEcurrent>0, ];dim(WE) #1574
#View(WE)
ForJob<-data.frame(Credential=rep(c("CL","CT","WE"),c(3549,1920,1574)),
                   Education=c(as.character(CL[,5]), as.character(CT[,5]), as.character(WE[,5])),
                   STW=c(as.character(CL[,10]), as.character(CT[,10]), as.character(WE[,10])),
                   Tally=c(CL[,11], CT[,11], WE[,11]), 
                   rbind(CL[,c(344:359,362)], CT[,c(344:359,362)], WE[,c(344:359,362)]))
ForJob$Education<-ordered(ForJob$Education, levels=c("High School or Less","Some College","Associates Degree"))
      ForJob$STW<-ordered(ForJob$STW, levels=c("STW","NonSTW"))

#Datasets where the credential is NOT for the job
CLnfj<-DEMfinal[DEMfinal$cnmain==1 & DEMfinal$CLcurrent==0, ];dim(CLnfj) #1429
#View(CLnfj)
CTnfj<-DEMfinal[DEMfinal$certprog==1 & DEMfinal$CTcurrent==0, ];dim(CTnfj) #2941
#View(CTnfj)
WEnfj<-DEMfinal[FINALjob$weprog==1 & FINALjob$WEcurrent==0, ];dim(WEnfj) #1415
#View(WEnfj)
NotForJob<-data.frame(Credential=rep(c("CL","CT","WE"),c(1429,2941,1415)),
                      Education=c(as.character(CLnfj[,5]), as.character(CTnfj[,5]), as.character(WEnfj[,5])),
                      STW=c(as.character(CLnfj[,10]), as.character(CTnfj[,10]), as.character(WEnfj[,10])),
                      Tally=c(CLnfj[,11], CTnfj[,11], WEnfj[,11]),
                      rbind(CLnfj[,c(344:359,362)], CTnfj[,c(344:359,362)], WEnfj[,c(344:359,362)]))
NotForJob$Education<-ordered(NotForJob$Education, levels=c("High School or Less","Some College","Associates Degree"))
       NotForJob$STW<-ordered(NotForJob$STW, levels=c("STW","NonSTW"))



