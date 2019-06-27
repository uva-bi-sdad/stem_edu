#Look at everyone Associate's Degree (AD) and Less
#Responded to EDUATTN - Highest Degree or level of school completion
#Usefulness of credential for those responders whose credential was used to get their current job
#Aggregate by Credential and education and whether the job is STW or notSTW
##################################################################################################################
library(readxl)
library(ggplot2)
library(sjPlot)
library(sjmisc)

#Color Blind Palettes
#color blind palette with grey
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#color blind palette with black
cbPalette.blk<-c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)

load("~/git/stem_edu/data/stem_edu/original/ATES/ates_pu_pert.rdata")
setwd("~/git/stem_edu/src/VAL")
ATES<-ates_pu_pert; remove(ates_pu_pert); table(ATES$eduatt)
#Read in Census Occupation Codes, descriptions, and STW designation
COC<-read_excel("~/git/stem_edu/data/stem_edu/original/COC_STW.xls")
PATH<-merge(ATES, COC, by.x="empocc", by.y="Code", all.x=TRUE, all.y=FALSE)

#Separate AD and less (AD=29223)
AD<-PATH[PATH$eduattn<7,c(1:97,355:359)]; dim(AD); dim(AD)[1]/dim(ATES)[1]; table(ATES$eduattn); table(AD$eduattn)

#Separate out those whose credential is for their current job
#Recode the variable credential for current job
#Create a variable that states whether the credential is for the current job
#Credential for current job (5458)
#Primary Certification/license for current job Q14
AD$CNCURRJOB1<-ifelse(AD$CNCURRJOB1==3,1,0)
#Second Certification/license for current job Q23
AD$CNCURRJOB2<-ifelse(AD$CNCURRJOB2==3,1,0)
#Certificate for current job Q37
AD$lccurrjob<-ifelse((AD$lccurrjob==3 | AD$lccurrjob==4),1,0)
#Work Experience Program for current job Q47
AD$wecurjo<-ifelse((AD$wecurjo==3 | AD$wecurjo==4),1,0)
#Create the new variable
CurrentJob<-(AD$CNCURRJOB1+AD$CNCURRJOB2+AD$lccurrjob+AD$wecurjo)
#Keep those responders whose credential is for their current job (5458)
ADcj<-AD[CurrentJob>0,]

#Create a new education variable education level for those whose credential is for their current job
#Associate Degree=1505 /  Some College=2424 / High School or Less=1529
Education<-ifelse(ADcj$eduattn==6, "Associates Degree",
                  ifelse((ADcj$eduattn==5 | ADcj$eduattn==4), "Some College", "High School or Less"))

ADcj<-data.frame(ADcj,Education=Education)

#########################################################################################

#Create a data set for Certifications/Licenses (n=3646)
CandL<-ADcj[(ADcj$cnmain==1 | ADcj$CNMAIN2==1 | ADcj$CNMAIN3==1), c(22:25,37:40,98:103)]; dim(CandL)
CandL<-CandL[,-c(5:8)]
  names(CandL)<-c("GET","KEEP","MARKETABLE","SKILLS","STWPercent","Occupations","MOG","Description","STW","Education")

CandLuse<-rbind(data.frame(Question=rep("Get",24), data.frame(table(CandL$GET, CandL$STW, CandL$Education))),
                data.frame(Question=rep("Keep",24), data.frame(table(CandL$KEEP, CandL$STW, CandL$Education))),
                data.frame(Question=rep("Marketable",24), data.frame(table(CandL$MARKETABLE, CandL$STW, CandL$Education))),
                data.frame(Question=rep("Skills",24), data.frame(table(CandL$SKILLS, CandL$STW, CandL$Education))))

CandLuse<-CandLuse[order(CandLuse[,4], CandLuse[,1], CandLuse[,3], CandLuse[,2]), ]
  names(CandLuse)<-c("Questions","Response","STW","Education","Frequency")
  den<-rep(aggregate(Frequency~STW+Questions+Education, data=CandLuse, sum)[,4], rep(4, 24))
  Category<-rep(rep(c("NonSTW: getting job","STW: getting job","NonSTW: keeping job","STW: keeping job",
                      "NonSTW: staying marketability","STW: staying marketability",
                      "NonSTW: improving skills","STW: improving skills"), rep(4, 8)), 3)
  Answer<-rep(c("Not","Somewhat","Very","Too soon"), 24)
  CandLuse<-data.frame(CandLuse, Denominator=den, Percent=round((CandLuse$Frequency/den)*100,0), Category=Category, Answer=Answer)
  CandLuse$Answer<-ordered(CandLuse$Answer, levels=c("Not","Somewhat","Very","Too soon"))
  CandLuse$Category<-ordered(CandLuse$Category, levels=c("NonSTW: getting job","STW: getting job",
                                                       "NonSTW: keeping job", "STW: keeping job",
                                                       "NonSTW: staying marketability", "STW: staying marketability",
                                                       "NonSTW: improving skills", "STW: improving skills"))

#Create a stacked bar chart centered at zero
#https://stackoverflow.com/questions/19505889/creating-a-stacked-bar-chart-centered-on-zero-using-ggplot

#Certification & License / Associates Degree
 CandLuse.ad<-CandLuse[(CandLuse$Response!=4 & CandLuse$Education=="Associates Degree"),]
  CandL.adup<-CandLuse.ad[CandLuse.ad$Response==3, ]
CandL.addown<-CandLuse.ad[CandLuse.ad$Response!=3, ]; CandL.addown$Percent<-CandL.addown$Percent*-1

ggplot() +
    geom_bar(data=CandL.adup, aes(y=Percent, x=Category, fill=Answer),
             color=cbPalette[1], stat="identity", width=0.75) +
    geom_bar(data=CandL.addown, aes(y=Percent, x=Category, fill=Answer),
             color=cbPalette[1], stat="identity", width=0.75) +
    xlab("") + ylab("") +
    scale_fill_manual(values=cbPalette[c(6,3,2)],
                      labels=c(" Not Useful   ", " Somewhat Useful   ", " Very Useful ")) +
    scale_y_continuous(limits=c(-95, 95), breaks=seq(from=-90, to=90, by=10),
                       labels=c(90,80,70,60,50,40,30,20,10,0,10,20,30,40,50,60,70,80,90)) +
    coord_flip() +
    theme(axis.text.y=element_text(size=15),
          axis.text.x=element_text(size=15, hjust=0.5),
          legend.title=element_blank(),
          legend.text=element_text(size=15)) +
    geom_hline(yintercept=0, lwd=1, lty=3) +
    annotate("text", x=8.05, y=-61, size=6, label="Associates\nDegree") +
    theme_val()

#Certification & License / Some College
 CandLuse.sc<-CandLuse[(CandLuse$Response!=4 & CandLuse$Education=="Some College"),]
  CandL.scup<-CandLuse.sc[CandLuse.sc$Response==3, ]
CandL.scdown<-CandLuse.sc[CandLuse.sc$Response!=3, ]; CandL.scdown$Percent<-CandL.scdown$Percent*-1

ggplot() +
    geom_bar(data=CandL.scup, aes(y=Percent, x=Category, fill=Answer),
             color=cbPalette[1], stat="identity", width=0.75) +
    geom_bar(data=CandL.scdown, aes(y=Percent, x=Category, fill=Answer),
             color=cbPalette[1], stat="identity", width=0.75) +
    xlab("") + ylab("") +
    scale_fill_manual(values=cbPalette[c(6,3,2)],
                      labels=c(" Not Useful   ", " Somewhat Useful   ", " Very Useful ")) +
    scale_y_continuous(limits=c(-95, 95), breaks=seq(from=-90, to=90, by=10),
                       labels=c(90,80,70,60,50,40,30,20,10,0,10,20,30,40,50,60,70,80,90)) +
    coord_flip() +
    theme(axis.text.y=element_text(size=15),
          axis.text.x=element_text(size=15, hjust=0.5),
          legend.title=element_blank(),
          legend.text=element_text(size=15)) +
    geom_hline(yintercept=0, lwd=1, lty=3) +
    annotate("text", x=8.3, y=-61, size=6, label="Some College") +
    theme_val()

#Certification & License / high School or Less
 CandLuse.hs<-CandLuse[(CandLuse$Response!=4 & CandLuse$Education=="High School or Less"),]
  CandL.hsup<-CandLuse.hs[CandLuse.hs$Response==3, ]
CandL.hsdown<-CandLuse.hs[CandLuse.hs$Response!=3, ]; CandL.hsdown$Percent<-CandL.hsdown$Percent*-1

ggplot() +
  geom_bar(data=CandL.hsup, aes(y=Percent, x=Category, fill=Answer),
           color=cbPalette[1], stat="identity", width=0.75) +
  geom_bar(data=CandL.hsdown, aes(y=Percent, x=Category, fill=Answer),
           color=cbPalette[1], stat="identity", width=0.75) +
  xlab("") + ylab("") +
  scale_fill_manual(values=cbPalette[c(6,3,2)],
                    labels=c(" Not Useful   ", " Somewhat Useful   ", " Very Useful ")) +
  scale_y_continuous(limits=c(-95, 95), breaks=seq(from=-90, to=90, by=10),
                     labels=c(90,80,70,60,50,40,30,20,10,0,10,20,30,40,50,60,70,80,90)) +
  coord_flip() +
  theme(axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15, hjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=15)) +
  geom_hline(yintercept=0, lwd=1, lty=3) +
  annotate("text", x=8.05, y=-61, size=6, label="High School\nor Less") +
  theme_val()


#########################################################################################

#Create a data set for Work Experimence Programs (n=1767) that has the usefullness variables
WEP<-ADcj[ADcj$weprog==3,c(77:79,98:103)]; dim(WEP)
  names(WEP)<-c("GET","PAY","SKILLS","STWPercent","Occupations","MOG","Description","STW","Education")

WEPuse<-rbind(data.frame(Question=rep("Get",24), data.frame(table(WEP$GET, WEP$STW, WEP$Education))),
              data.frame(Question=rep("Pay",24), data.frame(table(WEP$PAY, WEP$STW, WEP$Education))),
              data.frame(Question=rep("Skills",24), data.frame(table(WEP$SKILLS, WEP$STW, WEP$Education))))

WEPuse<-WEPuse[order(WEPuse[,4], WEPuse[,1], WEPuse[,3], WEPuse[,2]), ]
  names(WEPuse)<-c("Questions","Response","STW","Education","Frequency")
  den<-rep(aggregate(Frequency~STW+Questions+Education, data=WEPuse, sum)[,4], rep(4, 18))
  Category<-rep(rep(c("NonSTW: getting job","STW: getting job","NonSTW: increasing pay","STW: increasing pay",
                      "NonSTW: improving skills","STW: improving skills"), rep(4, 6)), 3)
  Answer<-rep(c("Not","Somewhat","Very","Too soon"), 18)
WEPuse<-data.frame(WEPuse, Denominator=den, Percent=round((WEPuse$Frequency/den)*100,0), Category=Category, Answer=Answer)
  WEPuse$Answer<-ordered(WEPuse$Answer, levels=c("Not","Somewhat","Very","Too soon"))
  WEPuse$Category<-ordered(WEPuse$Category, levels=c("NonSTW: getting job","STW: getting job",
                                                     "NonSTW: increasing pay", "STW: increasing pay",
                                                     "NonSTW: improving skills", "STW: improving skills"))

#Create a stacked bar chart centered at zero
#https://stackoverflow.com/questions/19505889/creating-a-stacked-bar-chart-centered-on-zero-using-ggplot

#Work Experience Program / Associates Degree
      WEP.ad<-WEPuse[(WEPuse$Response!=4 & WEPuse$Education=="Associates Degree"),]
    WEP.adup<-WEP.ad[WEP.ad$Response==3, ]
  WEP.addown<-WEP.ad[WEP.ad$Response!=3, ]; WEP.addown$Percent<-WEP.addown$Percent*-1

ggplot() +
  geom_bar(data=WEP.adup, aes(y=Percent, x=Category, fill=Answer),
           color=cbPalette[1], stat="identity", width=0.75) +
  geom_bar(data=WEP.addown, aes(y=Percent, x=Category, fill=Answer),
           color=cbPalette[1], stat="identity", width=0.75) +
  xlab("") + ylab("") +
  scale_fill_manual(values=cbPalette[c(6,3,2)],
                    labels=c(" Not Useful   ", " Somewhat Useful   ", " Very Useful ")) +
    scale_y_continuous(limits=c(-85, 85), breaks=seq(from=-80, to=80, by=10),
                     labels=c(80,70,60,50,40,30,20,10,0,10,20,30,40,50,60,70,80)) +
  coord_flip() +
  theme(axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15, hjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=15)) +
  geom_hline(yintercept=0, lwd=1, lty=3) +
  annotate("text", x=6.05, y=-61, size=6, label="Associates\nDegree") +
  theme_val()

#Work Experience Program / Some College
    WEP.sc<-WEPuse[(WEPuse$Response!=4 & WEPuse$Education=="Some College"),]
  WEP.scup<-WEP.sc[WEP.sc$Response==3, ]
WEP.scdown<-WEP.sc[WEP.sc$Response!=3, ]; WEP.scdown$Percent<-WEP.scdown$Percent*-1

ggplot() +
  geom_bar(data=WEP.scup, aes(y=Percent, x=Category, fill=Answer),
           color=cbPalette[1], stat = "identity", width=0.75) +
  geom_bar(data=WEP.scdown, aes(y=Percent, x=Category, fill=Answer),
           color=cbPalette[1], stat = "identity", width=0.75) +
  xlab("") + ylab("") +
  scale_fill_manual(values=cbPalette[c(6,3,2)],
                    labels=c(" Not Useful   ", " Somewhat Useful   ", " Very Useful ")) +
  scale_y_continuous(limits=c(-85, 85), breaks=seq(from=-80, to=80, by=10),
                     labels=c(80,70,60,50,40,30,20,10,0,10,20,30,40,50,60,70,80)) +
  coord_flip() +
  theme(axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15, hjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=15)) +
  geom_hline(yintercept=0, lwd=1, lty=3) +
  annotate("text", x=6.4, y=-61, size=6, label="Some College") +
  theme_val()

#Work Experience Program / High School or Less
    WEP.hs<-WEPuse[(WEPuse$Response!=4 & WEPuse$Education=="High School or Less"),]
  WEP.hsup<-WEP.hs[WEP.hs$Response==3, ]
WEP.hsdown<-WEP.hs[WEP.hs$Response!=3, ]; WEP.hsdown$Percent<-WEP.hsdown$Percent*-1

ggplot() +
  geom_bar(data=WEP.hsup, aes(y=Percent, x=Category, fill=Answer),
           color=cbPalette[1], stat = "identity", width=0.75) +
  geom_bar(data=WEP.hsdown, aes(y=Percent, x=Category, fill=Answer),
           color=cbPalette[1], stat = "identity", width=0.75) +
  xlab("") + ylab("") +
  scale_fill_manual(values=cbPalette[c(6,3,2)],
                    labels=c(" Not Useful   ", " Somewhat Useful   ", " Very Useful ")) +
  scale_y_continuous(limits=c(-85, 85), breaks=seq(from=-80, to=80, by=10),
                     labels=c(80,70,60,50,40,30,20,10,0,10,20,30,40,50,60,70,80)) +
  coord_flip() +
  theme(axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15, hjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=15)) +
  geom_hline(yintercept=0, lwd=1, lty=3) +
  annotate("text", x=6.05, y=-61, size=6, label="High School\nor Less") +
  theme_val()





