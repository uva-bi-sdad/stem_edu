library(readxl)
library(ggplot2)
#Color blind palette with grey
cbPalette<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#FILE SOME COLLEGE but NO DEGREE
load("~/git/stem_edu/data/stem_edu/original/ATES/ates_pu_pert.rdata")
setwd("~/git/stem_edu/src/VAL")
ATES<-ates_pu_pert; remove(ates_pu_pert); table(ATES$eduatt)
#Read in Census Occupation Codes, descriptions, and STW designation
COC<-read_excel("~/git/stem_edu/data/stem_edu/original/COC_STW.xls")
COC$STW<-ifelse(COC$STW==0,"NonSTW","STW");COC$STW<-as.factor(COC$STW)
rm(CL, PSC, WE)

############################################################################################
#Create the associates degree data set
#associates degree - 4,484 / 9.39% of the survey
AD<-ATES[ATES$eduattn==6,]; dim(AD); dim(AD)[1]/dim(ATES)[1]

#Recode the kinds of work that need to be split between two major occupation groups
#CNFIELD1 4 and 19
T4<-which(AD$CNFIELD1==4);T4no<-ceiling(length(T4)/2);
AD$CNFIELD1[T4[1:T4no]]<--4
T19<-which(AD$CNFIELD1==19);T19no<-ceiling(length(T19)/2);
AD$CNFIELD1[T19[1:T19no]]<--19;rm(T4,T4no,T19,T19no)
#psfos 3
T3<-which(AD$psfos==3);T3no<-ceiling(length(T3)/2)
AD$psfos[T3[1:T3no]]<--3;rm(T3,T3no)
#wefolp 7
T7<-which(AD$wefolp==7);T7no<-ceiling(length(T7)/2)
AD$wefolp[T7[1:T7no]]<--7;rm(T7,T7no)

#Assign the kinds of work for a credential into major occupation categories
#Q9. Occupation for most important certifications/licenses
ADQ9<-ifelse(AD$CNFIELD1==24, "49-0000",
       ifelse(AD$CNFIELD1==23, "47-0000",
        ifelse(AD$CNFIELD1==10, "29-0000",
         ifelse(AD$CNFIELD1==11, "29-0000",
          ifelse(AD$CNFIELD1==1, "17-0000",
           ifelse(AD$CNFIELD1==2, "17-0000",
            ifelse(AD$CNFIELD1==3, "15-0000",
             ifelse(AD$CNFIELD1==4, "15-0000",
              ifelse(AD$CNFIELD1==-4, "19-0000",
               ifelse(AD$CNFIELD1==19, "45-0000",
                ifelse(AD$CNFIELD1==-19, "19-0000",
                 ifelse(AD$CNFIELD1==17, "33-0000",
                  ifelse(AD$CNFIELD1==5, "<5% STW MOG",
                   ifelse(AD$CNFIELD1==6, "<5% STW MOG",
                    ifelse(AD$CNFIELD1==7, "<5% STW MOG",
                     ifelse(AD$CNFIELD1==8, "<5% STW MOG",
                      ifelse(AD$CNFIELD1==16, "<5% STW MOG",
                       ifelse(AD$CNFIELD1==25, "<5% STW MOG",
                        ifelse(AD$CNFIELD1==18, "nonSTW MOG",
                         ifelse(AD$CNFIELD1==20, "nonSTW MOG",
                          ifelse(AD$CNFIELD1==21, "nonSTW MOG",
                           ifelse(AD$CNFIELD1==22, "nonSTW MOG",
                            ifelse(AD$CNFIELD1==9, "nonSTW MOG",
                             ifelse(AD$CNFIELD1==12, "nonSTW MOG",
                              ifelse(AD$CNFIELD1==13, "nonSTW MOG",
                               ifelse(AD$CNFIELD1==14, "nonSTW MOG",
                                ifelse(AD$CNFIELD1==15, "nonSTW MOG",
                                 ifelse(AD$CNFIELD1==26, "OTHER",
                                   ifelse(AD$CNFIELD1==27, "OTHER",
                                    ifelse(AD$CNFIELD1==-8, "Uncodeable",
                                     ifelse(AD$CNFIELD1==-9, "Missing","Valid Skip")))))))))))))))))))))))))))))))
AD$CNFIELD1<-ADQ9
#Q30d. Number of certificates from a community or technical college or other school after HS
ADQ31d<-ifelse(AD$psfos==19, "49-0000",
         ifelse(AD$psfos==7, "47-0000",
          ifelse(AD$psfos==14, "29-0000",
           ifelse(AD$psfos==11, "17-0000",
            ifelse(AD$psfos==18, "51-0000",
             ifelse(AD$psfos==6, "15-0000",
              ifelse(AD$psfos==4, "27-0000",
               ifelse(AD$psfos==3, "45-0000",
                ifelse(AD$psfos==-3, "19-0000",
                 ifelse(AD$psfos==15, "33-0000",
                  ifelse(AD$psfos==5, "<5% STW MOG",
                   ifelse(AD$psfos==1, "<5% STW MOG",
                    ifelse(AD$psfos==16, "<5% STW MOG",
                     ifelse(AD$psfos==9, "<5% STW MOG",
                      ifelse(AD$psfos==20, "<5% STW MOG",
                       ifelse(AD$psfos==2, "<5% STW MOG",
                        ifelse(AD$psfos==10, "0% STW MOG",
                         ifelse(AD$psfos==8, "0% STW MOG",
                          ifelse(AD$psfos==13, "0% STW MOG",
                           ifelse(AD$psfos==12, "0% STW MOG",
                            ifelse(AD$psfos==17, "0% STW MOG",
                             ifelse(AD$psfos==21, "OTHER","Valid Skip"))))))))))))))))))))))
AD$psfos<-ADQ31d
#Q40. Occupation for most work experience programs
ADQ40<-ifelse(AD$wefolp==24, "49-0000",
        ifelse(AD$wefolp==20, "49-0000",
         ifelse(AD$wefolp==1, "47-0000",
          ifelse(AD$wefolp==2, "47-0000",
           ifelse(AD$wefolp==3, "47-0000",
            ifelse(AD$wefolp==4, "47-0000",
             ifelse(AD$wefolp==5, "47-0000",
              ifelse(AD$wefolp==6, "29-0000",
               ifelse(AD$wefolp==7, "29-0000",
                ifelse(AD$wefolp==-7, "31-0000",
                 ifelse(AD$wefolp==14, "17-0000",
                  ifelse(AD$wefolp==18, "51-0000",
                   ifelse(AD$wefolp==21, "51-0000",
                    ifelse(AD$wefolp==11, "15-0000",
                     ifelse(AD$wefolp==26, "27-0000",
                      ifelse(AD$wefolp==16, "33-0000",
                       ifelse(AD$wefolp==19, "<5% STW MOG",
                        ifelse(AD$wefolp==9, "<5% STW MOG",
                         ifelse(AD$wefolp==17, "<5% STW MOG",
                          ifelse(AD$wefolp==10, "<5% STW MOG",
                           ifelse(AD$wefolp==13, "<5% STW MOG",
                            ifelse(AD$wefolp==22, "0% STW MOG",
                             ifelse(AD$wefolp==23, "0% STW MOG",
                              ifelse(AD$wefolp==8, "0% STW MOG",
                               ifelse(AD$wefolp==12, "0% STW MOG",
                                ifelse(AD$wefolp==15, "0% STW MOG",
                                 ifelse(AD$wefolp==25, "OTHER","Valid Skip")))))))))))))))))))))))))))
AD$wefolp<-ADQ40

#Recode Earnings over the last 12 months Q62
ADQ62<-ifelse(AD$eeearn==1,5000,ifelse(AD$eeearn==2,15000,ifelse(AD$eeearn==3,25000,ifelse(AD$eeearn==4,35000,
         ifelse(AD$eeearn==5,45000,ifelse(AD$eeearn==6,55000,ifelse(AD$eeearn==7,67500,ifelse(AD$eeearn==8,112500,
          ifelse(AD$eeearn==9,175000,NA)))))))))
AD$eeearn<-ADQ62

#Assign the Census occupation codes a description and STW status
#Q66. What kind of work were you doing?
#Merge with the STW/NonSTW by Census Occupation Codes
AD<-merge(AD, COC, by.x="empocc", by.y="COC", all.x=TRUE, all.y=FALSE)



###########################################################################
#Analyses for certification/license
#Select those that have a current certification/license
T1<-AD[AD$cnmain==1,]; dim(T1)  #1383
#Recode preparation Question 13 A, B, C
PREPARE<-ifelse(T1$CNPRP_COLLG1==2 & T1$CNPRP_TRAIN1!=2 & T1$CNPRP_ONOWN1!=2, "School",
          ifelse(T1$CNPRP_COLLG1!=2 & T1$CNPRP_TRAIN1==2 & T1$CNPRP_ONOWN1!=2, "Training",
           ifelse(T1$CNPRP_COLLG1!=2 & T1$CNPRP_TRAIN1!=2 & T1$CNPRP_ONOWN1==2, "Self-Study",
            ifelse(T1$CNPRP_COLLG1==2 & T1$CNPRP_TRAIN1==2 & T1$CNPRP_ONOWN1!=2, "School/Training",
             ifelse(T1$CNPRP_COLLG1==2 & T1$CNPRP_TRAIN1!=2 & T1$CNPRP_ONOWN1==2, "School/Self-Study",
              ifelse(T1$CNPRP_COLLG1!=2 & T1$CNPRP_TRAIN1==2 & T1$CNPRP_ONOWN1==2, "Training/Self-Study","ALL"))))))
CL<-data.frame(T1[,c(12)],PREPARE,T1[,c(1,355,356,22,23,24,25,92,21,106)]); rm(T1)
names(CL)<-c("MOG","PREPARE","COC","DESCRIPTION","STW","GET","KEEP","MARKET","SKILLS","EARN","CURRENTJOB","AGE") #1383
#Remove MOGs that are "Missing" (30) or "Uncodeable" (3)
temp<-CL[CL$MOG!="Missing", ]; temp<-temp[temp$MOG!="Uncodeable", ]; temp$MOG<-factor(temp$MOG); CL<-temp
#Order by Rothwell percent STW
CL$MOG<-ordered(CL$MOG, levels=rev(c("49-0000","47-0000","29-0000","17-0000","51-0000","15-0000",
                                     "27-0000","45-0000","33-0000","19-0000","<5% STW MOG","0% STW MOG","OTHER")),
                        labels=rev(c("49-0000 (89.2%)","47-0000 (56.1%)","29-0000 (42.5%)","17-0000 (25.6%)","51-0000 (25.5%)","15-0000 (21.1%)",
                                     "27-0000 (11.7%)","45-0000 (11.1%)","33-0000 (9.4%)","19-0000 (5.6%)","MOGs (<5.0%)","MOGs (0.0%)","OTHER")))
CL$PREPARE<-ordered(CL$PREPARE, levels=c("School","Training","Self-Study","School/Training","School/Self-Study","Training/Self-Study","All"))
CL$CURRENTJOB<-ifelse(CL$CURRENTJOB==3,"YES",ifelse(CL$CURRENTJOB==1,"Not Working","NO"))  #1350

#Plot for Preparation by STW
PS<-data.frame(table(CL$STW, CL$PREPARE));names(PS)<-c("STW","Preparation","Percent")
PS<-PS[PS$Percent>0,];TOTAL<-rep(c(-sum(PS$Percent[PS$STW=="NonSTW"]),sum(PS$Percent[PS$STW=="STW"])),6)
PS$Percent<-(PS$Percent/TOTAL)*100
brks<-seq(-30, 30, 5)
lbls<-as.character(c("30%","","20%","","10%","","0%","","10%","","20%","","30%"))
ggplot(PS, aes(x=Preparation, y=Percent, fill=STW)) +   # Fill column
  geom_bar(stat="identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks=brks,   # Breaks
                     labels=lbls) + # Labels
  coord_flip() +  # Flip axes
  labs(title="Preparation for Certification/License",
       subtitle="NonSTW       STW       ") +
  theme_val() +
  theme(plot.title=element_text(hjust=.5, size=17),
        plot.subtitle=element_text(hjust=.5, size=15, vjust=-2, colour="#2a2a2b"),
        legend.position="none",
        axis.text=element_text(size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_manual(values=cbPalette)  # Color palette


#Plot STW by the Four How Useful Questions
USE<-rbind(data.frame(Question=rep("Keep",8),table(CL$KEEP,CL$STW)),
           data.frame(Question=rep("Get",8),table(CL$GET,CL$STW)),
           data.frame(Question=rep("Market",8),table(CL$MARKET,CL$STW)),
           data.frame(Question=rep("Skills",8),table(CL$SKILLS,CL$STW)))
TOTAL<-c(rep(sum(USE$Freq[1:4]),4),rep(sum(USE$Freq[5:8]),4),rep(sum(USE$Freq[9:12]),4),
         rep(sum(USE$Freq[13:16]),4),rep(sum(USE$Freq[17:20]),4),rep(sum(USE$Freq[21:24]),4),
         rep(sum(USE$Freq[25:28]),4),rep(sum(USE$Freq[29:32]),4))
USE<-data.frame(USE,TOTAL=TOTAL,Percent=round((USE$Freq/TOTAL)*100,0),
                Category=rep(c("NonSTW: Keeping job","STW: Keeping job","NonSTW: Getting job","STW: Getting job",
                               "NonSTW: Keeping marketable ","STW: Keeping marketable","NonSTW: Improving skills","STW: Improving skills"),rep(4,8)),
                Answer=rep(c("Not","Somewhat","Very","Too soon"),8))
names(USE)<-c("Questions","Response","STW","Frequency","Total","Percent","Category","Answer")
USE$Answer<-ordered(USE$Answer, levels=c("Not","Somewhat","Very","Too soon"))
USE$Category<-ordered(USE$Category, levels=c("NonSTW: Keeping job","STW: Keeping job","NonSTW: Getting job",
                                             "STW: Getting job","NonSTW: Keeping marketable ",
                                             "STW: Keeping marketable","NonSTW: Improving skills",
                                             "STW: Improving skills"))

p<-ggplot(USE, aes(Answer, Category)) +
  geom_tile(aes(fill=Percent), colour="white", lwd=2) +
  #  scale_fill_gradient(low="white", high="steelblue")
  scale_fill_gradient(low="ghostwhite", high="steelblue", limits=c(0,90))

ADUSE1<-p + theme_grey() + labs(x="", y="") + scale_x_discrete(expand=c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title="Associate's Degree: Usefulness of Certification/License",
       subtitle="Percent usefulness by skilled technical workforce (STW=793/NonSTW=526).") +
  labs(title="",
       subtitle="Associate's Degree\n(STW=793/NonSTW=526)") +
  annotate("text", x=c(rep(c(1,2,3,4),8)),
           y=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4)),
           size=7, label=c(USE$Percent), colour="#2a2a2b") +
  coord_fixed(ratio=0.75) +
  theme(legend.position="none",
        plot.title=element_text(hjust=.5, size=17, colour="#2a2a2b"),
        plot.subtitle=element_text(hjust=0.5),
        axis.ticks=element_blank(),
        axis.title.x=element_text(size=20, hjust=0.5, vjust=-1, colour="#2a2a2b"),
        axis.text.x=element_text(size=15, hjust=1.0, angle=45, colour="#2a2a2b"),
        axis.text.y=element_text(size=15, hjust=1.0, colour="#2a2a2b"))
ADUSE1
ggsave("ADUSE1.pdf", width=6, height=6)

rm(USE)

###########################################################################
#Analyses for post-secondary credential
#Select those that have a post-secondary credential
T1<-AD[AD$certprog==1,]; dim(T1)  #995
#Recode preparation Question 32
PREPARE<-ifelse(T1$lastpscer==1,"College/University","Someplace else")
PSC<-data.frame(T1[,c(51)],PREPARE,T1[,c(1,355,356,59,60,61,53,58,106,92)]); rm(T1)
names(PSC)<-c("MOG","PREPARE","COC","DESCRIPTION","STW","JOB","PAY","SKILLS","HOURS","CURRENTJOB","AGE","EARN") #1440
PSC$MOG<-ordered(PSC$MOG, levels=rev(c("49-0000","47-0000","29-0000","17-0000","51-0000","15-0000",
                                      "27-0000","45-0000","33-0000","19-0000","<5% STW MOG","0% STW MOG","OTHER")),
                          labels=rev(c("49-0000 (89.2%)","47-0000 (56.1%)","29-0000 (42.5%)","17-0000 (25.6%)","51-0000 (25.5%)","15-0000 (21.1%)",
                                       "27-0000 (11.7%)","45-0000 (11.1%)","33-0000 (9.4%)","19-0000 (5.6%)","MOGs (<5.0%)","MOGs (0.0%)","OTHER")))
PSC$PREPARE<-ordered(PSC$PREPARE, levels=c("College/University","Someplace else"))
PSC$CURRENTJOB<-ifelse(PSC$CURRENTJOB==1,"Not Working", ifelse(PSC$CURRENTJOB==2,"NO", ifelse(PSC$CURRENTJOB==-1,"Valid Skip", "YES")))
table(PSC$STW)

#Plot for Preparation by STW
PS<-data.frame(table(PSC$STW, PSC$PREPARE));names(PS)<-c("STW","Preparation","Percent")
  TOTAL<-rep(c(-sum(PS$Percent[PS$STW=="NonSTW"]),sum(PS$Percent[PS$STW=="STW"])),2)
  PS$Percent<-(PS$Percent/TOTAL)*100
brks<-seq(-80, 80, 10)
lbls<-as.character(c("80%","","60%","","40%","","20%","","0%","","20%","","40%","","60%","","80%"))
ggplot(PS, aes(x=Preparation, y=Percent, fill=STW)) +   # Fill column
  geom_bar(stat="identity", width=.6) +   # draw the bars
  scale_y_continuous(breaks=brks,   # Breaks
                     labels=lbls) + # Labels
  coord_flip() +  # Flip axes
  labs(title="Preparation for Post-secondary Credential",
       subtitle="NonSTW       STW       ") +
  theme_val() +
  theme(plot.title=element_text(hjust=.5, size=17),
        plot.subtitle=element_text(hjust=.5, size=15, vjust=-2, colour="#2a2a2b"),
        legend.position="none",
        axis.text=element_text(size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_manual(values=cbPalette)  # Color palette

#Plot STW by the Four How Useful Questions
USE<-rbind(data.frame(Question=rep("Job",8),table(PSC$JOB,PSC$STW)),
           data.frame(Question=rep("Pay",8),table(PSC$PAY,PSC$STW)),
           data.frame(Question=rep("Skills",8),table(PSC$SKILLS,PSC$STW)))
TOTAL<-c(rep(sum(USE$Freq[1:4]),4),rep(sum(USE$Freq[5:8]),4),rep(sum(USE$Freq[9:12]),4),
         rep(sum(USE$Freq[13:16]),4),rep(sum(USE$Freq[17:20]),4),rep(sum(USE$Freq[21:24]),4))
USE<-data.frame(USE,TOTAL=TOTAL,Percent=round((USE$Freq/TOTAL)*100,0),
                Category=rep(c("NonSTW: Getting job","STW: Getting job","NonSTW: Increasing pay",
                               "STW:Increasing pay","NonSTW: Improving skills","STW: Improving skills"),rep(4,6)),
                Answer=rep(c("Not","Somewhat","Very","Too soon"),6))
names(USE)<-c("Questions","Response","STW","Frequency","Total","Percent","Category","Answer")
USE$Answer<-ordered(USE$Answer, levels=c("Not","Somewhat","Very","Too soon"))
USE$Category<-ordered(USE$Category, levels=c(c("NonSTW: Getting job","STW: Getting job",
                                               "NonSTW: Increasing pay","STW:Increasing pay",
                                               "NonSTW: Improving skills","STW: Improving skills")))

p<-ggplot(USE, aes(Answer, Category)) +
  geom_tile(aes(fill=Percent), colour="white", lwd=2) +
  #  scale_fill_gradient(low="white", high="steelblue")
  scale_fill_gradient(low="ghostwhite", high="steelblue", limits=c(0,90))

ADUSE2<-p + theme_grey() + labs(x="", y="") + scale_x_discrete(expand=c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
#  labs(title="Associate's Degree: Usefulness of Post-secondary Credential",
#       subtitle="       Percent usefulness by skilled technical workforce (STW=360/NonSTW=565).") +
  labs(title="",
       subtitle="Associate's Degree\n(STW=360/NonSTW=565)") +
  annotate("text", x=c(rep(c(1,2,3,4),6)),
           y=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4)),
           size=7, label=c(USE$Percent), colour="#2a2a2b") +
  coord_fixed(ratio=0.75) +
  theme(legend.position="none",
        plot.title=element_text(hjust=.5, size=17, colour="#2a2a2b"),
        plot.subtitle=element_text(hjust=0.5),
        axis.ticks=element_blank(),
        axis.title.x=element_text(size=20, hjust=0.5, vjust=-1, colour="#2a2a2b"),
        axis.text.x=element_text(size=15, hjust=1.0, angle=45, colour="#2a2a2b"),
        axis.text.y=element_text(size=15, hjust=1.0, colour="#2a2a2b"))
ADUSE2
ggsave("ADUSE2.pdf", width=6, height=6)

rm(USE)

###########################################################################
#Analyses for work experience credential
#Select those that have a work experimence credential
T1<-AD[AD$weprog==3,]; dim(T1)  #1060
#Recode preparation Question 43
PREPARE<-ifelse(T1$WEPRP_INSTR==1 & T1$WEPRP_COLLG!=1 & T1$WEPRP_TRAIN!=1, "On job",
          ifelse(T1$WEPRP_INSTR!=1 & T1$WEPRP_COLLG==1 & T1$WEPRP_TRAIN!=1, "College",
           ifelse(T1$WEPRP_INSTR!=1 & T1$WEPRP_COLLG!=1 & T1$WEPRP_TRAIN==1, "NonCollege",
            ifelse(T1$WEPRP_INSTR==1 & T1$WEPRP_COLLG==1 & T1$WEPRP_TRAIN!=1, "On job/College",
             ifelse(T1$WEPRP_INSTR==1 & T1$WEPRP_COLLG!=1 & T1$WEPRP_TRAIN==1, "On job/NonCollege",
              ifelse(T1$WEPRP_INSTR!=1 & T1$WEPRP_COLLG==1 & T1$WEPRP_TRAIN==1, "College/NonCollege","All"))))))
WE<-data.frame(T1[,c(63)],PREPARE,T1[,c(1,355,356,77,78,79,92,75,106)]); rm(T1)
names(WE)<-c("MOG","PREPARE","COC","DESCRIPTION","STW","JOB","PAY","SKILLS","EARN","CURRENTJOB","AGE") #1440
WE$MOG<-ordered(WE$MOG, levels=rev(c("49-0000","47-0000","29-0000","17-0000","51-0000","15-0000",
                                     "27-0000","45-0000","33-0000","19-0000","<5% STW MOG","0% STW MOG","OTHER")),
                        labels=rev(c("49-0000 (89.2%)","47-0000 (56.1%)","29-0000 (42.5%)","17-0000 (25.6%)","51-0000 (25.5%)","15-0000 (21.1%)",
                                     "27-0000 (11.7%)","45-0000 (11.1%)","33-0000 (9.4%)","19-0000 (5.6%)","MOGs (<5.0%)","MOGs (0.0%)","OTHER")))
WE$PREPARE<-ordered(WE$PREPARE, levels=c("On job","College","NonCollege","On job/College","On job/NonCollege","College/NonCollege","All"))
WE$CURRENTJOB<-ifelse(WE$CURRENTJOB==1, "Not Working", ifelse(WE$CURRENTJOB==2,"NO", ifelse(WE$CURRENTJOB==-1,"Valid Skip", "YES")))
table(WE$STW)

#Plot for Preparation by STW
PS<-data.frame(table(WE$STW, WE$PREPARE));names(PS)<-c("STW","Preparation","Percent")
TOTAL<-rep(c(-sum(PS$Percent[PS$STW=="NonSTW"]),sum(PS$Percent[PS$STW=="STW"])),7)
PS$Percent<-(PS$Percent/TOTAL)*100
brks<-seq(-35, 35, 5)
lbls<-as.character(c("35%","","25%","","15%","","5%","0%","5%","","15%","","25%","","35%"))
ggplot(PS, aes(x=Preparation, y=Percent, fill=STW)) +   # Fill column
  geom_bar(stat="identity", width=0.5) +   # draw the bars
  scale_y_continuous(breaks=brks,   # Breaks
                     labels=lbls,
                     limits=c(-35, 35)) + # Labels
  coord_flip() +  # Flip axes
  labs(title="Preparation for Work Experience Credentials",
       subtitle="NonSTW       STW       ") +
  theme_val() +
  theme(plot.title=element_text(hjust=.5, size=17),
        plot.subtitle=element_text(hjust=.5, size=15, vjust=-2, colour="#2a2a2b"),
        legend.position="none",
        axis.text=element_text(size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_manual(values=cbPalette)  # Color palette


#Plot STW by the Four How Useful Questions
USE<-rbind(data.frame(Question=rep("Job",8),table(WE$JOB,WE$STW)),
           data.frame(Question=rep("Pay",8),table(WE$PAY,WE$STW)),
           data.frame(Question=rep("Skills",8),table(WE$SKILLS,WE$STW)))
TOTAL<-c(rep(sum(USE$Freq[1:4]),4),rep(sum(USE$Freq[5:8]),4),rep(sum(USE$Freq[9:12]),4),
         rep(sum(USE$Freq[13:16]),4),rep(sum(USE$Freq[17:20]),4),rep(sum(USE$Freq[21:24]),4))
USE<-data.frame(USE,TOTAL=TOTAL,Percent=round((USE$Freq/TOTAL)*100,0),
                Category=rep(c("NonSTW: Getting job","STW: Getting job","NonSTW: Increasing pay",
                               "STW:Increasing pay","NonSTW: Improving skills","STW: Improving skills"),rep(4,6)),
                Answer=rep(c("Not","Somewhat","Very","Too soon"),6))
names(USE)<-c("Questions","Response","STW","Frequency","Total","Percent","Category","Answer")
USE$Answer<-ordered(USE$Answer, levels=c("Not","Somewhat","Very","Too soon"))
USE$Category<-ordered(USE$Category, levels=c(c("NonSTW: Getting job","STW: Getting job",
                                               "NonSTW: Increasing pay","STW:Increasing pay",
                                               "NonSTW: Improving skills","STW: Improving skills")))

p<-ggplot(USE, aes(Answer, Category)) +
  geom_tile(aes(fill=Percent), colour="white", lwd=2) +
  #  scale_fill_gradient(low="white", high="steelblue")
  scale_fill_gradient(low="ghostwhite", high="steelblue", limits=c(0,90))

ADUSE3<-p + theme_grey() + labs(x="", y="") + scale_x_discrete(expand=c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
#  labs(title="Associate's Degree: Usefulness of Work Experience Program",
#       subtitle="       Percent usefulness by skilled technical workforce (STW=499/NonSTW=502).") +
  labs(title="",
       subtitle="Associate's Degree\n(STW=499/NonSTW=502)") +
  annotate("text", x=c(rep(c(1,2,3,4),6)),
           y=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4)),
           size=7, label=c(USE$Percent), colour="#2a2a2b") +
  coord_fixed(ratio=0.75) +
  theme(legend.position="none",
        plot.title=element_text(hjust=.5, size=17, colour="#2a2a2b"),
        plot.subtitle=element_text(hjust=0.5),
        axis.ticks=element_blank(),
        axis.title.x=element_text(size=20, hjust=0.5, vjust=-1, colour="#2a2a2b"),
        axis.text.x=element_text(size=15, hjust=1.0, angle=45, colour="#2a2a2b"),
        axis.text.y=element_text(size=15, hjust=1.0, colour="#2a2a2b"))
ADUSE3
ggsave("ADUSE3.pdf", width=6, height=6)

rm(USE)

#########################################################################################################
#Plot for STW by credential by MOG
library(ggmosaic)

rm(FINAL)
ONE<-data.frame(Credential=rep("Certification/License",dim(CL)[1]),(CL[,c(1,5)]))
TWO<-data.frame(Credential=rep("Certificate",dim(PSC)[1]),(PSC[,c(1,5)]))
THR<-data.frame(Credential=rep("Work Program",dim(WE)[1]),(WE[,c(1,5)]))
FINAL<-data.frame(rbind(ONE,TWO,THR))

#Plot for NonSTW
ggplot(data=FINAL[FINAL$STW=="NonSTW",]) +
  geom_mosaic(aes(x=product(Credential), fill=MOG), na.rm=TRUE) +
  theme_val() +
  labs(title="Field of NonDegree Credential by Major Occupation Category ",
       subtitle="Current Employment in NonSkilled Technical Workforce (N=2,077)") +
  scale_fill_manual(values=c(cbPalette,cbPalette)) +
  theme(legend.position="none",
        plot.title=element_text(hjust=.5, size=17, colour="#2a2a2b", vjust=-8),
        plot.subtitle=element_text(hjust=.5, size=15, colour="#2a2a2b", vjust=-10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=15, hjust=0.5, colour="#2a2a2b"),
        axis.text.y=element_text(size=15, hjust=1.0, colour="#2a2a2b"))

#Plot for STW
ggplot(data=FINAL[FINAL$STW=="STW",]) +
  geom_mosaic(aes(x=product(Credential), fill=MOG), na.rm=TRUE) +
  theme_val() +
  labs(title="Field of NonDegree Credential by Major Occupation Category ",
       subtitle="Current Employment in Skilled Technical Workforce (N=1,008)") +
  scale_fill_manual(values=c(cbPalette,cbPalette)) +
  theme(legend.position="none",
        plot.title=element_text(hjust=.5, size=17, colour="#2a2a2b", vjust=-8),
        plot.subtitle=element_text(hjust=.5, size=15, colour="#2a2a2b", vjust=-10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=15, hjust=0.5, colour="#2a2a2b"),
        axis.text.y=element_text(size=15, hjust=1.0, colour="#2a2a2b"))

#Plot for ALL
ggplot(data=FINAL) +
  geom_mosaic(aes(x=product(Credential), fill=MOG), na.rm=TRUE) +
  theme_val() +
  labs(title="Field of NonDegree Credential by Major Occupation Category and Credential Type",
       subtitle="Associate's Degree (N=3,405)") +
  scale_fill_manual(values=rev(c(cbPalette[c(2,6,7,8,1,3,5,4)],cbPalette[c(2,6)],"#DCDCDC","#DCDCDC","#DCDCDC"))) +
  theme(legend.position="none",
        plot.title=element_text(hjust=.5, size=17, colour="#2a2a2b", vjust=-8),
        plot.subtitle=element_text(hjust=.5, size=15, colour="#2a2a2b", vjust=-10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=15, hjust=0.5, colour="#2a2a2b"),
        axis.text.y=element_text(size=15, hjust=1.0, colour="#2a2a2b"))


###########################################################################
#Preparation for credential
CLearn<-data.frame(table(CL$PREPARE)); names(CLearn)<-c("Preparation","Percent")
CLearn$Percent<-CLearn$Percent/sum(CLearn$Percent)
PSCearn<-data.frame(table(PSC$PREPARE)); names(PSCearn)<-c("Preparation","Percent")
PSCearn$Percent<-PSCearn$Percent/sum(PSCearn$Percent)
WEearn<-data.frame(table(WE$PREPARE)); names(WEearn)<-c("Preparation","Percent")
WEearn$Percent<-WEearn$Percent/sum(WEearn$Percent)

#Credential for current job
rm(CLjob, PSCjob, WEjob)
CLjob<-data.frame(table(CL$CURRENTJOB)); names(CLjob)<-c("Job","Percent")
CLjob$Percent<-CLjob$Percent/sum(CLjob$Percent)
PSCjob<-data.frame(table(PSC$CURRENTJOB)); names(PSCjob)<-c("Job","Percent")
PSCjob$Percent<-PSCjob$Percent/sum(PSCjob$Percent)
WEjob<-data.frame(table(WE$CURRENTJOB)); names(WEjob)<-c("Job","Percent")
WEjob$Percent<-WEjob$Percent/sum(WEjob$Percent)

#Salary over the Last 12 Months where the Credential is for the Current Job
summary(CL$EARN[CL$CURRENT=="YES"]);summary(CL$AGE[CL$CURRENT=="YES"])
summary(PSC$EARN[PSC$CURRENT=="YES"]);summary(PSC$AGE[PSC$CURRENT=="YES"])
summary(WE$EARN[WE$CURRENT=="YES"]);summary(WE$AGE[WE$CURRENT=="YES"])
summary(CL$EARN[CL$CURRENT=="NO"]);summary(CL$AGE[CL$CURRENT=="NO"])
summary(PSC$EARN[PSC$CURRENT=="NO"]);summary(PSC$AGE[PSC$CURRENT=="NO"])
summary(WE$EARN[WE$CURRENT=="NO"]);summary(WE$AGE[WE$CURRENT=="NO"])




