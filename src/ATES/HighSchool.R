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
rm(CL, PSC, WE)

############################################################################################
#Create the HS diploma or less data set
#HS diploma or less - 13879 / 29.07% of the survey
HS<-ATES[ATES$eduattn<4,]; dim(HS); dim(HS)[1]/dim(ATES)[1]; table(ATES$eduattn); table(HS$eduattn)

#Recode the kinds of work that need to be split between two major occupation groups
#CNFIELD1 4 and 19
T4<-which(HS$CNFIELD1==4);T4no<-ceiling(length(T4)/2);
HS$CNFIELD1[T4[1:T4no]]<--4
T19<-which(HS$CNFIELD1==19);T19no<-ceiling(length(T19)/2);
HS$CNFIELD1[T19[1:T19no]]<--19;rm(T4,T4no,T19,T19no)
#psfos 3
T3<-which(HS$psfos==3);T3no<-ceiling(length(T3)/2)
HS$psfos[T3[1:T3no]]<--3;rm(T3,T3no)
#wefolp 7
T7<-which(HS$wefolp==7);T7no<-ceiling(length(T7)/2)
HS$wefolp[T7[1:T7no]]<--7;rm(T7,T7no)

#Assign the kinds of work for a credential into major occupation categories
#Q9. Occupation for most important certifications/licenses
HSQ9<-ifelse(HS$CNFIELD1==24, "49-0000",
       ifelse(HS$CNFIELD1==23, "47-0000",
        ifelse(HS$CNFIELD1==10, "29-0000",
         ifelse(HS$CNFIELD1==11, "29-0000",
          ifelse(HS$CNFIELD1==1, "17-0000",
           ifelse(HS$CNFIELD1==2, "17-0000",
            ifelse(HS$CNFIELD1==3, "15-0000",
             ifelse(HS$CNFIELD1==4, "15-0000",
              ifelse(HS$CNFIELD1==-4, "19-0000",
               ifelse(HS$CNFIELD1==19, "45-0000",
                ifelse(HS$CNFIELD1==-19, "19-0000",
                 ifelse(HS$CNFIELD1==17, "33-0000",
                  ifelse(HS$CNFIELD1==5, "<5% STW MOG",
                   ifelse(HS$CNFIELD1==6, "<5% STW MOG",
                    ifelse(HS$CNFIELD1==7, "<5% STW MOG",
                     ifelse(HS$CNFIELD1==8, "<5% STW MOG",
                      ifelse(HS$CNFIELD1==16, "<5% STW MOG",
                       ifelse(HS$CNFIELD1==25, "<5% STW MOG",
                        ifelse(HS$CNFIELD1==18, "MOGs (0.0%)",
                         ifelse(HS$CNFIELD1==20, "MOGs (0.0%)",
                          ifelse(HS$CNFIELD1==21, "MOGs (0.0%)",
                           ifelse(HS$CNFIELD1==22, "MOGs (0.0%)",
                            ifelse(HS$CNFIELD1==9, "MOGs (0.0%)",
                             ifelse(HS$CNFIELD1==12, "MOGs (0.0%)",
                              ifelse(HS$CNFIELD1==13, "MOGs (0.0%)",
                               ifelse(HS$CNFIELD1==14, "MOGs (0.0%)",
                                ifelse(HS$CNFIELD1==15, "MOGs (0.0%)",
                                 ifelse(HS$CNFIELD1==26, "OTHER",
                                   ifelse(HS$CNFIELD1==27, "OTHER",
                                    ifelse(HS$CNFIELD1==-8, "Uncodeable",
                                     ifelse(HS$CNFIELD1==-9, "Missing","Valid Skip")))))))))))))))))))))))))))))))
HS$CNFIELD1<-HSQ9
#Q30d. Number of certificates from a community or technical college or other school after HS
HSQ31d<-ifelse(HS$psfos==19, "49-0000",
         ifelse(HS$psfos==7, "47-0000",
          ifelse(HS$psfos==14, "29-0000",
           ifelse(HS$psfos==11, "17-0000",
            ifelse(HS$psfos==18, "51-0000",
             ifelse(HS$psfos==6, "15-0000",
              ifelse(HS$psfos==4, "27-0000",
               ifelse(HS$psfos==3, "45-0000",
                ifelse(HS$psfos==-3, "19-0000",
                 ifelse(HS$psfos==15, "33-0000",
                  ifelse(HS$psfos==5, "<5% STW MOG",
                   ifelse(HS$psfos==1, "<5% STW MOG",
                    ifelse(HS$psfos==16, "<5% STW MOG",
                     ifelse(HS$psfos==9, "<5% STW MOG",
                      ifelse(HS$psfos==20, "<5% STW MOG",
                       ifelse(HS$psfos==2, "<5% STW MOG",
                        ifelse(HS$psfos==10, "MOGs (0.0%)",
                         ifelse(HS$psfos==8, "MOGs (0.0%)",
                          ifelse(HS$psfos==13, "MOGs (0.0%)",
                           ifelse(HS$psfos==12, "MOGs (0.0%)",
                            ifelse(HS$psfos==17, "MOGs (0.0%)",
                             ifelse(HS$psfos==21, "OTHER","Valid Skip"))))))))))))))))))))))
HS$psfos<-HSQ31d
#Q40. Occupation for most work experience programs
HSQ40<-ifelse(HS$wefolp==24, "49-0000",
        ifelse(HS$wefolp==20, "49-0000",
         ifelse(HS$wefolp==1, "47-0000",
          ifelse(HS$wefolp==2, "47-0000",
           ifelse(HS$wefolp==3, "47-0000",
            ifelse(HS$wefolp==4, "47-0000",
             ifelse(HS$wefolp==5, "47-0000",
              ifelse(HS$wefolp==6, "29-0000",
               ifelse(HS$wefolp==7, "29-0000",
                ifelse(HS$wefolp==-7, "31-0000",
                 ifelse(HS$wefolp==14, "17-0000",
                  ifelse(HS$wefolp==18, "51-0000",
                   ifelse(HS$wefolp==21, "51-0000",
                    ifelse(HS$wefolp==11, "15-0000",
                     ifelse(HS$wefolp==26, "27-0000",
                      ifelse(HS$wefolp==16, "33-0000",
                       ifelse(HS$wefolp==19, "<5% STW MOG",
                        ifelse(HS$wefolp==9, "<5% STW MOG",
                         ifelse(HS$wefolp==17, "<5% STW MOG",
                          ifelse(HS$wefolp==10, "<5% STW MOG",
                           ifelse(HS$wefolp==13, "<5% STW MOG",
                            ifelse(HS$wefolp==22, "MOGs (0.0%)",
                             ifelse(HS$wefolp==23, "MOGs (0.0%)",
                              ifelse(HS$wefolp==8, "MOGs (0.0%)",
                               ifelse(HS$wefolp==12, "MOGs (0.0%)",
                                ifelse(HS$wefolp==15, "MOGs (0.0%)",
                                 ifelse(HS$wefolp==25, "OTHER","Valid Skip")))))))))))))))))))))))))))
HS$wefolp<-HSQ40

#Recode Earnings over the last 12 months Q62
HSQ62<-ifelse(HS$eeearn==1,5000,ifelse(HS$eeearn==2,15000,ifelse(HS$eeearn==3,25000,ifelse(HS$eeearn==4,35000,
        ifelse(HS$eeearn==5,45000,ifelse(HS$eeearn==6,55000,ifelse(HS$eeearn==7,67500,ifelse(HS$eeearn==8,112500,
         ifelse(HS$eeearn==9,175000,NA)))))))))
HS$eeearn<-HSQ62



#Assign the Census occupation codes a description and STW status
#Q66. What kind of work were you doing?
HS<-merge(HS, COC, by.x="empocc", by.y="COC", all.x=TRUE, all.y=FALSE)


###########################################################################
#Analyses for certification/license
#Select those that have a current certification/license
T1<-HS[HS$cnmain==1,]; dim(T1) #1440
#Recode preparation Question 13 A, B, C
PREPARE<-ifelse(T1$CNPRP_COLLG1==2 & T1$CNPRP_TRAIN1!=2 & T1$CNPRP_ONOWN1!=2, "School",
          ifelse(T1$CNPRP_COLLG1!=2 & T1$CNPRP_TRAIN1==2 & T1$CNPRP_ONOWN1!=2, "Training",
           ifelse(T1$CNPRP_COLLG1!=2 & T1$CNPRP_TRAIN1!=2 & T1$CNPRP_ONOWN1==2, "Self-Study",
            ifelse(T1$CNPRP_COLLG1==2 & T1$CNPRP_TRAIN1==2 & T1$CNPRP_ONOWN1!=2, "School/Training",
             ifelse(T1$CNPRP_COLLG1==2 & T1$CNPRP_TRAIN1!=2 & T1$CNPRP_ONOWN1==2, "School/Self-Study",
              ifelse(T1$CNPRP_COLLG1!=2 & T1$CNPRP_TRAIN1==2 & T1$CNPRP_ONOWN1==2, "Training/Self-Study","ALL"))))))
CL<-data.frame(T1[,c(12)],PREPARE,T1[,c(1,355,356,22,23,24,25,92,21,106)]); rm(T1)
names(CL)<-c("MOG","PREPARE","COC","DESCRIPTION","STW","GET","KEEP","MARKET","SKILLS","EARN","CURRENTJOB","AGE") #1440
#Remove MOGs that are "Missing" (93) or "Uncodeable" (15)
temp<-CL[CL$MOG!="Missing", ]; temp<-temp[temp$MOG!="Uncodeable", ]; temp$MOG<-factor(temp$MOG); CL<-temp
#Order by Rothwell percent STW
CL$MOG<-ordered(CL$MOG, levels=rev(c("49-0000","47-0000","29-0000","17-0000","51-0000","15-0000",
                                     "27-0000","45-0000","33-0000","19-0000","<5% STW MOG","0% STW MOG","OTHER")),
                        labels=rev(c("49-0000 (89.2%)","47-0000 (56.1%)","29-0000 (42.5%)","17-0000 (25.6%)","51-0000 (25.5%)","15-0000 (21.1%)",
                                     "27-0000 (11.7%)","45-0000 (11.1%)","33-0000 (9.4%)","19-0000 (5.6%)","MOGs (<5.0%)","MOGs (0.0%)","OTHER")))
CL$PREPARE<-ordered(CL$PREPARE, levels=c("School","Training","Self-Study","School/Training","School/Self-Study","Training/Self-Study","All"))
CL$CURRENTJOB<-ifelse(CL$CURRENTJOB==3,"YES",ifelse(CL$CURRENTJOB==1,"Not Working","NO"))


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


HSUSE1<-p + theme_grey() + labs(x="", y="") + scale_x_discrete(expand=c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
#  labs(title="High School or Less: Usefulness of Certification/License",
#       subtitle="           Percent usefulness by current job type (STW=421/NonSTW=808).") +
   labs(title="",
        subtitle="High School or Less\n(STW=421/NonSTW=808)") +
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
HSUSE1
ggsave("HSUSE1.pdf", width=6, height=6)

rm(USE)

###########################################################################
#Analyses for post-secondary credential
#Select those that have a post-secondary credential
T1<-HS[HS$certprog==1,]; dim(T1)  #1284
#Recode preparation Question 32
PREPARE<-ifelse(T1$lastpscer==1,"College/University","Someplace else")
PSC<-data.frame(T1[,c(51)],PREPARE,T1[,c(1,355,356,59,60,61,53,58,92,106)]); rm(T1)
names(PSC)<-c("MOG","PREPARE","COC","DESCRIPTION","STW","JOB","PAY","SKILLS","HOURS","CURRENTJOB","EARN","AGE") #1440
PSC$MOG<-ordered(PSC$MOG, levels=rev(c("49-0000","47-0000","29-0000","17-0000","51-0000","15-0000",
                                       "27-0000","45-0000","33-0000","19-0000","<5% STW MOG","0% STW MOG","OTHER")),
                          labels=rev(c("49-0000 (89.2%)","47-0000 (56.1%)","29-0000 (42.5%)","17-0000 (25.6%)","51-0000 (25.5%)","15-0000 (21.1%)",
                                       "27-0000 (11.7%)","45-0000 (11.1%)","33-0000 (9.4%)","19-0000 (5.6%)","MOGs (<5.0%)","MOGs (0.0%)","OTHER")))
PSC$PREPARE<-ordered(PSC$PREPARE, levels=c("College/University","Someplace else"))
PSC$CURRENTJOB<-ifelse(PSC$CURRENTJOB==1, "Not Working", ifelse(PSC$CURRENTJOB==2,"NO", ifelse(PSC$CURRENTJOB==-1,"Valid Skip", "YES")))


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

HSUSE2<-p + theme_grey() + labs(x="", y="") + scale_x_discrete(expand=c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
#  labs(title="High School or Less: Usefulness of Post-Secondary Credential",
#       subtitle="           Percent usefulness by current job type (STW=306/NonSTW=837).") +
   labs(title="",
        subtitle="High School or Less\n(STW=306/NonSTW=837)") +
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
ggsave("HSUSE2.pdf", width=6, height=6)

rm(USE)


###########################################################################
#Analyses for work experience credential
#Select those that have a work experimence credential
T1<-HS[HS$weprog==3,]; dim(T1); table(HS$eduattn); table(T1$eduattn); table(T1$weprog)  #780
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

HSUSE3<-p + theme_grey() + labs(x="", y="") + scale_x_discrete(expand=c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
#  labs(title="High School or Less: Usefulness of Work Experience Programs",
#       subtitle="           Percent usefulness by current job type (STW=281/NonSTW=432).") +
  labs(title="",
       subtitle="High School or Less\n(STW=281/NonSTW=432).") +
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
ggsave("HSUSE3.pdf", width=6, height=6)

rm(USE)


#########################################################################################################
#Plot for STW by credential by MOG
library(ggalluvial)
HSCL<-CL[,c(1,11,5)]
Temp<-data.frame(table(HSCL$MOG,HSCL$CURRENTJOB,HSCL$STW));names(Temp)<-c("MOG","USE","STW","FREQ")
ggplot(data=Temp,
       aes(y=FREQ, axis1=MOG, axis2=USE, axis3=STW)) +
  geom_alluvium(aes(fill=MOG), width=0, knot.pos=0, reverse=FALSE) +
  guides(fill=FALSE) +
  theme_val() +
  geom_stratum(width=1/8, reverse=FALSE) +
  scale_fill_manual(values=c(cbPalette,cbPalette,cbPalette)) +
  geom_text(stat="stratum", label.strata=TRUE, reverse=FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("MOG", "Use", "STW")) +
  ggtitle("HS/CL")


###########################################################################
#Prepration for Credential
CLearn<-data.frame(table(CL$PREPARE)); names(CLearn)<-c("Preparation","Percent")
CLearn$Percent<-CLearn$Percent/sum(CLearn$Percent)
PSCearn<-data.frame(table(PSC$PREPARE)); names(PSCearn)<-c("Preparation","Percent")
PSCearn$Percent<-PSCearn$Percent/sum(PSCearn$Percent)
WEearn<-data.frame(table(WE$PREPARE)); names(WEearn)<-c("Preparation","Percent")
WEearn$Percent<-WEearn$Percent/sum(WEearn$Percent)

#Credential for Current Job
rm(CLjob, PSCjob, WEjob)
CLjob<-data.frame(table(CL$CURRENTJOB)); names(CLjob)<-c("Job","Percent")
CLjob$Percent<-CLjob$Percent/sum(CLjob$Percent)
PSCjob<-data.frame(table(PSC$CURRENTJOB)); names(PSCjob)<-c("Job","Percent")
PSCjob$Percent<-PSCjob$Percent/sum(PSCjob$Percent)
WEjob<-data.frame(table(WE$CURRENTJOB)); names(WEjob)<-c("Job","Percent")
WEjob$Percent<-WEjob$Percent/sum(WEjob$Percent)

#Salary over the Last 12 Months where the Credential is for the Current Job
summary(CL$EARN[CL$CURRENT=="YES"])
summary(PSC$EARN[PSC$CURRENT=="YES"])
summary(WE$EARN[WE$CURRENT=="YES"])
summary(CL$EARN[CL$CURRENT=="NO"])
summary(PSC$EARN[PSC$CURRENT=="NO"])
summary(WE$EARN[WE$CURRENT=="NO"])


