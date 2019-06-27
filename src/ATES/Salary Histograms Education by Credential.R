#Salary histograms by education
#Certifications and Licenses
EARN<-HSCL[HSCL$CURRENTJOB=="YES",10]
  HSCLYES<-data.frame(Credential=rep("HS",length(EARN)),EARN);rm(EARN)
EARN<-NCCL[NCCL$CURRENTJOB=="YES",10]
  NCCLYES<-data.frame(Credential=rep("NC",length(EARN)),EARN);rm(EARN)
EARN<-ADCL[ADCL$CURRENTJOB=="YES",10]
  ADCLYES<-data.frame(Credential=rep("AD",length(EARN)),EARN);rm(EARN)

#Credentials
EARN<-HSCE[HSCE$CURRENTJOB=="YES",11]
  HSCEYES<-data.frame(Credential=rep("HS",length(EARN)),EARN);rm(EARN)
EARN<-NCCE[NCCE$CURRENTJOB=="YES",12]
  NCCEYES<-data.frame(Credential=rep("NC",length(EARN)),EARN);rm(EARN)
EARN<-ADCE[ADCE$CURRENTJOB=="YES",12]
  ADCEYES<-data.frame(Credential=rep("AD",length(EARN)),EARN);rm(EARN)

#Work Experience Programs
EARN<-HSWE[HSWE$CURRENTJOB=="YES",9]
  HSWEYES<-data.frame(Credential=rep("HS",length(EARN)),EARN);rm(EARN)
EARN<-NCWE[NCWE$CURRENTJOB=="YES",9]
  NCWEYES<-data.frame(Credential=rep("NC",length(EARN)),EARN);rm(EARN)
EARN<-ADWE[ADWE$CURRENTJOB=="YES",9]
  ADWEYES<-data.frame(Credential=rep("AD",length(EARN)),EARN);rm(EARN)

CLearn<-data.frame(rbind(HSCLYES[HSCLYES$EARN>0,],NCCLYES[NCCLYES$EARN>0,],ADCLYES[ADCLYES$EARN>0,]))
CEearn<-data.frame(rbind(HSCEYES[HSCEYES$EARN>0,],NCCEYES[NCCEYES$EARN>0,],ADCEYES[ADCEYES$EARN>0,]))
WEearn<-data.frame(rbind(HSWEYES[HSWEYES$EARN>0,],NCWEYES[NCWEYES$EARN>0,],ADWEYES[ADWEYES$EARN>0,]))




#Color blind palette with grey
cbPalette<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black", binwidth=1) +
#   geom_density(alpha=0.7) +
    scale_fill_manual(values=cbPalette[c(4,2,7)], name="",
                      labels=c(" High School or Less  ", " Some College No Degree ", " Associate's Degree")) +
    scale_y_continuous(limits=c(0, 0.3),breaks=c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)) +
    scale_x_continuous(limits=c(0.5, 9.5),breaks=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5),
                       label=c("0-$10,000","10,001-$20,000","20,001-$30,000","30,001-$40,000","40,001-$50,000",
                               "50,001-$60,000","60,001-$75,000","75,001-$150,000",">$150,000")) +
#   geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x="", y="") +
    theme_val() +
    annotate("text", x=6.5, y=0.225, label="Work Experience Programs") +
    theme(axis.text.x=element_text(angle=45,hjust=1),
          legend.title=element_blank())
  plt + guides(fill=guide_legend(title=label_column))
}
#These plots are very hard to interpret
plot_multi_histogram(CLearn, 'EARN', 'Credential')
plot_multi_histogram(CEearn, 'EARN', 'Credential')
plot_multi_histogram(WEearn, 'EARN', 'Credential')

#Data set for certifications/license plot
Total<-data.frame(table(CLearn$Credential))[,2]
  Denominator<-rep(Total,9)
CLplot<-data.frame(table(CLearn$Credential,CLearn$EARN))
  names(CLplot)<-c("Education","Earnings","Percent")
  CLplot$Percent<-(CLplot$Percent/Denominator)*100
  CLplot$Education<-factor(CLplot$Education,
                           labels=c("High School or Less  ","Some College but No Degree  ","Associate's Degree"))
  rm(Total, Denominator)
#Data set for Certificates
Total<-data.frame(table(CEearn$Credential))[,2]
  Denominator<-rep(Total,9)
CEplot<-data.frame(table(CEearn$Credential,CEearn$EARN))
  names(CEplot)<-c("Education","Earnings","Percent")
  CEplot$Percent<-(CEplot$Percent/Denominator)*100
  rm(Total, Denominator)
#Data set for Work experience programs
  Total<-data.frame(table(WEearn$Credential))[,2]
  Denominator<-rep(Total,9)
  WEplot<-data.frame(table(WEearn$Credential,WEearn$EARN))
  names(WEplot)<-c("Education","Earnings","Percent")
  WEplot$Percent<-(WEplot$Percent/Denominator)*100
  WEplot$Education<-factor(WEplot$Education,
                           labels=c("High School or Less  ","Some College but No Degree  ","Associate's Degree"))
  rm(Total, Denominator)

#Line Plots by Credential
CLbyCE<-ggplot(CLplot, aes(x=as.numeric(Earnings), y=Percent, group=Education)) +
    geom_line(aes(colour=Education), lwd=2) +
    geom_point(colour="white", size=3) +
    geom_point(aes(colour=Education), size=2.5) +
    scale_color_manual(values=cbPalette, "Education") +
    xlab("") + ylab("Percent") +
    theme_val() +
    annotate("text", x=1, y=17, label="Certification/License", hjust=0) +
#    scale_x_continuous(breaks=c(1:9),
#                       labels=c("0-$10,000","10,001-$20,000","20,001-$30,000","30,001-$40,000","40,001-$50,000",
#                                "50,001-$60,000","60,001-$75,000","75,001-$150,000",">$150,000")) +
    scale_x_continuous(breaks=c(1:9),
                       labels=c("","","","","","","","","")) +
    scale_y_continuous(breaks=c(-2,0,2,4,6,8,10,12,14,16,18,20),
                       labels=c(-2,0,2,4,6,8,10,12,14,16,18,20)) +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(legend.title=element_blank()) +
    theme(axis.text.x=element_text(angle=45)) +
    labs(title="Earning Categories by Credential and Eduction Levels")
CLbyCE
ggsave("CLbyCE.pdf", width=6, height=6)

CEbyCE<-ggplot(CEplot, aes(x=as.numeric(Earnings), y=Percent, group=Education)) +
  geom_line(aes(colour=Education), lwd=2) +
  geom_point(colour="white", size=3) +
  geom_point(aes(colour=Education), size=2.5) +
  scale_color_manual(values=cbPalette, "Education") +
  xlab("") + ylab("Percent") +
  theme_val() +
  annotate("text", x=1, y=17, label="Certificate", hjust=0) +
  #    scale_x_continuous(breaks=c(1:9),
  #                       labels=c("0-$10,000","10,001-$20,000","20,001-$30,000","30,001-$40,000","40,001-$50,000",
  #                                "50,001-$60,000","60,001-$75,000","75,001-$150,000",">$150,000")) +
  scale_x_continuous(breaks=c(1:9),
                     labels=c("","","","","","","","","")) +
  scale_y_continuous(breaks=c(-2,0,2,4,6,8,10,12,14,16,18,20),
                     labels=c(-2,0,2,4,6,8,10,12,14,16,18,20)) +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.title=element_blank()) +
  theme(axis.text.x=element_text(angle=45)) +
  labs(title="")
CEbyCE
ggsave("CEbyCE.pdf", width=6, height=6)

WEbyCE<-ggplot(WEplot, aes(x=as.numeric(Earnings), y=Percent, group=Education)) +
  geom_line(aes(colour=Education), lwd=2) +
  geom_point(colour="white", size=3) +
  geom_point(aes(colour=Education), size=2.5) +
  scale_color_manual(values=cbPalette, "Education") +
  xlab("") + ylab("Percent") +
  theme_val() +
  annotate("text", x=1, y=17, label="Work Experience Program", hjust=0) +
  scale_x_continuous(breaks=c(1:9),
                     labels=c("0-$10,000","10,001-$20,000","20,001-$30,000","30,001-$40,000","40,001-$50,000",
                              "50,001-$60,000","60,001-$75,000","75,001-$150,000",">$150,000")) +
  scale_y_continuous(breaks=c(-2,0,2,4,6,8,10,12,14,16,18,20),
                     labels=c(-2,0,2,4,6,8,10,12,14,16,18,20)) +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.title=element_blank()) +
  theme(axis.text.x=element_text(angle=45)) +
  labs(title="")
WEbyCE
ggsave("WEbyCE.pdf", width=6, height=7)

