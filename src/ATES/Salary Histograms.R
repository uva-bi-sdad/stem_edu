#Salary histograms by education
#High School and Less
EARN<-CL[CL$CURRENTJOB=="YES",10]
  CLHSYES<-data.frame(Credential=rep("CHY",length(EARN)),EARN);rm(EARN)
EARN<-CL[CL$CURRENTJOB=="NO",10]
  CLHSNO<-data.frame(Credential=rep("CHN",length(EARN)),EARN);rm(EARN)
#Work Experience Programs
EARN<-WE[WE$CURRENTJOB=="YES",9]
  WEHSYES<-data.frame(Credential=rep("WHY",length(EARN)),EARN);rm(EARN)
EARN<-WE[WE$CURRENTJOB=="NO",9]
  WEHSNO<-data.frame(Credential=rep("WHN",length(EARN)),EARN);rm(EARN)
#Post-secondary Credentials
EARN<-PSC[PSC$CURRENTJOB=="YES",12]
  PSCHSYES<-data.frame(Credential=rep("PHY",length(EARN)),EARN);rm(EARN)
EARN<-PSC[PSC$CURRENTJOB=="NO",12]
  PSCHSNO<-data.frame(Credential=rep("PHN",length(EARN)),EARN);rm(EARN)

CLearn<-data.frame(rbind(CLHSYES[CLHSYES$EARN>0,],CLHSNO[CLHSNO$EARN>0,]))
PSearn<-data.frame(rbind(PSCHSYES[PSCHSYES$EARN>0,],PSCHSNO[PSCHSNO$EARN>0,]))
WEearn<-data.frame(rbind(WEHSYES[WEHSYES$EARN>0,],WEHSNO[WEHSNO$EARN>0,]))


#Salary histograms by education
#Some college
EARN<-CL[CL$CURRENTJOB=="YES",10]
  CLSCYES<-data.frame(Credential=rep("CCY",length(EARN)),EARN);rm(EARN)
EARN<-CL[CL$CURRENTJOB=="NO",10]
  CLSCNO<-data.frame(Credential=rep("CCN",length(EARN)),EARN);rm(EARN)
#Work Experience Programs
EARN<-WE[WE$CURRENTJOB=="YES",9]
  WESCYES<-data.frame(Credential=rep("WCY",length(EARN)),EARN);rm(EARN)
EARN<-WE[WE$CURRENTJOB=="NO",9]
  WESCNO<-data.frame(Credential=rep("WCN",length(EARN)),EARN);rm(EARN)
#Post-secondary Credentials
EARN<-PSC[PSC$CURRENTJOB=="YES",12]
  PSCSCYES<-data.frame(Credential=rep("PCY",length(EARN)),EARN);rm(EARN)
EARN<-PSC[PSC$CURRENTJOB=="NO",12]
  PSCSCNO<-data.frame(Credential=rep("PCN",length(EARN)),EARN);rm(EARN)


CLearn<-data.frame(rbind(CLSCYES[CLSCYES$EARN>0,],CLSCNO[CLSCNO$EARN>0,]))
PSearn<-data.frame(rbind(PSCSCYES[PSCSCYES$EARN>0,],PSCSCNO[PSCSCNO$EARN>0,]))
WEearn<-data.frame(rbind(WESCYES[WESCYES$EARN>0,],WESCNO[WESCNO$EARN>0,]))




#Color blind palette with grey
cbPalette<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black", binwidth=1) +
#   geom_density(alpha=0.7) +
    scale_fill_manual(values=cbPalette, name="",
                      labels=c(" Credential for Job  ", " Credential not for Job")) +
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


plot_multi_histogram(CLearn, 'EARN', 'Credential')
plot_multi_histogram(PSearn, 'EARN', 'Credential')
plot_multi_histogram(WEearn, 'EARN', 'Credential')

rm(CLearn,PSearn,WEearn)






#Box Plots
ggplot(BXP, aes(x=Credential, y=EARN, fill=Credential)) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(pch=21, position=position_jitter(w=0.35, h=2000)) +
theme_val() +
  labs(title="Field of NonDegree Credential by Major Occupation Category ",
       subtitle="Current Employment in NonSkilled Technical Workforce (N=2,077)") +
  scale_fill_manual(values=c(rep(cbPalette, rep(2,8)))) +
  theme(legend.position="none",
        plot.title=element_text(hjust=.5, size=17, colour="#2a2a2b", vjust=-8),
        plot.subtitle=element_text(hjust=.5, size=15, colour="#2a2a2b", vjust=-10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=15, hjust=0.5, colour="#2a2a2b"),
        axis.text.y=element_text(size=15, hjust=1.0, colour="#2a2a2b"))


