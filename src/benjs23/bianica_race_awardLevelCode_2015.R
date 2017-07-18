load("~/Google Drive/2017 DSPG Program - shared folder/DSPG 2017 Projects/NSF NCSES STEM Education/Data for Team Education/meltTable.RData")

ipeds<-select(ipedsM,unitid,year,`CIP Code`,`Award Level Code`,variable,count)
ipeds<-filter(ipeds,variable!="Grand Total")
ipeds2015<-filter(ipeds,year==2015)

ipeds2015<-group_by(ipeds2015,`Award Level Code`,variable) %>%
  summarise(total=sum(count))

total<-group_by(ipeds2015,variable) %>%
  summarise(totallevel=sum(total))

ipeds2015<-left_join(ipeds2015,total,by="variable")
ipeds2015$percent<-ipeds2015$total/ipeds2015$totallevel
ipeds2015$variable<-as.factor(ipeds2015$variable)

ipeds2015$variable = factor(ipeds2015$variable,levels(ipeds2015$variable)[c(7,5,4,3,2,6,8,10,9,1)])

ipeds2015$variable <- factor(ipeds2015$variable,
                             labels = c("White","Hispanic/Latino","Black","Asian",
                                        "American Indian or Alaska Native",
                                        "Pacific Islander",
                                        "Two or More Races", "Nonresident Alien",
                                        "Unknown","Grand Total"))

library(grid)
# white, hispanic, asian, black
png("~/Google Drive/2017 DSPG Program - shared folder/DSPG 2017 Projects/NSF NCSES STEM Education/Data for Team Education/RaceAwardPlot.png",
    height=800,width=1200)
ggplot(ipeds2015,aes(x=variable,y=percent,fill=`Award Level Code`)) + geom_bar(stat="identity") +
  scale_x_discrete(labels=c("White","Hispanic/Latino","Black","Asian",
                            "American Indian or Alaska Native",
                            "Pacific Islander",
                            "Two or More Races", "Nonresident Alien",
                            "Unknown","Grand Total")) +
  scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%")) +
  theme(text=element_text(size=20),axis.text=element_text(size=20),axis.title=element_text(size=24),
        legend.text=element_text(size=20),plot.title=element_text(size=28,hjust=.5),axis.text.x=element_text(angle=0,hjust=1),
        legend.key.height=unit(2.2,"line")) +
  labs(title="2015 Degrees Awarded by Race", y="Percent", x="Race/Ethnicity")
dev.off()
