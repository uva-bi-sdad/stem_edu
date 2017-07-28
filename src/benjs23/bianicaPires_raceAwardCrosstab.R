library(dplyr)
library(data.table)
library(grid)
library(stringr)
library(ggplot2)

load("./data/stem_edu/working/IPEDS_working/meltTable.RData")
# phd data

phds<-read.csv("./data/stem_edu/working/IPEDS_working/all2010.csv", stringsAsFactors = FALSE)

phds<-melt(phds,id=c("unitid","institution.name","year","C2010_A.First.or.Second.Major",
                     "C2010_A.CIP.Code....2010.Classification",
                     "CipTitle","C2010_A.Award.Level.code",
                     "C2010_A_RV.Grand.total",
                     "C2010_A_RV.Grand.total.women","IDX_C"))

phds$variable<-as.character(phds$variable)


# rename race/ethnicity to match other data
phds$variable<-ifelse(phds$variable=="C2010_A_RV.American.Indian.or.Alaska.Native.total...new",
                      "American Indian or Alaska Native Total", phds$variable)
phds$variable<-ifelse(phds$variable=="C2010_A_RV.Asian.total...new",
                      "Asian",phds$variable)
phds$variable<-ifelse(phds$variable=="C2010_A_RV.Black.or.African.American.total...new",
                      "Black or African Americanl",phds$variable)
phds$variable<-ifelse(phds$variable=="C2010_A_RV.Hispanic.or.Latino.total...new",
                      "Hispanic or Latino/Hispanic Total",phds$variable)
phds$variable<-ifelse(phds$variable=="C2010_A_RV.Native.Hawaiian.or.Other.Pacific.Islander.total...new",
                      "Native Hawaiian or Other Pacific Islander Total",phds$variable)
phds$variable<-ifelse(phds$variable=="C2010_A_RV.White.total...new",
                      "White Total",phds$variable)
phds$variable<-ifelse(phds$variable=="C2010_A_RV.Two.or.more.races.total...new",
                      "Two or More Races Total",phds$variable)
phds$variable<-ifelse(phds$variable=="C2010_A_RV.Race.ethnicity.unknown.total",
                      "Race/Ethnicity Unknown Total",phds$variable)
phds$variable<-ifelse(phds$variable=="C2010_A_RV.Nonresident.alien.total",
                      "Nonresident Alien Total",phds$variable)

class(phds$variable)

unique(phds$variable)

#

phds<-group_by(phds,year,variable, C2010_A.Award.Level.code) %>%
  summarise(total=sum(value, na.rm=TRUE))
colnames(phds)[3] <- "Award Level Code"

phds <- filter(phds, `Award Level Code`== "Doctor's degree - other (new degree classification)" |`Award Level Code`== "Doctor's degree - professional practice (new degree classification)"|
                     `Award Level Code`== "Doctor's degree - research/scholarship (new degree classification)")



phds$`Award Level Code`<-"Doctor's degree"
phds<-phds[,c(1,4,2,3)]
phds$raceEth <- phds$variable

phds <- group_by(phds, year , `Award Level Code`, variable) %>% summarise(total =sum(total, na.rm = TRUE))

phdsTotal <- rbind(phdsTotal,phds)
phdsTotal$raceEth <- phdsTotal$variable

ipeds<-select(ipedsM,unitid,year,`CIP Code`,`Award Level Code`,variable,count)
ipeds<-group_by(ipeds,year,`Award Level Code`,variable) %>%
  summarise(total=sum(count))
ipeds<-filter(ipeds,variable!="Grand Total")
ipeds$raceEth <- ipeds$variable

ipeds<-rbind(phdsTotal,ipeds)

ipeds$raceEth<-ifelse(ipeds$variable=="Native Hawaiian or Other Pacific Islander Total" |
                        ipeds$variable=="Asian",
                      "Asian, Native Hawaiian or Other Pacific Islander",as.character(ipeds$variable))




ipeds$raceEth<-as.factor(ipeds$raceEth)
ipeds$raceEth = factor(ipeds$raceEth,levels(ipeds$raceEth)[c(3,2,8,1,7,4,5,6)])
ipeds$raceEth <- factor(ipeds$raceEth,
                        labels = c("Black","Asian, Native Hawaiian or Other Pacific Islander","White",
                                   "American Indian or Alaska Native",
                                   "Two or More Races",
                                   "Hispanic/Latino",
                                   "Nonresident Alien",
                                   "Unknown Race/Ethnicity"))
save(ipeds,file = "./data/stem_edu/working/IPEDS_working/allSTEMDegreesbyRace_2010-2015.RData")

# group awards

ipeds$award<-ifelse(ipeds$`Award Level Code`=="Award of at least 1 but less than 2 academic years" |
                      ipeds$`Award Level Code`=="Award of at least 2 but less than 4 academic years" |
                      ipeds$`Award Level Code`=="Award of less than 1 academic year",
                    "Awards of less than 4 academic years",ipeds$`Award Level Code`)
ipeds$award<-ifelse(ipeds$award=="Post-master's certificate" |
                      ipeds$award=="Postbaccalaureate certificate",
                    "Post-baccalaureate or post-master's certificate",
                    ipeds$award)

ipeds<-group_by(ipeds,award,raceEth,year) %>%
  summarise(total=sum(total))

total<-group_by(ipeds,raceEth,year) %>%
  summarise(totallevel=sum(total))

ipeds<-left_join(ipeds,total,by=c("raceEth","year"))
ipeds$percent<-ipeds$total/ipeds$totallevel

ipeds$award<-as.factor(ipeds$award)
ipeds$award = factor(ipeds$award,levels(ipeds$award)[c(2,1,3,5,6,4)])
ipeds<-rename(ipeds,`Award Level Code`=award)


# white, hispanic, asian, black
yr<-2015
png(paste0("./output/ipeds_trend/",yr,".png"),height=800,width=1200)
ggplot(filter(ipeds,year==yr),aes(x=raceEth,y=percent,fill=`Award Level Code`)) + geom_bar(stat="identity") +
  scale_x_discrete(labels=c("Black or \nAfrican American","Asian, Native Hawaiian \nor Other Pacific Islander","White",
                            "American Indian or \nAlaska Native",
                            "Two or More Races",
                            "Hispanic/Latino",
                            "Nonresident Alien",
                            "Unknown Race/\nEthnicity")) +
  scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_discrete(labels = c(expression("Awards of less than \n4 academic years",
                                            "Associate's degree",
                                            "Bachelor's degree",
                                            "Master's degree",
                                            "Post-baccalaureate or \npost-master's certificate",
                                            "Doctor's degree"))) +
  theme(text=element_text(size=20),axis.text=element_text(size=20),axis.title=element_text(size=24),
        legend.text=element_text(size=20),plot.title=element_text(size=28,hjust=.5),axis.text.x=element_text(angle=45,hjust=1),
        legend.key.height=unit(3.2,"line"),
        legend.title=element_blank()) +
  labs(title=paste0(yr," Degrees Awarded by Race/Ethnicity"), y="Percent", x="Race/Ethnicity")
dev.off()

### percent by year - check
perRace<-group_by(ipeds,`Award Level Code`,raceEth) %>%
  summarise(totalforRace=sum(total))
perRace<-left_join(ipeds,perRace,by=c("Award Level Code","raceEth"))
perRace$percentRace<-perRace$total/perRace$totalforRace

perRaced<-dcast(perRace,`Award Level Code`+raceEth~year,sum,value.var="percentRace")
filter(perRace,`Award Level Code`="Associate's degree")

# create ipeds file for heat map chart
ipeds[is.na(ipeds)]<-0
# remove awards and certs
ipedsAgg<-filter(ipeds,`Award Level Code`!="Awards of less than 4 academic years")
ipedsAgg<-filter(ipedsAgg,`Award Level Code`!="Post-baccalaureate or post-master's certificate")
ipedsAgg<-group_by(ipedsAgg,raceEth,year) %>%
  summarise(totalAcrossAwards=sum(total))
totalRace<-group_by(ipedsAgg,raceEth) %>%
  summarise(totalAcrossRace=sum(totalAcrossAwards))
ipedsAgg<-left_join(ipedsAgg,totalRace,by="raceEth")
ipedsAgg$percent<-ipedsAgg$totalAcrossAwards/ipedsAgg$totalAcrossRace
View(ipedsAgg)
write.csv(ipedsAgg,"./data/stem_edu/working/ipedsbyRace.csv")

aggAssociates <- filter(ipeds, `Award Level Code` == "Associate's degree")
aggAssociates <-group_by(aggAssociates,raceEth,year) %>%
  summarise(totalAcrossAwards=sum(total))
totalRace<-group_by(aggAssociates,raceEth) %>%
  summarise(totalAcrossRace=sum(totalAcrossAwards))
aggAssociates <- left_join(aggAssociates, totalRace, by="raceEth")
aggAssociates$percent<-aggAssociates$totalAcrossAwards/aggAssociates$totalAcrossRace
write.csv(aggAssociates,"./data/stem_edu/working/associatesByRace.csv")

aggMasters <- filter(ipeds, `Award Level Code` == "Master's degree")
aggMasters <-group_by(aggMasters,raceEth,year) %>%
  summarise(totalAcrossAwards=sum(total))
totalRace<-group_by(aggMasters,raceEth) %>%
  summarise(totalAcrossRace=sum(totalAcrossAwards))
aggMasters <- left_join(aggMasters, totalRace, by="raceEth")
aggMasters$percent<-aggMasters$totalAcrossAwards/aggMasters$totalAcrossRace
write.csv(aggMasters,"./data/stem_edu/working/mastersByRace.csv")

aggDoctors <- filter(ipeds, `Award Level Code` == "Doctor's degree")
aggDoctors <-group_by(aggDoctors,raceEth,year) %>%
  summarise(totalAcrossAwards=sum(total))
totalRace<-group_by(aggDoctors,raceEth) %>%
  summarise(totalAcrossRace=sum(totalAcrossAwards))
aggDoctors <- left_join(aggDoctors, totalRace, by="raceEth")
aggDoctors$percent<-aggDoctors$totalAcrossAwards/aggDoctors$totalAcrossRace
write.csv(aggDoctors,"./data/stem_edu/working/doctorsByRace.csv")

aggBachelors <- filter(ipeds, `Award Level Code` == "Bachelor's degree")
aggBachelors <-group_by(aggBachelors,raceEth,year) %>%
  summarise(totalAcrossAwards=sum(total))
totalRace<-group_by(aggBachelors,raceEth) %>%
  summarise(totalAcrossRace=sum(totalAcrossAwards))
aggBachelors <- left_join(aggBachelors, totalRace, by="raceEth")
aggBachelors$percent<-aggBachelors$totalAcrossAwards/aggBachelors$totalAcrossRace
write.csv(aggBachelors,"./data/stem_edu/working/bachelorsByRace.csv")
