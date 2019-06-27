# library
library(tidyverse)
library(viridis)
library(reshape2)
library(ggplot2)



#Create Dataset
data9<-data.frame(individual=rep(Q9$MOG[1:15],rep(3,15)), group=rep(c("CD","noCD","noHS"),15),
                value1=melt(t(Q9[1:15,8:10]))[,3], value2=melt(t(Q9[1:15,11:13]))[,3])
data9[is.na(data9)]<-0

data31<-data.frame(individual=rep(Q31$MOG[1:15],rep(3,15)), group=rep(c("CD","noCD","noHS"),15),
                 value1=melt(t(Q31[1:15,8:10]))[,3], value2=melt(t(Q31[1:15,11:13]))[,3])
data31[is.na(data31)]<-0

data40<-data.frame(individual=rep(Q40$MOG[1:15],rep(3,15)), group=rep(c("CD","noCD","noHS"),15),
                   value1=melt(t(Q40[1:15,8:10]))[,3], value2=melt(t(Q40[1:15,11:13]))[,3])
data40[is.na(data40)]<-0

##############################################################################################################
#Certification and Licenses
#noSTEM versus STEM Plots by Education group
DEG9<-data9[data9$group=="CD",]
  DEG9<-data.frame(MOG=rep(DEG9$individual[1:15],2),Group=c(rep(c("noSTEM","STEM"),c(15,15))),Percent=c(DEG9$value1[1:15],DEG9$value2[1:15]))
noCD9<-data9[data9$group=="noCD",]
  noCD9<-data.frame(MOG=rep(noCD9$individual[1:15],2),Group=c(rep(c("noSTEM","STEM"),c(15,15))),Percent=c(noCD9$value1[1:15],noCD9$value2[1:15]))
noHS9<-data9[data9$group=="noCD",]
  noHS9<-data.frame(MOG=rep(noHS9$individual[1:15],2),Group=c(rep(c("noSTEM","STEM"),c(15,15))),Percent=c(noHS9$value1[1:15],noHS9$value2[1:15]))

#no College Degree
ggplot(noCD9, aes(x=as.numeric(MOG), y=Percent, fill=Group)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(noCD9$Group)))
#College Degree
ggplot(DEG9, aes(x=as.numeric(MOG), y=Percent, fill=Group)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(DEG9$Group)))
#no High School
ggplot(noHS9, aes(x=as.numeric(MOG), y=Percent, fill=Group)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(noHS9$Group)))

#noHS, noCD, versus CD Plots by Job Group
Education9<-data.frame(MOG=rep(DEG9$MOG[1:15],3),Education=c(rep(c("CD","noCD","noHS"),c(15,15,15))),Percent=c(DEG9$Percent[16:30],noCD9$Percent[16:30],noHS9$Percent[16:30]))
ggplot(Education9, aes(x=as.numeric(MOG), y=Percent, fill=Education)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=levels(Education9$Education))

##############################################################################################################
#Certificates
#noSTEM versus STEM Plots by Education group
DEG31<-data31[data31$group=="CD",]
  DEG31<-data.frame(MOG=rep(DEG31$individual[1:15],2),Group=c(rep(c("noSTEM","STEM"),c(15,15))),Percent=c(DEG31$value1[1:15],DEG31$value2[1:15]))
noCD31<-data31[data31$group=="noCD",];
  noCD31<-data.frame(MOG=rep(noCD31$individual[1:15],2),Group=c(rep(c("noSTEM","STEM"),c(15,15))),Percent=c(noCD31$value1[1:15],noCD31$value2[1:15]))
noHS31<-data31[data31$group=="noHS",];
  noHS31<-data.frame(MOG=rep(noHS31$individual[1:15],2),Group=c(rep(c("noSTEM","STEM"),c(15,15))),Percent=c(noHS31$value1[1:15],noHS31$value2[1:15]))

#no College Degree
ggplot(noCD31, aes(x=as.numeric(MOG), y=Percent, fill=Group)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(noCD31$Group)))
#College Degree
ggplot(DEG31, aes(x=as.numeric(MOG), y=Percent, fill=Group)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(DEG31$Group)))
#no High School
ggplot(noHS31, aes(x=as.numeric(MOG), y=Percent, fill=Group)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(noHS31$Group)))

#noHS, noCD, versus CD Plots by Job Group
Education31<-data.frame(MOG=rep(DEG31$MOG[1:15],3),Education=c(rep(c("CD","noCD","noHS"),c(15,15,15))),Percent=c(DEG31$Percent[16:30],noCD31$Percent[16:30],noHS31$Percent[16:30]))
ggplot(Education31, aes(x=as.numeric(MOG), y=Percent, fill=Education)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=levels(Education31$Education))

##############################################################################################################
#Work Programs
#noSTEM versus STEM Plots by Education group
DEG40<-data40[data40$group=="CD",]
  DEG40<-data.frame(MOG=rep(DEG40$individual[1:15],2),Group=c(rep(c("noSTEM","STEM"),c(15,15))),Percent=c(DEG40$value1[1:15],DEG40$value2[1:15]))
noCD40<-data40[data31$group=="noCD",];
  noCD40<-data.frame(MOG=rep(noCD40$individual[1:15],2),Group=c(rep(c("noSTEM","STEM"),c(15,15))),Percent=c(noCD40$value1[1:15],noCD40$value2[1:15]))
noHS40<-data40[data40$group=="noHS",];
  noHS40<-data.frame(MOG=rep(noHS40$individual[1:15],2),Group=c(rep(c("noSTEM","STEM"),c(15,15))),Percent=c(noHS40$value1[1:15],noHS40$value2[1:15]))

#no College Degree
ggplot(noCD40, aes(x=as.numeric(MOG), y=Percent, fill=Group)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(noCD40$Group)))
#College Degree
ggplot(DEG40, aes(x=as.numeric(MOG), y=Percent, fill=Group)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(DEG40$Group)))
#no High School
ggplot(noHS40, aes(x=as.numeric(MOG), y=Percent, fill=Group)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(noHS40$Group)))

#noHS, noCD, versus CD Plots by Job Group
Education40<-data.frame(MOG=rep(DEG40$MOG[1:15],3),Education=c(rep(c("CD","noCD","noHS"),c(15,15,15))),Percent=c(DEG40$Percent[16:30],noCD40$Percent[16:30],noHS40$Percent[16:30]))
ggplot(Education40, aes(x=as.numeric(MOG), y=Percent, fill=Education)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=levels(Education40$Education))




