# Library
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)

STW<-read_excel("~/git/stem_edu/data/Percent STW.xls")
STW$MOG<-(ordered(STW$MOG,levels=STW$MOG))

HGH<-STW[c(1:3),]
HGH$MOG<-factor(HGH$MOG)
LOW<-STW[c(4:6),]
LOW$MOG<-factor(LOW$MOG)

ggplot(LOW) +
  geom_segment(aes(x=MOG, xend=MOG, y=NCD9, yend=CD9), color="grey", lwd=3) +
  geom_segment(aes(x=MOG, xend=MOG, y=NCD9, yend=NHS9), color="grey", lwd=3) +
  geom_point(aes(x=MOG, y=NCD9), color="darkgoldenrod3", size=4) +
  geom_point(aes(x=MOG, y=NCD9), pch=1, color="black", size=6) +
  geom_point(aes(x=MOG, y=CD9), color="darkgoldenrod4", size=4) +
  geom_point(aes(x=MOG, y=NHS9), color="darkgoldenrod1", size=4) +
  coord_flip() +
  theme_val() +
  theme(
    panel.border=element_blank(),
    axis.text.x=element_text(color="grey20", size=13),
    axis.text.y=element_text(color="grey20", size=13),
    plot.title = element_text(color="grey20", size=15, face="bold", hjust=0.5),
    plot.subtitle=element_text(color="grey20", size=12, hjust=0.5)
  ) +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x="", y="",
       title="Percentage STEM Certifications & Licenses",
       subtitle="12.2% No High School Diploma / 12.9% High School+ / 9.8% College/Prof. Degree") +
#  annotate("text", x=16.0, y=12.5, label="Education", color="grey20", size=5, hjust=0) +
#  annotate("text", x=15.5, y=13.1, label="No High School Diploma", color="grey20", size=5, hjust=0) +
# annotate("text", x=15.0, y=13.1, label="High School Diploma+", color="grey20", size=5, hjust=0) +
#  annotate("text", x=14.5, y=13.1, label="College/Prof. Degree", color="grey20", size=5, hjust=0) +
#  annotate("point", x=15.5, y=12.7, color="darkgoldenrod1", size=5) +
#  annotate("point", x=15.0, y=12.7, color="darkgoldenrod3", size=5) +
#    annotate("point", x=15.0, y=12.7, color="black", pch=1, size=7) +
#  annotate("point", x=14.5, y=12.7, color="darkgoldenrod4", size=5)
 annotate("text", x=3.00, y=12.5, label="Education", color="grey20", size=5, hjust=0) +
 annotate("text", x=2.75, y=13.1, label="No High School Diploma", color="grey20", size=5, hjust=0) +
 annotate("text", x=2.50, y=13.1, label="High School Diploma+", color="grey20", size=5, hjust=0) +
 annotate("text", x=2.25, y=13.1, label="College/Prof. Degree", color="grey20", size=5, hjust=0) +
 annotate("point", x=2.75, y=12.7, color="darkgoldenrod1", size=5) +
 annotate("point", x=2.50, y=12.7, color="darkgoldenrod3", size=5) +
  annotate("point", x=2.50, y=12.7, color="black", pch=1, size=7) +
 annotate("point", x=2.25, y=12.7, color="darkgoldenrod4", size=5)

ggplot(LOW) +
  geom_segment( aes(x=MOG, xend=MOG, y=NCD31, yend=CD31), color="grey", lwd=3) +
  geom_segment( aes(x=MOG, xend=MOG, y=NCD31, yend=NHS31), color="grey", lwd=3) +
  geom_point(aes(x=MOG, y=NCD31), color="darkgoldenrod3", size=4) +
  geom_point(aes(x=MOG, y=NCD31), pch=1, color="black", size=6) +
  geom_point(aes(x=MOG, y=CD31), color="darkgoldenrod4", size=4) +
  geom_point(aes(x=MOG, y=NHS31), color="darkgoldenrod1", size=4) +
  coord_flip() +
  theme_val() +
  theme(
    panel.border=element_blank(),
    axis.text.x=element_text(color="grey20", size=13),
    axis.text.y=element_text(color="grey20", size=13),
    plot.title = element_text(color="grey20", size=15, face="bold", hjust=0.5),
    plot.subtitle=element_text(color="grey20", size=12, hjust=0.5)
  ) +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x="", y="",
       title="Percentage STEM Post-Secondary Certificates",
       subtitle="29.0% No High School Diploma / 25.8% High School+ / 15.7% College/Prof. Degree")


ggplot(LOW) +
  geom_segment( aes(x=MOG, xend=MOG, y=NCD40, yend=CD40), color="grey", lwd=3) +
  geom_segment( aes(x=MOG, xend=MOG, y=NCD40, yend=NHS40), color="grey", lwd=3) +
  geom_point(aes(x=MOG, y=NCD40), color="darkgoldenrod3", size=4) +
  geom_point(aes(x=MOG, y=NCD40), pch=1, color="black", size=6) +
  geom_point(aes(x=MOG, y=CD40), color="darkgoldenrod4", size=4) +
  geom_point(aes(x=MOG, y=NHS40), color="darkgoldenrod1", size=4) +
  coord_flip() +
  theme_val() +
  theme(
    panel.border=element_blank(),
    axis.text.x=element_text(color="grey20", size=13),
    axis.text.y=element_text(color="grey20", size=13),
    plot.title = element_text(color="grey20", size=15, face="bold", hjust=0.5),
    plot.subtitle=element_text(color="grey20", size=12, hjust=0.5)
  ) +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x="", y="",
       title="Percentage STEM Work Experience Programs",
       subtitle="33.7% No High School Diploma / 18.6% High School+ / 8.0% College/Prof. Degree")

























# Create data
value1=abs(rnorm(26))*2
data=data.frame(x=LETTERS[1:26], value1=value1, value2=value1+1+rnorm(26, sd=1) )

# Reorder data using average?
data = data %>% rowwise() %>% mutate( mymean = mean(c(value1,value2) )) %>% arrange(mymean) %>% mutate(x=factor(x, x))

# plot
ggplot(data) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()

# With a bit more style
ggplot(data) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Value of Y")

Q9[is.na(Q9)]<-0
ggplot(Q9[1:15,]) +
  geom_segment( aes(x=MOG[1:15], xend=MOG[1:15], y=NCDpstem[1:15], yend=CDpstem[1:15]), color="grey") +
  geom_segment( aes(x=MOG[1:15], xend=MOG[1:15], y=NCDpstem[1:15], yend=NHSpstem[1:15]), color="grey") +
  geom_point(aes(x=MOG[1:15], y=NCDpstem[1:15]), color="darkgoldenrod3", size=4) +
  geom_point(aes(x=MOG[1:15], y=CDpstem[1:15]), color="darkgoldenrod4", size=4) +
  geom_point(aes(x=MOG[1:15], y=NHSpstem[1:15]), color="darkgoldenrod1", size=4) +  coord_flip()+
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Percentage STEM Certifications & Licenses")


ggplot(Q31[1:18,]) +
  geom_segment( aes(x=MOG[1:18], xend=MOG[1:18], y=NCDpstem[1:18], yend=CDpstem[1:18]), color="grey") +
  geom_segment( aes(x=MOG[1:18], xend=MOG[1:18], y=NCDpstem[1:18], yend=NHSpstem[1:18]), color="grey") +
  geom_point(aes(x=MOG[1:18], y=NCDpstem[1:18]), color="darkgoldenrod3", size=4) +
  geom_point(aes(x=MOG[1:18], y=CDpstem[1:18]), color="darkgoldenrod4", size=4) +
  geom_point(aes(x=MOG[1:18], y=NHSpstem[1:18]), color="darkgoldenrod1", size=4) +
  coord_flip()+
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Percentage STEM Post-Secondary Certificates")

Q40[is.na(Q40)]<-0
ggplot(Q40[1:17,]) +
  geom_segment( aes(x=MOG[1:17], xend=MOG[1:17], y=NCDpstem[1:17], yend=CDpstem[1:17]), color="grey") +
  geom_segment( aes(x=MOG[1:17], xend=MOG[1:17], y=NCDpstem[1:17], yend=NHSpstem[1:17]), color="grey") +
  geom_point(aes(x=MOG[1:17], y=NCDpstem[1:17]), color="darkgoldenrod3", size=4) +
  geom_point(aes(x=MOG[1:17], y=CDpstem[1:17]), color="darkgoldenrod4", size=4) +
  geom_point(aes(x=MOG[1:17], y=NHSpstem[1:17]), color="darkgoldenrod1", size=4) +
    coord_flip()+
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Percentage STEM Work Experience Programs")




