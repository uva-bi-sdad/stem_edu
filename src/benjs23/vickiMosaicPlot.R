#Color Blind Palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
wheel <- function(col, radius = 1, ...)
    pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)
cbPalette2 <- c("#999999","#999999","#999999","#009E73","#56B4E9","#E69F00")

#Redo of https://www.nsf.gov/statistics/2017/nsf17310/digest/fod-women/economics.cfm
Year<-rep(rep(c(1995,2004,2014),3),2)
Degree<-rep(c("DM","MM","BM","DW","MW","BW"),rep(3,6))
Degree<-ordered(Degree,levels=c("DM","MM","BM","DW","MW","BW"))
Size<-c(1081,976,1213,2838,3202,4539,19049,24931,31392,
        266,289,415,894,1252,1852,5842,8277,9762)
#Sex<-rep(c("Women","Men"),rep(9,2))
#Degree<-ordered(Sex,levels=c("Women","Men"))


D2015<-xtabs(Size~Year+Degree)
mosaicplot(D2015, color=cbPalette2, cex.axis = 0.01, las = 2, xlab="", ylab="",
           main="Low Participation Field for Women: Economics, 1995, 2004, 2014")


par(mgp=c(3,0.15,0))
axis(side=1,at=c(0.125,0.430,0.790),label=c("29,976","38,927","49,373"),tick=FALSE)
par(mgp=c(3,-0.6,0))
axis(side=2,at=c(0.065,0.185,0.230),label=c("Bachelor","Master","Doctorate"),las=1,tick=FALSE)
axis(side=2,at=c(0.550,0.910,0.990),label=c("Bachelor","Master","Doctorate"),las=1,tick=FALSE)
axis(side=3,at=c(0.125,0.430,0.790),label=c(1995,2004,2014),tick=FALSE)
axis(side=4,at=c(0.10,0.60),label=c("Women","Men"),tick=FALSE)
mtext(side=1,line=2,"Number of Economics Degrees by Year")

text(0.198,-0.017,"19.5%",cex=0.75);text(0.197,0.1845,"3.0%",cex=0.75);text(0.197,0.229,"0.9%",cex=0.75)
text(0.533,-0.017,"21.2%",cex=0.75);text(0.533,0.1995,"3.2%",cex=0.75);text(0.533,0.244,"0.7%",cex=0.75)
text(0.950,-0.017,"24.8%",cex=0.75);text(0.950,0.188,"3.8%",cex=0.75);text(0.950,0.236,"0.8%",cex=0.75)

text(0.198,0.263,"63.5%",cex=0.75);text(0.197,0.879,"9.5%",cex=0.75);text(0.197,0.989,"3.6%",cex=0.75)
text(0.533,0.279,"64.0%",cex=0.75);text(0.533,0.901,"8.2%",cex=0.75);text(0.533,0.995,"2.5%",cex=0.75)
text(0.950,0.274,"64.0%",cex=0.75);text(0.950,0.893,"9.2%",cex=0.75);text(0.950,0.995,"2.5%",cex=0.75)




#Redo of 7-6
News<-rep(c("Current News","Science & Technology","Scientific Issues"),6)
Media<-rep(c("Newspaper","Other Print","Internet","Television","Radio","Other Media"),rep(3,6))
Media<-ordered(Media,levels=c("Newspaper","Other Print","Internet","Television","Radio","Other Media"))
Percent<-c(11,6,3,1,11,10,37,47,67,43,28,13,6,3,1,2,5,6)
F76.plot<-xtabs(Percent~News+Media)
mosaicplot(F76.plot, color=cbPalette, cex.axis = 0.01, main="", xlab="", ylab="")
#par(mgp=c(3,0.15,0))
axis(side=1,at=c(0.15,0.49,0.82),label=c("Scientific Issues","Science & Technology","Current News"),tick=FALSE)
#par(mgp=c(3,-0.6,0))
axis(side=2,at=c(-0.015,0.045,0.300,0.700,0.890,0.950),
     label=c("Newspaper","Other Print","Internet","Television","Radio","Other Media"),las=1,tick=FALSE)
mtext(side=3,line=1,"Figure 7-6. Primary source respondents used to learn
           about current news, science and technology,
            and specific scientific issues (2014).", cex=1.25)


axis(side=3,at=c(0.125,0.430,0.790),label=c(1993,2002,2012),tick=FALSE)
mtext(side=1,line=2,"Number of Women")
text(0.230,-0.017,"3.0%",cex=0.75);text(0.230,0.093,"10.4%",cex=0.75);text(0.230,0.980,"86.6%",cex=0.75)
text(0.550,-0.017,"3.1%",cex=0.75);text(0.550,0.110,"12.1%",cex=0.75);text(0.550,0.980,"84.8%",cex=0.75)
text(0.950,-0.017,"3.8%",cex=0.75);text(0.950,0.155,"15.9%",cex=0.75);text(0.950,0.980,"80.3%",cex=0.75)
