

###   Principle Component Analysis on Schev Schools Data
###   Emily Sheen
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(factoextra)

#Load the SCHEV data

schev <- read_csv("data/stem_edu/original/schev-data/va_hs_ps_traj_model.csv")
View(schev)
colnames(schev)
length(schev$sch_num)
ncol(schev)


#################    DATA CLEANING    ##########################

schev <- as.data.frame(schev)
length(unique(schev$id_unique))  #id_unique uniquely identifies schools, assign this value as row name

abs(schev$expulsion_prop - schev$expulsion_prop.1) <= .18   #two different expulsion variables

#make row names = id_unique
row.names(schev) <- schev$id_unique

# Remove school identifier information for PCA, create new data.frame without identifiers
keepVars <- c("total_grads",
              "adv_studies_diploma_prop",
              "other_diploma_prop",
              "certificate_of_program_completion_prop",
              "ged_certificate_prop",
              "standard_diploma_prop",
              "isaep_prop",
              "attending_four_year_college_prop",
              "attending_two_year_college_prop",
              "other_continuing_ed_plans_prop",   #collinear with other postsecondary options
              "employment_prop",
              "military_prop",
              "no_plans_prop",
              "population",
              "offenses_prop",
              "expulsion_prop",
              "in_school_suspension_prop",
              "lt_suspension_prop",
              "st_suspension_prop",
              "expulsion_prop.1",
              "spcl_ed_placement_prop",
              "non_disadv_dropout_prop",
              "disadv_dropout_prop",
              "non_disadv_grad_prop",
              "disadv_grad_prop",
              "dropout_prop_F",
              "dropout_prop_M",
              "grad_prop_F",
              "grad_prop_M",
              "percentAPCourceEnrollees",
              "percentAPTestTakers",
              "percentGovernorsSchoolEnrollees",
              "percentDualCourseEnrollment",
              "percentCTECompleters",
              "percentTeachersBachelors",
              "percentTeachersGraduateDegree",
              "percentTeachersWProvisionalCredentials",
              "percentPassedAPTest",
              "admitRate4YearInstitution",
              "yieldRate4YearInstitution")

idVars <- c("div_name",
            "sch_num",
            "sch_name",
            "id_unique",
            "id_combine",
            "status",
            "low_grade",
            "high_grade",
            "nces_school_num",
            "school_description",
            "locale1",
            "locale2",
            "lat",
            "long",
            "SchoolName")

removeVars <- c("total_offenders",  # collinear with offenses proportion
                "percentTeachersNotHighlyQualified")   # all == 0

length(keepVars) + length(removeVars) + length(idVars)

schevPCA <- subset(schev, select = keepVars)
#View(schevPCA)

map(schevPCA, ~sum(is.na(.)))  #counts NA values for each variable: no NAs


#Clean and organize data into response and predictor variables
responseColnames <- c("attending_two_year_college_prop","attending_four_year_college_prop",
                    "other_continuing_ed_plans_prop", "employment_prop","military_prop","no_plans_prop")
responses <- select(schev, responseColnames)

predictors<-select(schev, -c(which(colnames(schev) %in% idVars), which(colnames(schev) %in% removeVars), which(colnames(schev) %in% responseColnames)))
colnames(predictors)

#NOTES ON PCA
# Spectral decomposition examines the covariances / correlations between variables
# Singular value decomposition examines the covariances / correlations between individuals
# princomp() uses the spectral decomposition approach (covariance between variables).
# The functions prcomp() and PCA()[FactoMineR] use the singular value decomposition (SVD).

# According to the R help, SVD (covariance between individuals) has slightly better
# numerical accuracy. Therefore, the function prcomp() is preferred compared to princomp().

#Preliminary PCA
pgc.pca <- prcomp(predictors, scale.=TRUE, center=TRUE)
pgc.varPCA <- princomp(predictors, cor = TRUE)
#eigenvalues
pgc.pca$sdev^2
#loadings
pgc.pca$rotation
#scores
pgc.pca$x

library(psych)
library(pracma)

#Varimax rotation
rawLoadings <- pgc.pca$rotation[,1:21] %*% diag(pgc.pca$sdev, 21, 21)
rotatedLoadings<-varimax(rawLoadings)$loadings
invLoadings<-t(pracma::pinv(rotatedLoadings))
rotatedscores<-scale(pgc[,-1]) %*% invLoadings
#scores computed via rotated loadings
print(rotatedscores[,c(1,2,3)])

#Color Blind Palettes
cbPalette<-c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2",
             "#D55E00","#CC79A7","#000000","#FFFFFF")
wheel<-function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)
#Get the Hex and Decimal ID for a Color
GetColorHexAndDecimal <- function(color)
{
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}


library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
g<-ggbiplot(pgc.pca, obs.scale=1, var.scale=1,
            groups=pgc$`Study Area`, ellipse=TRUE,
            circle=TRUE)
g<-g+scale_colour_manual(values=c(cbPalette[6], cbPalette[7]))
g<-g+theme(legend.direction='horizontal',
           legend.position='top')
print(g)

#Plot the Scores
library(ggplot2)
#create data frame with scores
scores<-as.data.frame(pgc.pca$x)
#plot of observations
ggplot(data=scores, aes(x=PC1, y=PC2, label=rownames(pgc))) +
  geom_point(data=scores, aes(x=PC1, y=PC2)) +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  geom_text(colour=c(rep(cbPalette[7],12),rep(cbPalette[6],9)), alpha=0.8, size=6) +
  ggtitle("PCA Scores plot of School Areas")

#Plot Correlations
circle<-function(center=c(0, 0), npoints=100) {
  r<-1
  tt<-seq(0, 2 * pi, length=npoints)
  xx<-center[1] + r*cos(tt)
  yy<-center[1] + r*sin(tt)
  return(data.frame(x=xx, y=yy))
}
corcir<-circle(c(0, 0), npoints=100)

#create data frame with correlations between variables and PCs
correlations<-as.data.frame(cor(pgc[,-1], pgc.pca$x))
groups.pgc<-c(rep("Diploma",6),rep("Discipline",7),rep("Timeliness",8),
              rep("Programs",3),rep("Teachers",4),rep("Availability",2),
              rep("Tests",11))

#data frame with arrows coordinates
arrows<-data.frame(x1=rep(0, 41), y1=rep(0, 41),
                   x2=correlations$PC1, y2=correlations$PC2, labels=groups.pgc)

#geom_path will do open circles
q<-ggplot() + geom_path(data=corcir, aes(x=x, y=y), colour="gray65") +
  coord_fixed(ratio=1) +
  scale_colour_manual(values=c(cbPalette)) +
  labs(x="PC1 Axis", y="PC2 Axis") +
  ggtitle("Circle of Correlations: Variables versus PC")

q + geom_segment(data=arrows, aes(x=x1, y=y1, xend=x2, yend=y2, color=labels), size=2) +
  geom_hline(yintercept=0, colour="black", lty=8) +
  geom_vline(xintercept=0, colour="black", lty=8)


#Do a heatmap of the  raw loadings
load.pgc<-unname(pgc.pca$rotation[,c(1:9)])
discrete.pgc<-data.frame(ifelse(load.pgc<=-0.20,-1,ifelse(load.pgc>=0.2,1,0)))
colnames(discrete.pgc)<-paste0("PC", 1:9)
discrete.pgc<-discrete.pgc %>% mutate(Variable=paste0("V", 1:41))
pgc.tidy<-discrete.pgc %>% tidyr::gather(PC, mutated, 1:9)
pgc.tidy$Variable<-factor(pgc.tidy$Variable, levels=paste0("V", 1:41))
pgc.tidy$PC<-factor(pgc.tidy$PC, levels=paste0("PC", 1:9))
#change the levels for X names and sample names so it goes 1,2,3,4... rather than 1, 10...
gg<-ggplot(pgc.tidy, aes(x=Variable, y=PC, fill=as.factor(mutated))) +
  coord_equal() +
  geom_tile(color="white", size=0.5) +
  scale_fill_manual(values=cbPalette[c(7,1,6)]) +
  labs(x="", y="") +
  theme(legend.position="none")
gg+geom_vline(xintercept=c(6.5,13.5,21.5,24.5,28.5,30.5),lty=8) +
  scale_x_discrete(labels=c("","","","Diploma","","",
                            "","","","Discipline","","","",
                            "","","","","Timeliness","","","",
                            "","Programs","",
                            "","Teachers","","",
                            "","Availability",
                            "","","","","","Tests","","","","",""))


############################################################################
##Graphic for the paper - use unrotated PCs
############################################################################
#############################
#Select the variable with correlation > 0.75

circle<-function(center=c(0, 0), npoints=100) {
  r<-1
  tt<-seq(0, 2 * pi, length=npoints)
  xx<-center[1] + r*cos(tt)
  yy<-center[1] + r*sin(tt)
  return(data.frame(x=xx, y=yy))
}
corcir<-circle(c(0, 0), npoints=100)

#S1<-c(0.16,0,0,-0.05,0,0.2,0.09,-0.02,-0.058,-0.1,0,0,0,0,0.09,0.11,0.2,-0.15,0.13)
#S2<-c(0.01,0.05,0.05,0.01,-0.025,-0.006,-0.04,0.015,0.015,-0.035,-0.035,-0.03,-0.02,-0.02,0.045,0,0.07,-0.02,0.04)

#create data frame with correlations between variables and PC loadings
groups.pgc<-c(rep("Diploma",6),rep("Discipline",7),rep("Timeliness",8),
              rep("Programs",3),rep("Teachers",4),rep("Availability",2),
              rep("Tests",11))
#Standardize the PC loadings
temp<-pgc.pca$rotation[,c(1,2)];temp<-unname(temp);temp<-as.data.frame(temp)
temp[,1]<-(temp[,1]-mean(temp[,1]))/sd(temp[,1]);temp[,2]<-(temp[,2]-mean(temp[,2]))/sd(temp[,2])
names(temp)<-c("PC1","PC2")

temp34<-pgc.pca$rotation[,c(3,4)];temp34<-unname(temp34);temp34<-as.data.frame(temp34)
temp34[,1]<-(temp34[,1]-mean(temp34[,1]))/sd(temp34[,1]);temp34[,2]<-(temp34[,2]-mean(temp34[,2]))/sd(temp34[,2])
names(temp34)<-c("PC1","PC2")

high.loadings<-temp[c(1:5,9,11,12,13:22,24:27,30:41),]
#data frame with arrows coordinates
high.arrows<-data.frame(x1=rep(0, 41), y1=rep(0, 41),
                        x2=temp[,1], y2=temp[,2], labels=groups.pgc)
arrow.names<-c("Advanced Study Diploma","Standard Diploma","Other Diploma","COPC","GED","ISAEP",
               "Explusion","Lon-term Suspension","Short-term Suspension","In-school Suspension","Explusion to Suspension","Special Ed.","Disciplinary Actions",
               "Non-DA Dropout","DA Dropout","Dropout","","Non-DA On-time","DA Graduation","Female On-time","Male On-time",
               "Dual Course Enrollees","Governor School Enrollees","CTE Completion",
               "Teachers with Grad. Degree","Teachers with Bachelor Degree","Provisional Credit Teachers","Not Highly Qualified Teachers",
               "4-Yr Acceptance Rate","4-Yr Yield",
               "AP Test Takers","AP Enrollees","AP Pass Rate","PSAT Junior Takers","PSAT Sophomore Takers","SAT Critical Reading Mean",
               "SAT Math Mean","SAT Subject Takers","SAT Subject Taken","SAT Test Takers","SAT Writing Mean")
PCLoadings<-ggplot() + geom_path(data=corcir, aes(x=x, y=y), colour="black") +
  geom_hline(yintercept=0, colour="black", lty=8) +
  geom_vline(xintercept=0, colour="black", lty=8) +
  geom_segment(data=high.arrows, aes(x=x1, y=y1, xend=x2, yend=y2, color=labels), size=1.5,
               arrow=arrow(length=unit(0.3, "cm"))) +
  coord_fixed(ratio=1) +
  scale_colour_manual(values=c(cbPalette[c(1,2,3,4,6,7,8)])) +
  labs(x="First Principal Component Standardized Loading", y="Second Principal Component Standardized Loading") +
  ggtitle("") + theme(legend.position="none") +
  #(++) and (+-)
  annotate("text", x=temp$PC1[c(1,11,18:22,25,31:41)]+c(-0.75,-0.25,-0.5,0,-0.4,-0.4,0,0,0,-0.05,rep(0,6),0.05,0,0),
           y=temp$PC2[c(1,11,18:22,25,31:41)]+c(0.09, 0.06, rep(0,7),0.03,-0.05,0,-0.07,0,0, -0.05, 0.05, 0, -0.05),
           label=arrow.names[c(1,11,18:22,25,31:41)], size=4,
           hjust=0, colour="black") +
  annotate("text", x=temp$PC1[c(2:6,9,12:17,24,26,27,30)],
           y=temp$PC2[c(2:6,9,12:17,24,26,27,30)],
           label=arrow.names[c(2:6,9,12:17,24,26,27,30)], size=4,
           hjust=1, colour="black") +
  annotate("text", x=-1.99, y=-1.17, label="Legend", size=5,
           hjust=0, colour="black") +
  annotate("text", x=-1.85, y=-1.9, label="Programs", size=5,
           hjust=0, colour=cbPalette[9]) + geom_point(aes(x=-1.95, y=-1.9), pch=22, size=8, fill=cbPalette[4]) +
  annotate("text", x=-1.85, y=-1.8, label="Diploma Types", size=5,
           hjust=0, colour=cbPalette[9]) + geom_point(aes(x=-1.95, y=-1.8), pch=22, size=8, fill=cbPalette[2]) +
  annotate("text", x=-1.85, y=-1.7, label="Postsecondary Rate", size=5,
           hjust=0, colour=cbPalette[9]) + geom_point(aes(x=-1.95, y=-1.7), pch=22, size=8, fill=cbPalette[1]) +
  annotate("text", x=-1.85, y=-1.6, label="Teacher Credentials", size=5,
           hjust=0, colour=cbPalette[9]) + geom_point(aes(x=-1.95, y=-1.6), pch=22, size=8, fill=cbPalette[6]) +
  annotate("text", x=-1.85, y=-1.5, label="Disciplinary Actions", size=5,
           hjust=0,colour=cbPalette[9]) + geom_point(aes(x=-1.95, y=-1.5), pch=22, size=8, fill=cbPalette[3]) +
  annotate("text", x=-1.85, y=-1.4, label="Graduation Timeliness", size=5,
           hjust=0,colour=cbPalette[9]) + geom_point(aes(x=-1.95, y=-1.4), pch=22, size=8, fill=cbPalette[8]) +
  annotate("text", x=-1.85, y=-1.3, label="College Admission Tests", size=5,
           hjust=0,colour=cbPalette[9]) + geom_point(aes(x=-1.95, y=-1.3), pch=22, size=8, fill=cbPalette[7]) +
  theme(axis.title=element_text(size=16, hjust=0.5),
        axis.text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))
PCLoadings



############################################################################
##Graphic for the paper - use UNrotated PCs
############################################################################
#############################
#Elipses plus the HS PC Scores
#scores<-as.data.frame(pgc.pca$x[,c(1,2)])
#scores<-data.frame(scores, Type=as.factor(c(rep("Rural",12),rep("Urban",9))))
HS<-c("Bland","Grundy","Hurley","Twin","Council","Powhatan","Cave","Hidden",
      "Glenvar","Northside","William Byrd","Sussex","Armstrong","Thomas Jefferson",
      "John Marshall","Richmond","George Wythe","Huguenot","Open","William Fleming",
      "Patrick Henry")
S1<-c(1.1,0,0,0.8,1.4,0,-0.19,0,0,0,2.2,0.1,0,3.2,2.33,0,0,0,0,0,0)
S2<-c(0.2,0,0,0.4,0,0,0.3,0,0.2,0.3,0,0,0,0,0,0,0,0,0,0,0)
PCScores<-ggplot() + coord_equal() +
  scale_colour_manual(values=c(cbPalette)) +
  labs(x="First Principal Component Score", y="Second Principal Component Score") +
  ggtitle("") +
  #theme_bw() +
  geom_hline(yintercept=0, colour="black", lty=8) +
  geom_vline(xintercept=0, colour="black", lty=8) +
  geom_point(data=scores[c(1:12),], aes(x=PC1, y=PC2), size=3, colour=cbPalette[3]) +
  stat_ellipse(data=scores[c(1:12),], aes(x=PC1, y=PC2, colour=Type), type="norm", colour=cbPalette[3], size=2) +
  geom_point(data=scores[c(13:21),], aes(x=PC1, y=PC2), size=3, colour=cbPalette[2]) +
  stat_ellipse(data=scores[c(13:21),], aes(x=PC1, y=PC2), type="norm", colour=cbPalette[2], size=2) +
  annotate("text", x=scores$PC1+0.03+S1, y=scores$PC2+S2, label=HS, vjust=1, hjust=1) +
  annotate("text", x=-16, y=8.0, label="Rural - Light Blue Circle", size=5,
           hjust=0,colour=cbPalette[3]) +
  annotate("text", x=-16, y=7.3, label="Urban - Mustard Circle", size=5,
           hjust=0, colour=cbPalette[2]) +
  theme(axis.title=element_text(size=16, hjust=0.5),
        axis.text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))
PCScores



#######################################################################################
#Varimax Rotated Analyses
#######################################################################################
correlations<-as.data.frame(cor(pgc[,-1], rotatedscores))

#Do a heatmap of the Varimax rotated loadings
rotload.pgc<-unname(rotatedLoadings[,c(1:9)])
rotload.tr<-data.frame(ifelse(rotload.pgc<=-0.30,-1,ifelse(rotload.pgc>=0.3,1,0)))
colnames(rotload.tr)<-paste0("PC", 1:9)
rotload.tr<-rotload.tr %>% mutate(Variable=paste0("V", 1:41))
rotload.tidy<-rotload.tr %>% tidyr::gather(PC, mutated, 1:9)
rotload.tidy$Variable<-factor(rotload.tidy$Variable, levels=paste0("V", 1:41))
rotload.tidy$PC<-factor(rotload.tidy$PC, levels=paste0("PC", 1:9))
#change the levels for X names and sample names so it goes 1,2,3,4... rather than 1, 10...
gg<-ggplot(rotload.tidy, aes(x=Variable, y=PC, fill=as.factor(mutated))) +
  coord_equal() +
  geom_tile(color="white", size=0.5) +
  scale_fill_manual(values=cbPalette[c(7,1,6)]) +
  labs(x="", y="") +
  theme(legend.position="none")
gg+geom_vline(xintercept=c(6.5,13.5,21.5,24.5,28.5,30.5),lty=8) +
  scale_x_discrete(labels=c("","","","Diploma","","",
                            "","","","Discipline","","","",
                            "","","","","Timeliness","","","",
                            "","Programs","",
                            "","Teachers","","",
                            "","Availability",
                            "","","","","","Tests","","","","",""))

#Plot the rotated Scores
library(ggplot2)
#create data frame with scores
rotatedscores.df<-as.data.frame(rotatedscores)
#plot of observations
ggplot(data=rotatedscores.df, aes(x=V1, y=V2,
                                  label=rownames(rotatedscores.df))) +
  geom_point(data=rotatedscores.df, aes(x=V1, y=V2)) +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  labs(x="PC1", y="PC2") +
  geom_text(colour=c(rep(cbPalette[7],12),rep(cbPalette[6],9)), alpha=0.8, size=6) +
  ggtitle("PC Varimax Rotated Scores identified by School Areas (Urban (Blue) / Rural (Orange)")


#Plot Rotated Correlations
circle<-function(center=c(0, 0), npoints=100) {
  r<-1
  tt<-seq(0, 2 * pi, length=npoints)
  xx<-center[1] + r*cos(tt)
  yy<-center[1] + r*sin(tt)
  return(data.frame(x=xx, y=yy))
}
corcir<-circle(c(0, 0), npoints=100)

#create data frame with correlations between variables and PCs
correlations<-as.data.frame(cor(pgc[,-1], rotatedscores))
groups.pgc<-c(rep("Diploma",6),rep("Discipline",7),rep("Timeliness",8),
              rep("Programs",3),rep("Teachers",4),rep("Availability",2),
              rep("Tests",11))

#data frame with arrows coordinates
arrows<-data.frame(x1=rep(0, 41), y1=rep(0, 41),
                   x2=correlations$V1, y2=correlations$V2, labels=groups.pgc)

#geom_path will do open circles
q<-ggplot() + geom_path(data=corcir, aes(x=x, y=y), colour="gray65") +
  coord_fixed(ratio=1) +
  scale_colour_manual(values=c(cbPalette)) +
  labs(x="PC1 Axis", y="PC2 Axis") +
  ggtitle("Circle of Correlations: Variables versus Rotated PC")

q + geom_segment(data=arrows, aes(x=x1, y=y1, xend=x2, yend=y2, color=labels), size=2) +
  geom_hline(yintercept=0, colour="black", lty=8) +
  geom_vline(xintercept=0, colour="black", lty=8)

library(ggbiplot)
g<-ggbiplot(pgc.pca, obs.scale=1, var.scale=1,
            groups=pgc$`Study Area`, ellipse=TRUE,
            circle=TRUE)
g<-g+scale_colour_manual(values=c(cbPalette[6], cbPalette[7]))
g<-g+theme(legend.direction='horizontal',
           legend.position='top')
print(g)




############################################################################
##Graphic for the paper - use rotated PCs
############################################################################
#############################
#Select the variable with correlation > 0.75

circle<-function(center=c(0, 0), npoints=100) {
  r<-1
  tt<-seq(0, 2 * pi, length=npoints)
  xx<-center[1] + r*cos(tt)
  yy<-center[1] + r*sin(tt)
  return(data.frame(x=xx, y=yy))
}


S1<-c(0.16,0,0,-0.05,0,0.2,0.09,-0.02,-0.058,-0.1,0,0,0,0,0.09,0.11,0.2,-0.15,0.13)
S2<-c(0.01,0.05,0.05,0.01,-0.025,-0.006,-0.04,0.015,0.015,-0.035,-0.035,-0.03,-0.02,-0.02,0.045,0,0.07,-0.02,0.04)

#create data frame with correlations between variables and PC loadings
groups.pgc<-c(rep("Diploma",6),rep("Discipline",7),rep("Timeliness",8),
              rep("Programs",3),rep("Teachers",4),rep("Availability",2),
              rep("Tests",11))
high.loadings<-rotatedLoadings[c(1,2,3,5,11,13,14,16,17,18,20,21,27,30,31,32,34,35,40),c(1:2)]
Categories<-groups.pgc[c(1,2,3,5,11,13,14,16,17,18,20,21,27,30,31,32,34,35,40)]

#data frame with arrows coordinates
high.arrows<-data.frame(x1=rep(0, 19), y1=rep(0, 19),
                        x2=high.loadings[,1], y2=high.loadings[,2], labels=Categories)
arrow.names<-c("Adv. Study Diploma","Std. Diploma","Other Diploma","GED","EXP1","Disciplinary Actions",
               "Non-Disadvantaged Dropout","Female Dropout","Male Dropout","Non-Disadvantaged On-time","Female On-time",
               "Male On-Time","Teachers with Provisional Credits","4-Yr Acceptance Rate","AP Takers","AP Enrollees",
               "PSAT JR Takers","PSAT SOPH Takers","SAT Takers")
q<-ggplot() + geom_path(data=corcir, aes(x=x, y=y), colour="gray65") +
  geom_segment(data=high.arrows, aes(x=x1, y=y1, xend=x2, yend=y2, color=labels), size=2,
               arrow=arrow(length=unit(0.3, "inches"))) +
  coord_fixed(ratio=1) +
  scale_colour_manual(values=c(cbPalette[c(1,2,3,4,6,7,8)])) +
  labs(x="First Principal Component Loading", y="Second Principal Component Loading") +
  ggtitle("") +
  geom_hline(yintercept=0, colour="black", lty=8) +
  geom_vline(xintercept=0, colour="black", lty=8) +
  annotate("text", x=high.arrows$x2+S1, y=high.arrows$y2+S2, label=arrow.names, vjust=1) +
  theme(legend.position="none") +
  annotate("text", x=0.05, y=0.7, label="Legend", size=5,
           hjust=0, colour="black") +
  annotate("text", x=0.05, y=0.6, label="Diploma Types - Mustard Line", size=5,
           hjust=0, colour=cbPalette[2]) +
  annotate("text",  x=0.05, y=0.5, label="Postsecondary Rate - Grey Line", size=5,
           hjust=0, colour=cbPalette[1]) +
  annotate("text",  x=0.05, y=0.4, label="Teacher Credentials - Green Line", size=5,
           hjust=0, colour=cbPalette[4]) +
  annotate("text", x=0.05, y=0.3, label="Disciplinary Actions - Light Blue Line", size=5,
           hjust=0,colour=cbPalette[3]) +
  annotate("text", x=0.05, y=0.2, label="Graduation Timeliness - Orange Line", size=5,
           hjust=0,colour=cbPalette[7]) +
  annotate("text", x=0.05, y=0.1, label="College Admission Tests - Dark Blue Line", size=5,
           hjust=0,colour=cbPalette[6]) +
  theme(axis.title=element_text(size=16, hjust=0.50))
q

#############################
#Elipses plus the HS PC Scores
scores<-as.data.frame(rotatedscores[,c(1,2)])
scores<-data.frame(scores, Type=as.factor(c(rep("Rural",12),rep("Urban",9))))
HS<-c("Bland","Grundy","Hurley","Twin","Council","Powhatan","Cave","Hidden",
      "Glenvar","Northside","William Byrd","Sussex","Armstrong","Thomas Jefferson",
      "John Marshall","Richmond","George Wythe","Huguenot","Open","William Fleming",
      "Patrick Henry")
S2<-c(0,0,0,-0.06,0,0,0,-0.04,0,0.05,-0.09,0,-0.05,0,0,0,0,0,0,0,-0.06)
q<-ggplot() + coord_fixed(ratio=1) +
  scale_colour_manual(values=c(cbPalette)) +
  labs(x="First Principal Component Score", y="Second Principal Component Score") +
  ggtitle("") +
  geom_hline(yintercept=0, colour="black", lty=8) +
  geom_vline(xintercept=0, colour="black", lty=8) +
  geom_point(data=scores[c(1:12),], aes(x=V1, y=V2), size=3, colour=cbPalette[3]) +
  stat_ellipse(data=scores[c(1:12),], aes(x=V1, y=V2, colour=Type), type="norm", colour=cbPalette[3], size=2) +
  geom_point(data=scores[c(13:21),], aes(x=V1, y=V2), size=3, colour=cbPalette[2]) +
  stat_ellipse(data=scores[c(13:21),], aes(x=V1, y=V2), type="norm", colour=cbPalette[2], size=2) +
  annotate("text", x=scores$V1+0.03, y=scores$V2+S2, label=HS, vjust=0, hjust=0) +
  annotate("text", x=-4, y=-3.5, label="Rural - Light Blue Circle", size=5,
           hjust=0,colour=cbPalette[3]) +
  annotate("text", x=-4, y=-3.7, label="Urban - Mustard Circle", size=5,
           hjust=0, colour=cbPalette[2]) +
  theme(axis.title=element_text(size=16, hjust=0.60))
q




