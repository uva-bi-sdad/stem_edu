
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# install.packages("devtools")
library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
# install.packages("psych")
library(psych)
install.packages("pracma")
library(pracma)

#BEFORE I WAS USING THIS FILE, some of the code is broken from this change
# VAHS <- read.xls("VAHS.xls")
VAHS <- read.csv("./data/stem_edu/working/DSPG18/SCHEV/hs_model_data_merged2.csv")
View(VAHS)
colnames(VAHS)
length(unique(VAHS$percentTeachersNotHighlyQualified))  # NOT all == 0 with new data

#Assign the school ID as row name
VAHS <- as.data.frame(VAHS)
rownames(VAHS)<-c(VAHS$id_combine.y)

clean <- VAHS  # Clean the "clean" dataframe in next section
View(clean)

#####   DATA CLEANING   #############################################
##
##  Emily Notes:  Found that PCA Analysis produces misleading results with
##  near-collinear variables.  Our dataset subsets proportions into categories
##  such as M/F, disadv/non-disadv., dropout vs. grad, different kinds of diplomas.
##  These collinearities place higher value on repeated variables and artificially
##  make more "variance explained" go towards earlier principle components
##  Helpful StackExchange post:
##     https://stats.stackexchange.com/questions/50537/should-one-remove-highly-correlated-variables-before-doing-pca


# THE VARIABLE NAMES IN THE FILE BELOW HAVE BEEN REDUCED AND MODIFIED,
#  BUT THE "keep" variables used to subset the data in this analysis are up-to-date.

id <- c("div_name",
        "sch_num",
        "sch_name.x",
        "sch_name.y",
        "sch_name",
        "id_unique",
        "id_combine.y",
        "status",
        "low_grade",
        "high_grade",
        "nces_school_num",
        "school_description",
        "locale1",
        "locale2",
        "lat",
        "long",
        "SchoolName",
        "census_urban_rural")

response <- c("attending_four_year_college_prop",
              "attending_two_year_college_prop",
              "other_continuing_ed_plans_prop",
              "employment_prop",
              "military_prop",
              "no_plans_prop")

# Remove following cols for listed reasons
remove <- c("population",   # Not part of "post-secondary going culture"
            "total_grads",  #collinear with population, not part of "post-secondary going culture"
            "other_diploma_prop",   #collinear with other kinds of diplomas
            "percentTeachersNotHighlyQualified",   # all equal 0
            "total_offenders",     #correlated with population, proportion suspended/expelled better
            "spcl_ed_placement_prop",    #mostly equal 0
            "psat_soph_mean_score",   # high number NA's
            "psat_soph_math_mean",   # high number NA's
            "psat_soph_ebrw_mean",   # high number NA's
            "psat_junior_mean_score",   # high number NA's
            "psat_junior_math_mean",   # high number NA's
            "psat_junior_ebrw_mean",   # high number NA's
            "percentAPTestTakers",  #collinear with course enrollees
            "expulsion_prop",     # collinear with offenses
            "in_school_suspension_prop",     # collinear with offenses
            "lt_suspension_prop",     # collinear with offenses
            "st_suspension_prop",     # collinear with offenses
            "expulsion_prop.1",     # collinear with offenses
            "sat_sub_total_tests_taken", # proportion is better variable
            "disadv_grad_prop",   #collinear with dropout
            "non_disadv_ontime_prop",     #collinear with dropout
            "F_dropout_prop",  #sum with M collinear with dropout rates for disadv vs. nondisadv.
            "M_dropout_prop",     #sum with F collinear with dropout rates for disadv vs. nondisadv.
            "F_ontime_prop",   #on time grad proportion female, collinear with dropout
            "M_ontime_prop",   #on time grad proportion male, collinear with dropout
            "percentTeachersBachelors",    #collinear with grad degree, corr = .96
            "sat_critical_reading_mean",  #corr with sat_math_mean = 0.96
            "sat_writing_mean",    #corr with sat_math_mean = 0.96
            "sat_subj_test_takers")  #corr with sat_test_takers = 0.64

keep <- c("adv_studies_diploma_prop",
          "certificate_of_program_completion_prop",
          "ged_certificate_prop",
          "standard_diploma_prop",     # removed the collinear variable "other_diploma_prop"
          "isaep_prop",
          "offenses_prop",     # removed sub-categories
          "disadv_dropout_prop",
          "non_disadv_dropout_prop",   # corr with disadv dropout = 0.37
          "percentAPCourseEnrollees",  # collinear with AP test takers
          "percentPassedAPTest",     # corr with "percentAPCourseEnrollees" = .6
          "percentGovernorsSchoolEnrollees",
          "percentDualCourseEnrollees",
          "percentCTECompleters",
          "percentTeachersGraduateDegree",  #collinear with percentTeachersBachelorsDegree (-0.96)
          "percentTeachersWProvisionalCredentials",
          "admitRate4YearInstitution",
          "yieldRate4YearInstitution",
          "sat_math_mean",
          "sat_test_takers",
          "psat_soph_test_takers",  #corr with sat_test_takers = 0.53, with psat_junior is 0.43
          "psat_junior_test_takers")   #corr with sat_test_takers = 0.53, with psat_soph is 0.43

#Check for NA values, note they appear as text "NA" so is.na will not flag
na_count <- sapply(clean, function(y) sum(length(which(is.na(y)))))
na_count <- sapply(clean, function(y) sum(length(which(y == "NA"))))
View(na_count)
View(clean)

#  REMOVE 4 TOTAL OBSERVATIONS THAT HAVE NA VALS OR ARE OUTLIERS
#remove outliers found during first PCA, obs. with NA scores
outliers = c(which(VAHS$id_combine.y == 291371), which(VAHS$id_combine.y == 290190), which(VAHS$id_combine.y == 210030))
clean <- clean[-outliers,]

#remove observations with NA SAT scores
naScores = c(which(clean$sat_critical_reading_mean == "NA" | clean$sat_math_mean == "NA" | clean$sat_writing_mean == "NA"))
#clean <- clean[-naScores,] makes all rows disappear after new data doesn't have NA's

# Notice sat scores are character vectors
clean$sat_critical_reading_mean <- as.numeric(clean$sat_critical_reading_mean)
clean$sat_writing_mean <- as.numeric(clean$sat_writing_mean)
clean$sat_math_mean <- as.numeric(clean$sat_math_mean)

#VAHSnotNA gives all observations included in analysis, before subsetting keep cols
VAHSnotNA <- clean
colnames(VAHSnotNA)
View(VAHSnotNA)

# We have a lot of highly correlated variables that will
# screw up the PCA because they are perfectly collinear
cor(VAHSnotNA$non_disadv_dropout_prop, VAHSnotNA$non_disadv_ontime_prop)   #-.93
cor(VAHSnotNA$disadv_dropout_prop, VAHSnotNA$non_disadv_dropout_prop)   #.22 = include these in analysis
#could still include disadv. and non_disadv dropout rates = 0.37
cor(VAHSnotNA$disadv_dropout_prop, VAHSnotNA$disadv_grad_prop)    #-0.713
cor(VAHSnotNA$percentAPCourseEnrollees, VAHSnotNA$percentAPTestTakers)   #very high correlation
cor(VAHSnotNA$percentAPCourseEnrollees, VAHSnotNA$percentPassedAPTest)   #lower correlation
cor(VAHSnotNA$percentTeachersGraduate, VAHSnotNA$percentTeachersBachelors)
cor(VAHSnotNA$disadv_dropout_prop + VAHSnotNA$non_disadv_dropout_prop, VAHSnotNA$F_dropout_prop + VAHSnotNA$M_dropout_prop)
cor(VAHSnotNA$F_ontime_prop, VAHSnotNA$M_ontime_prop)

cor(VAHSnotNA$sat_critical_reading_mean, VAHSnotNA$sat_math_mean)  #0.96
cor(VAHSnotNA$sat_critical_reading_mean, VAHSnotNA$sat_writing_mean)   #0.98
cor(VAHSnotNA$sat_math_mean, VAHSnotNA$sat_writing_mean)    #.96
cor(VAHSnotNA$sat_math_mean, VAHSnotNA$sat_subj_test_takers)   #.69 maybe include both
cor(VAHSnotNA$sat_math_mean, VAHSnotNA$sat_test_takers)   #.38 can include both
cor(VAHSnotNA$sat_subj_test_takers, VAHSnotNA$sat_test_takers)  #.64
cor(VAHSnotNA$sat_test_takers, VAHSnotNA$psat_soph_test_takers)   #.53
cor(VAHSnotNA$sat_test_takers, VAHSnotNA$psat_junior_test_takers)  #.53
cor(VAHSnotNA$psat_soph_test_takers, VAHSnotNA$psat_junior_test_takers)   #.43
#can include sat_math_mean, sat_test_takers, psat_soph_test_takers, psat_junior_test_takers

#graduation rates check
dropOut <- VAHSnotNA$non_disadv_dropout_prop + VAHSnotNA$disadv_dropout_prop
dropOut2 <- VAHSnotNA$F_dropout_prop + VAHSnotNA$M_dropout_prop
c(abs(dropOut - dropOut2) <= 0.05)
#  These 2 dropout rates do not add up, cannot assess overall dropout rate

# Subset to only the columns in "keep" vector

clean <- clean[ , which(names(clean) %in% keep)]

colnames(clean)






############################################################################
###     END OF DATA CLEANING, START PCA ANALYSIS                         ###
############################################################################

#Preliminary PCA
clean.pca<-prcomp(clean, scale.=TRUE, center=TRUE)

#eigenvalues
clean.pca$sdev^2

#rotation = loadings = eigenvectors: linear combos of schools for each PC
clean.pca$rotation

# sum(clean.pca$rotation[1])
#scores = score i for row vector/ obs. X_{l.} is distance to PC_i
clean.pca$x

rownames(clean.pca$x)<-c(VAHSnotNA$id_combine.y)

View(clean.pca$x)

#Color Blind Palettes
cbPalette<-c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2",
             "#D55E00","#CC79A7","#000000","#FFFFFF")
wheel<-function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)


g<-ggbiplot(clean.pca, obs.scale=1, var.scale=1,
            groups=VAHSnotNA$census_urban_rural, ellipse=TRUE, cex=3,
            circle=TRUE) +
  scale_colour_manual(values=c(cbPalette[3],cbPalette[2])) +  #City=mustard/Rural=Light Blue
  theme(legend.direction='horizontal', legend.position='top')
print(g)


#Plot the Scores

#create data frame with scores
scores <- data.frame(urbRural = VAHSnotNA$census_urban_rural, clean.pca$x)
#plot of observations
ggplot(data=scores,
       aes(x=PC1, y=PC2,
           fill=factor(urbRural))) + coord_fixed() +
  geom_point(shape=21, colour="black", size=3.5, stroke=1) +
  scale_fill_manual(values=cbPalette[c(3,2)]) +
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  ggtitle("High School PCA Scores Identified by Area")

#  3 outliers found here were removed from dataset:
#  School ID's of removed observations were 291371, 290190, 210030



head(sort(clean.pca$rotation[,1]));tail(sort(clean.pca$rotation[,1]))

#PC1:  Vicki's original findings for most important variables
#DropoutMale     DropoutFemale   NonDisadvDropout
#-0.2308030      -0.2193240      -0.2082227
#APCourseEnrollees   APTestTakers     GradsFemale     GradsMale
#0.2265214           0.2282923        0.2349878       0.2374311
#StdDipl       AdvStudiesDipl
#0.2654732     0.2654732

head(sort(clean.pca$rotation[,2]));tail(sort(clean.pca$rotation[,2]))
#PC2
#TeachersBachelors  DualCourseEnrollees   CTECompleters
#-0.2574519         -0.2555276            -0.2391616
#Admit4YrInstitution    NonDisadvGrads      GradsMale
#-0.2331841             -0.2255711          -0.2183348
#DisadvDropout      DropoutMale
#0.2187525          0.2251874
#NonDisadvDropout   TeachersGradDegree      DropoutFemale
#0.2260329          0.2344291               0.2379342

#Scores for PC1
sort(clean.pca$x[,1])


View(clean.pca$rotation)
#Varimax rotation
rawLoadings<-clean.pca$rotation %*% diag(clean.pca$sdev)
ncol(clean)
rotatedLoadings<-varimax(rawLoadings)$loadings
invLoadings<-t(pracma::pinv(rotatedLoadings))
rotatedscores<-scale(clean) %*% invLoadings
rownames(rotatedscores)<-c(VAHSnotNA$id_combine.y)
#scores computed via rotated loadings
sort(rotatedscores[,c(1)])

