

### K-means clustering of SCHEV Schools Data
### Emily Sheen

library(viridis)
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(cluster)    # clustering algorithms
install.packages("factoextra")
library(factoextra) # clustering algorithms & visualization
library(ggmap)
library(gridExtra)

#  Load Schools Data

schev <- read_csv("./data/stem_edu/working/DSPG18/SCHEV/hs_model_data_merged5.csv")
# schev <- subset(schev, select = -c(locale1, locale2))

colnames(schev)

###  T
pcaVars <- c("adv_studies_diploma_prop",
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


idVars <- c("id_combine.y",
            "div_name",
            "sch_name",
            "long",
            "lat",
            "census_urban_rural")

predictorVars <- c("standard_diploma_prop",
                   "adv_studies_diploma_prop",
                   "other_diploma_prop",
                   "certificate_of_program_completion_prop",
                   "ged_certificate_prop",
                   "isaep_prop",
                   "expulsion_prop",
                   "in_school_suspension_prop",
                   "lt_suspension_prop",
                   "expulsion_prop.1",
                   "st_suspension_prop",
                   "spcl_ed_placement_prop",
                   "offenses_prop",
                   "non_disadv_ontime_prop",
                   "disadv_grad_prop",
                   "non_disadv_dropout_prop",
                   "disadv_dropout_prop",
                   "F_ontime_prop",
                   "M_ontime_prop",
                   "F_dropout_prop",
                   "M_dropout_prop",
                   "percentAPCourseEnrollees",
                   "percentAPTestTakers",
                   "percentGovernorsSchoolEnrollees",
                   "percentDualCourseEnrollees",
                   "percentTeachersNotHighlyQualified",
                   "percentTeachersWProvisionalCredentials",
                   "percentCTECompleters",
                   "percentPassedAPTest",
                   "sat_test_takers",
                   "sat_critical_reading_mean",
                   "sat_math_mean",
                   "sat_writing_mean",
                   "sat_subj_test_takers",
                   "sat_subj_total_tests_taken",
                   "psat_soph_test_takers",
                   "psat_junior_test_takers",
                   "admitRate4YearInstitution",
                   "yieldRate4YearInstitution",
                   "percentTeachersBachelors",
                   "percentTeachersGraduateDegree",
                   "ontime_prop",
                   "dropout_prop",
                   "F_cohort_prop",
                   "M_cohort_prop",
                   "nondisadvantaged_cohort_prop",
                   "disadvantaged_cohort_prop")

responseVars <- c("attending_two_year_college_prop",
                  "attending_four_year_college_prop",
                  "other_continuing_ed_plans_prop",
                  "employment_prop",
                  "military_prop",
                  "no_plans_prop")

schevClust <- subset(schev, select = predictorVars)

View(schevClust)

# # change percents to proportions
# unique(schevClust$percentTeachersBachelors)
# schevClust$percentTeachersBachelors <- schevClust$percentTeachersBachelors/100
# schevClust$percentTeachersGraduateDegree <- schevClust$percentTeachersGraduateDegree/100
# schevClust$percentTeachersWProvisionalCredentials <- schevClust$percentTeachersWProvisionalCredentials/100
# schevClust$percentPassedAPTest <- schevClust$percentPassedAPTest/100

na_count <- sapply(schev, function(y) sum(length(which(is.na(y)))))
na_count <- sapply(clean, function(y) sum(length(which(y == "NA"))))
View(na_count)   #No NA values

schevNotNA <- na.omit(schevClust)

# Scale all variables
schevNotNA <- scale(schevNotNA)

# Choose a distance measure
distance <- get_dist(schevNotNA, method = "euclidean") #methods are euclidean, pearson, spearmen, etc.
fviz_dist(distance, show_labels = TRUE)

k2 <- kmeans(schevNotNA, centers = 2, nstart = 25)
str(k2)
k2$centers
k2$cluster
length(k2$cluster)

fviz_cluster(k2, data = schevNotNA)


### TRY OTHER NUMBERS OF CLUSTERS

k3 <- kmeans(schevNotNA, centers = 3, nstart = 25)
k4 <- kmeans(schevNotNA, centers = 4, nstart = 25)
k5 <- kmeans(schevNotNA, centers = 5, nstart = 25)
k6 <- kmeans(schevNotNA, centers = 6, nstart = 25)

# plots to compare
p2 <- fviz_cluster(k2, geom = "point", data = schevNotNA) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = schevNotNA) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = schevNotNA) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = schevNotNA) + ggtitle("k = 5")
p6 <- fviz_cluster(k5, geom = "point",  data = schevNotNA) + ggtitle("k = 6")

grid.arrange(p2, p3, p4, p5, p6, nrow = 2)


#######################################################################
### To determine the appropriate number of clusters, we can use     ###
###   1) Elbow method: plot SSTOT by # clusters, choose k at bend   ###
###   2) Silhouette method                                         ###
###   3) Gap statistic                                              ###
#######################################################################

### ELBOW METHOD: plot the "within cluster sum of squared distances against the # clusters K

fviz_nbclust(schevNotNA, kmeans, method = "wss", k.max = 15)  #Notice bend at elbow at 2-3 clusters

#  fvis_nbclust function is essentially mapping the number of clusters k
#  against the total within cluster sum of squared distances (sq. distance
#  between each observation and the cluster center)

#ELBOW METHOD BY HAND:
library(purrr)

# function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(schevNotNA, k, nstart = 30)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Optimal number of clusters = 2



### SILHOUETTE METHOD: silhouette method measures how well an observation lies within its cluster
# and how far apart it lies from other clusters.  A higher average silhouette value equates to
# more optimal number of clusters.  https://en.wikipedia.org/wiki/Silhouette_(clustering)

fviz_nbclust(schevNotNA, kmeans, method = "silhouette")  #notice optimal silhouette at 2 clusters


#SILHOUETTE METHOD BY HAND: Optimal Number of cluster is k=2
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(schevNotNA, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(schevNotNA))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


## Gap Statistic Method
# compute gap statistic
gap_stat <- clusGap(schevNotNA, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 50)
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)


####  THE OPTIMAL NUMBER OF CLUSTERS K =2, suggested by all three methods



final <- kmeans(schevNotNA, 2, nstart = 25)
print(final)

fviz_cluster(final, data = schevNotNA)

#Append final cluster information to dataset

#add columns for school identifier variables
clustered <- schevNotNA %>%
  as_tibble() %>%
  mutate(div_name = schev$div_name,
         sch_name = schev$sch_name,
         urban_rural = schev$census_urban_rural,
         lat = schev$lat,
         long = schev$long,
         cluster = final$cluster)
colnames(clustered)
View(clustered)



clus1 <- filter(clustered, cluster == 1)
clus2 <- filter(clustered, cluster == 2)

# SOME CLUSTER PLOTS
ggplot(data = clustered, aes(adv_studies_diploma_prop, disadv_dropout_prop, color = factor(cluster))) +
  geom_point()

theme_set(theme_bw())

#install 'ggalt' pkg
# devtools::install_github("hrbrmstr/ggalt")

theme_set(theme_classic())
library(ggalt)
library(RColorBrewer)
ggplot(data = clustered, aes(urban_rural, standard_diploma_prop)) +
  scale_color_manual(values=c("#99d8c9", "#2ca25f")) +
  geom_boxplot() +
  geom_point(color = factor(clustered$cluster)) +
  ggalt::geom_encircle(aes(x=urban_rural, y=standard_diploma_prop),
                       data=clus1,
                       color="magenta",
                       size=2,
                       expand=0.08) +  # encircle bloack
  ggalt::geom_encircle(aes(x=urban_rural, y=standard_diploma_prop),
                       data=clus2,
                       color="green",
                       size=2,
                       expand=0.08)

#TRIED COLORING BY URBAN/RURAL BUT NOT SIGNIFICANT
ggplot(data = clustered, aes(standard_diploma_prop, percentTeachersWProvisionalCredentials, color = factor(urban_rural))) +
  geom_point()

ggplot(data = clustered, aes(adv_studies_diploma_prop, percentTeachersWProvisionalCredentials, color = factor(cluster), label = sch_name)) +
  geom_text() +
  geom_smooth(method="loess", se=F)


colnames(clustered)

#plot of disadv_dropout_prop vs. expulsion_prop
ggplot(data = clustered, aes(disadv_dropout_prop, offenses_prop, color = factor(urban_rural))) +
  geom_point() +
  geom_smooth(method="loess", se=F)


#PLOT CLUSTERS ON THE MAP

usa = map_data("usa")
states = map_data("state")
virginia <- subset(states, region %in% c("virginia"))
mapSchools <- clustered
mapSchools <- mapSchools[-which(mapSchools$long <= -120),]   # obs 277 has the long that is not in virginia

#Notice one value has lat and long that don't make sense: obs 277

ggplot(data = virginia) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  geom_point(data = mapSchools,
             aes(x = long, y = lat, color = as.factor(urban_rural), shape = as.factor(mapSchools$cluster)),
             size = 2)

ggplot(data = virginia) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  geom_point(data = mapSchools,
             aes(x = long, y = lat, color = as.factor(urban_rural)),
             size = 2)

ggplot(data = virginia) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  geom_point(data = mapSchools,
             aes(x = long, y = lat, color = as.factor(cluster)),
             size = 2)

ggplot(data = virginia) +
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  geom_point(data = mapSchools,
             aes(x = long, y = lat, color = as.factor(urban_rural)),
             size = 2)



#CLUSTER PLOTS WITH MOST IMPORTANT VARIABLES FROM PCA
#    MOST IMPORTANT VARIABLES FROM PCA ARE:
mvp <- c("standard_diploma_prop",
         "offenses_prop",
         "non_disadv_dropout_prop",
         "percentCTECompleters",
         "percentTeachersWProvisionalCredentials",
         "ged_certificate_prop",
         "psat_junior_test_takers",
         "percentTeachersGraduateDegree",
         "sat_test_takers",
         "percentPassedAPTest",
         "sat_math_mean",
         "adv_studies_diploma_prop")

mvpClust <- subset(clustered, select = mvp)
pairs(mvpClust)

ggplot(data = clustered, aes(offenses_prop, standard_diploma_prop, color = factor(cluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(offenses_prop, standard_diploma_prop, color = factor(urban_rural))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(non_disadv_dropout_prop, standard_diploma_prop, color = factor(cluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(non_disadv_dropout_prop, standard_diploma_prop, color = factor(urban_rural))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(percentCTECompleters, standard_diploma_prop, color = factor(cluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(percentCTECompleters, standard_diploma_prop, color = factor(urban_rural))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(percentTeachersWProvisionalCredentials, standard_diploma_prop, color = factor(cluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(percentTeachersWProvisionalCredentials, standard_diploma_prop, color = factor(urban_rural))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(ged_certificate_prop, standard_diploma_prop, color = factor(cluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(ged_certificate_prop, standard_diploma_prop, color = factor(urban_rural))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(ged_certificate_prop, standard_diploma_prop, color = factor(cluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(ged_certificate_prop, standard_diploma_prop, color = factor(urban_rural))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(ged_certificate_prop, standard_diploma_prop, color = factor(cluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(ged_certificate_prop, standard_diploma_prop, color = factor(urban_rural))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(percentTeachersGraduateDegree, percentPassedAPTest, color = factor(cluster))) +
  geom_point() +
  geom_smooth(method="loess", se=F)

ggplot(data = clustered, aes(percentTeachersGraduateDegree, percentPassedAPTest, color = factor(urban_rural))) +
  geom_point() +
  geom_smooth(method="loess", se=F)



