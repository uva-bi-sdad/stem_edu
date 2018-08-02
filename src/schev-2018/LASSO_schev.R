#####################################################
###     LASSO Regression: SCHEV Schools Data      ###
###                    Emily Sheen                ###
#####################################################
library(readr)
library(glmnet)
schev <- read_csv("./data/stem_edu/working/DSPG18/SCHEV/hs_model_data_merged3.csv")

#These are the variables kept in the K-Means clustering and PCA

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

schevClust <- subset(schev, select = keep)
View(schevClust)

response <- glmnet()
