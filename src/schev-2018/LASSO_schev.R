#####################################################
###     LASSO Regression: SCHEV Schools Data      ###
###                    Emily Sheen                ###
#####################################################
library(readr)
library(glmnet)
schev <- read_csv("./data/stem_edu/working/DSPG18/SCHEV/hs_model_data_merged5.csv")
colnames(schev)

#These are the variables kept in the K-Means clustering and PCA
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

##########################################################################
###                   DATA CLEANING                                    ###
##########################################################################


na_count <- sapply(schev, function(y) sum(length(which(is.na(y)))))
na_count <- sapply(clean, function(y) sum(length(which(y == "NA"))))
#View(na_count)   #No NA values

#  REMOVE 1 TOTAL OBSERVATIONS THAT HAVE NA VALS OR ARE OUTLIERS
#remove outliers found during first PCA, obs. with NA scores
outliers = c(which(schev$id_combine.y == 291371))   #this was outlier in PCA and initial lm
schev <- schev[-outliers,]

lateOutliers = c(which(schev$id_combine.y == 960652))
schev <- schev[-lateOutliers,]

#predictors = keep variables
predictors <- subset(schev, select = predictorVars)
responses <- subset(schev, select = responseVars)
colnames(responses)

####   TRANSFORM RESPONSES TO LOG-ODDS FOR LOGISTIC REGRESSION
responses$logOdds4year <- log(responses$attending_four_year_college_prop/(1 - responses$attending_four_year_college_prop))
responses$logOdds2year <- log(responses$attending_two_year_college_prop/(1 - responses$attending_two_year_college_prop))
responses$logOddsOther <- log(responses$other_continuing_ed_plans_prop/(1 - responses$other_continuing_ed_plans_prop))
responses$logOddsEmploy <- log(responses$employment_prop/(1 - responses$employment_prop))
responses$logOddsMilitary <- log(responses$military_prop/(1-responses$military_prop))
responses$logOddsNoPlan <- log(responses$no_plans_prop/(1-responses$no_plans_prop))

#college <- data.frame(not_4year_college = 1 - response$attending_four_year_college_prop, four_year_college = response$attending_four_year_college_prop)
#View(college)

regressVars <- cbind(logOdds4year = responses$logOdds4year, predictors)
lm1 <- lm(regressVars$logOdds4year ~., data = regressVars)
summary(lm1)
lm1$coefficients
plot(lm1)
lm1.AIC=step(lm1)
summary(lm1.AIC)

#  Notice obs. 203 is outlier (remove back at top as lateOutliers)

regressVars <- cbind(logOdds2year = responses$logOdds2year, predictors)
lm2 <- lm(regressVars$logOdds2year ~., data = regressVars)
plot(lm2)
lm2.AIC=step(lm2)
summary(lm2.AIC)

regressVars <- cbind(logOddsOther = responses$logOddsOther, predictors)
lm3 <- lm(regressVars$logOddsOther ~., data = regressVars)
plot(lm3)
lm3.AIC=step(lm3)
summary(lm3.AIC)

regressVars <- cbind(logOddsEmploy = responses$logOddsEmploy, predictors)
lm4 <- lm(regressVars$logOddsEmploy ~., data = regressVars)
plot(lm4)
lm4.AIC=step(lm4)
summary(lm4.AIC)

regressVars <- cbind(logOddsMilitary = responses$logOddsMilitary, predictors)
lm5 <- lm(regressVars$logOddsMilitary ~., data = regressVars)
plot(lm5)
lm5.AIC=step(lm5)
summary(lm5.AIC)


#SOME RESPONSE VALUES FOR NO PLANS HAVE 0, with log-odds -Inf.
#  I am choosing to impute these values with NO PLANS PROPORTIONS OF .0004,
#  which is close to the nearest minimum of .00061
#  LogOdds of imputed values then is -7.6
responses$noPlanImputed <- responses$no_plans_prop
responses$noPlanImputed[which(responses$no_plans_prop == 0)] <- 0.0004
responses$logOddsNoPlanImputed <- log(responses$noPlanImputed / (1-responses$noPlanImputed))

regressVars <- cbind(logOddsNoPlanImputed = responses$logOddsNoPlanImputed, predictors)
lm6 <- lm(regressVars$logOddsNoPlanImputed ~., data = regressVars)
plot(lm6)
lm6.AIC=step(lm6)
summary(lm6.AIC)

#######################################################################
###             LASSO: GLMNET                                       ###
#######################################################################
set.seed(69)
glm1 <- glmnet(as.matrix(predictors), y = responses$logOdds4year, family = "gaussian", nlambda = 100, alpha = 1, standardize = TRUE)
glm1$beta  #notice percentGovernorsSchoolEnrollees has massive coefficients
#plot of coefficient betas for diff. penalty vals lambda
plot(glm1,xvar="lambda",main="LASSO Regression Betas for Different Values of the Tuning Parameter")
cv.glm1 <- cv.glmnet(as.matrix(predictors), y = responses$logOdds4year, family = "gaussian", nlambda = 100, alpha = 1)
plot(cv.glm1, main = "MSE for Different Values of Tuning Parameter Lambda")
## getting cvmspe from best value of lambda
cvmspe.glm1 = min(cv.glm1$cvm)
## get lambda and best fit
lambda.glm1 = cv.glm1$lambda.min
## some plots
plot(glm1,xvar="lambda")
title("LASSO Coefficients for Different Values of the Penalty Lambda", line = 2.5)
abline(v=log(lambda.glm1), col = "black")
legend(x = -3.5, y = -10, "Optimal Lambda", lty = 1, col = "black", )
## beta estimates for best lambda
betas.best=coef(cv.glm1,s="lambda.min")
plot(betas.best, lm1$coefficients, xlim=c(-5,5), ylim=c(-35,35), main = "LASSO Coefficients vs. Linear Model Coefficients")
abline(0,1)
#rerun glm with only best lambda
glm1Best <- glmnet(as.matrix(predictors), y = responses$logOdds4year, family = "gaussian", lambda = lambda.glm1, alpha = 1, standardize = TRUE)
glm1Best$beta
View(lm1.AIC$coefficients)
View(betas.best)
plot(betas.best, lm1$coefficients, xlim=c(-5,5), ylim=c(-35,35), main = "LASSO Coefficients vs. Linear Model Coefficients")
abline(0,1)


glm2 <- glmnet(as.matrix(predictors), y = responses$logOdds2year, family = "gaussian", nlambda = 100, alpha = 1, standardize = TRUE)
glm2$beta  #notice percentGovernorsSchoolEnrollees has massive coefficients
#plot of coefficient betas for diff. penalty vals lambda
plot(glm2,xvar="lambda",main="LASSO Regression Betas for Different Values of the Tuning Parameter")
cv.glm2 <- cv.glmnet(as.matrix(predictors), y = responses$logOdds2year, family = "gaussian", nlambda = 100, alpha = 1)
plot(cv.glm2, main = "MSE for Different Values of Tuning Parameter Lambda")
## getting cvmspe from best value of lambda
cvmspe.glm2 = min(cv.glm2$cvm)
## get lambda and best fit
lambda.glm2 = cv.glm2$lambda.min
glm2$beta
## some plots
plot(glm2,xvar="lambda",main="LASSO Regression Betas for Different Values of the Tuning Parameter")
abline(v=log(lambda.glm2), col = "black")
## beta estimates for best lambda
betas.best=coef(cv.glm2,s="lambda.min")
plot(betas.best, lm2$coefficients, xlim=c(-5,5), ylim=c(-35,35), main = "LASSO Coefficients vs. Linear Model Coefficients")
abline(0,1)
glm2Best <- glmnet(as.matrix(predictors), y = responses$logOdds2year, family = "gaussian", lambda = lambda.glm2, alpha = 1, standardize = TRUE)
glm2Best$beta

