# Division of Epidemiology and Biostatistics 
# R workshop missing data 
# 2026

# install packages 
# only run this once if you have never installed these before 
# install.packages("tidyverse")
# install.packages("naniar")
# install.packages("mice")
# install.packages("survival")

# load libraries
library(tidyverse) # data management and visualisation
library(naniar) # missing data visualisation 
library(mice) # conditional multiple imputation/multiple imputation by chained equations
library(survival) # survival analysis 

# all the datasets we'll be working with are found in R libraries 
# nhanes2 --> mice
# cancer --> survival 
# will also be using the boys data from mice 

#### Understanding/visualising the data ####
data("nhanes2")
?nhanes2

head(nhanes2)

dim(nhanes2)

str(nhanes2)

summary(nhanes2)

ggplot(nhanes2, aes(x = bmi, y = chl)) +
  geom_point() + geom_smooth() +
  theme_classic() + 
  labs(x = "BMI (kg/m^2)", y = "Total serum cholesterol (mg/dL)")

ggplot(nhanes2, aes(x = age, y = chl)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = "Age group", y = "Total serum cholesterol (mg/dL)")

ggplot(nhanes2, aes(x = hyp, y = chl)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = "Hypertensive", y = "Total serum cholesterol (mg/dL)")

#### Missing data visualisation ####
# scatterplot but include missing observations 
ggplot(nhanes2, aes(x = bmi, y = chl)) +
  geom_miss_point() +
  theme_classic() 

# count of missing values per variable 
gg_miss_var(nhanes2)

# cumulative count of missing values
gg_miss_case_cumsum(nhanes2)

# proportion of missing values by a factor 
gg_miss_fct(x = nhanes2, fct = age)
gg_miss_fct(x = nhanes2, fct = hyp)

# pattern of missing data
md.pattern(nhanes2)

# pairs of missing data
md.pairs(nhanes2)

#### Complete case analysis ####
glm1 <- glm(chl ~ bmi + hyp + age, data = nhanes2, 
            family = "gaussian")
summary(glm1)

glm2 <- glm(hyp ~ bmi + chl, data = nhanes2, 
            family = "binomial")
summary(glm2)

#### Default multiple imputation ####
# mice pipeline: mice() --> with() --> pool()
#                mids   --> mira   --> mipo 

# default imputation 
imp1 <- mice(nhanes2, seed = 2026)

# check the result
imp1

# check the imputed values 
imp1$imp$bmi

# look at first completed dataset 
complete(imp1, action = 1) 

# all completed datasets stacked together
nhanes2_complete <- complete(imp1, action = "long", include = T)
head(nhanes2_complete)
table(nhanes2_complete$.imp) 
table(nhanes2_complete$.id) 
nhanes2_complete %>% filter(.id == 1)
summary(nhanes2_complete)

# inspect the imputed values for one variable across all imputed datasets 
stripplot(imp1)
# we want the imputed values (pink) to closely correspond to observed values (blue) under MAR assumptions 
bwplot(imp1)
densityplot(x = imp1)

# scatterplot of two imputed values  
stripplot(imp1, bmi ~ chl)

# compare proportions of categorical variables across each imputed dataset
prop.table(table(nhanes2_complete$hyp, nhanes2_complete$.imp), margin = 2) 

# fit the regression models using the imputed data
glm1_imp1 <- with(imp1, glm(chl ~ bmi + hyp + age, 
                            family = "gaussian"))
glm2_imp1 <- with(imp1, glm(hyp ~ bmi + chl, 
                  family = "binomial"))

print(summary(glm1_imp1), n = 25)
summary(glm2_imp1)

# get the model output from the first imputed dataset
summary(glm1_imp1$analyses[[1]])

# pool the results using Rubin's rules 
pool(glm1_imp1)
pool(glm2_imp1)

# regression estimates
summary(pool(glm1_imp1), conf.int = T)
summary(pool(glm2_imp1), conf.int = T, exponentiate = T)

# can compare nested models using a likelihood ratio test 
glm1_imp0 <- with(imp1, glm(chl ~ bmi, 
                            family = "gaussian"))
glm1_imp1 <- with(imp1, glm(chl ~ bmi + hyp + age, 
                            family = "gaussian"))
D3(fit1 = glm1_imp1, fit0 = glm1_imp0)

#### Going beyong the default ####
# increase number of imputations using "m" argument 
# increase number of iteration using "maxit" argument 
# fit an imputation model with 50 imputations and 10 iterations per imputation 
imp2 <- mice(nhanes2, seed = 2026, 
             m = 50, 
             maxit = 10, 
             printFlag = F)
imp2
densityplot(imp2)

# fit the regression models using the imputed data
glm1_imp2 <- with(imp2, glm(chl ~ bmi + hyp + age, 
                            family = "gaussian"))
glm2_imp2 <- with(imp2, glm(hyp ~ bmi + chl, 
                            family = "binomial"))

pool(glm1_imp2)
pool(glm2_imp2)
# regression estimates
summary(pool(glm1_imp2), conf.int = T)
summary(pool(glm2_imp2), conf.int = T, exponentiate = T)

# changing imputation method 
imp3 <- mice(nhanes2, seed = 2026, 
             m = 50, 
             maxit = 10, 
             printFlag = F, 
             method = "pmm")
imp3

imp4 <- mice(nhanes2, seed = 2026, 
             m = 50, 
             maxit = 10, 
             printFlag = F, 
             method = c("", "norm", "cart", "lasso.norm")) # order matters 
imp4

# specifying which variables are used in the imputation model for other variables
# get the predictor matrix 
pred_matrix <- imp2$predictorMatrix
# row = what you are predicting 
# column = variable used in imputation model 
# make it so BMI isn't used to predict any other variable
pred_matrix[, "bmi"] <- 0
# don't used hypertension to impute cholesterol 
pred_matrix[4, "hyp"] <- 0
imp5 <- mice(nhanes2, seed = 2026, 
             m = 10, 
             maxit = 10, 
             printFlag = F, 
             predictorMatrix = pred_matrix)


# can change the order in which variables are imputed
imp5$visitSequence
imp6 <- mice(nhanes2, seed = 2026, 
             m = 10, 
             maxit = 10, 
             printFlag = F, 
             visitSequence = c(2, 5, 4, 3))
imp6$visitSequence

# transformations of variables and passive imputation 
# for example, you want to derive BMI from weight and height 
head(boys)
md.pattern(boys[, c("hgt", "wgt", "bmi")])
# we want to ensure that when BMI is imputed, the values will be consistent with weight and height 
# initialise the imputation 
ini <- mice(boys, maxit = 0, printFlag = F)
method <- ini$method
method["bmi"] <- "~I(wgt/(hgt/100)^2)"
# make sure that we don't use BMI to impute height or weight
pred_matrix2 <- ini$predictorMatrix
pred_matrix2[c("hgt", "wgt"), "bmi"] <- 0
imp7 <- mice(boys, pred = pred_matrix2, method = method,
                seed = 2026, print = FALSE)
complete(imp7, action = "long", include = T) %>% filter(.id == 18)

# what would happen if you didn't do this
imp7.1 <- mice(boys, pred = pred_matrix2, seed = 2026, print = FALSE)
complete(imp7.1, action = "long", include = T) %>% filter(.id == 18)

# same principle applies to interactions 
# if you plan on including an interaction, you must include the interaction in the imputation model 
# create a column representing the interaction between bmi and cholesterol 
nhanes2_int <- nhanes2 %>% mutate(bmixchl = (bmi-25)*(chl-200))
ini2 <- mice(nhanes2_int, max = 0, printFlag = F)
method2 <- ini2$method
method2["bmixchl"] <- "~I((bmi-25)*(chl-200))" # mean-centring and interaction between 2 variables 
pred_matrix3 <- ini2$predictorMatrix
pred_matrix3[c("bmi", "chl"), "bmixchl"] <- 0 # don't use the interaction to impute bmi or cholesterol 
imp8 <- mice(nhanes2_int, method = method2, 
             predictorMatrix = pred_matrix3, 
             seed = 2026,
             m = 10, 
             maxit = 10,
             print = FALSE)
imp8
complete(imp8, action = "long", include = T) %>% filter(.id == 1)

glm2_imp8 <- with(imp8, glm(hyp ~ I(bmi-25)*I(chl-200), 
                            family = "binomial"))
summary(pool(glm2_imp8), conf.int = T, exponentiate = T)

#### Extra: multiple imputation of survival data ####
head(cancer)
str(cancer)
summary(cancer)
# quick data management
cancer <- cancer %>% mutate(status = status - 1, 
                            sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")), 
                            ph.ecog = as.factor(ph.ecog))

coxph1 <- coxph(Surv(time = time, event = status) ~ sex + age + ph.ecog + meal.cal, 
                data = cancer)
summary(coxph1)
# set up imputation
imp9 <- mice(cancer, seed = 2026, 
             m = 1, 
             maxit = 0, 
             printFlag = F)
pred_matrix4 <- imp9$predictorMatrix
# don't want to use inst to predict anything else  
pred_matrix4[, "inst"] <- 0 
# don't want to impute inst
method3 <- c("", "", "", "", "", "polyreg", "pmm", "pmm", "pmm", "pmm")
# perform imputation using predictor matrix
imp9 <- mice(cancer, seed = 2026, 
             m = 10, 
             maxit = 10, 
             printFlag = F, 
             predictorMatrix = pred_matrix4, 
             method = method3)
complete(imp9, action = "long", include = T) %>% filter(.id == 156) # inst wasn't imputed for this individual 

# inspect imputations 
stripplot(imp9)
bwplot(imp9)
densityplot(imp9)

# fit the cox model
coxph1_imp <- with(imp9, 
                   coxph(Surv(time = time, 
                              event = status) ~ sex +
                           age + ph.ecog + meal.cal))

summary(pool(coxph1_imp), conf.int = T, exponentiate = T)

# for the survival example, we included the event time and indicator as separate variables in the imputation model
# another approach is to include the cumulative baseline hazard instead of the event time in the imputation model
# baseline hazard is the hazard function when all predictors are zero or at the reference level 
# cumulative baseline hazard sums the baseline hazard up to a given time
# ref: https://doi.org/10.1002/sim.3618

# calculate cumulative baseline hazard 
cancer <- cancer %>% mutate(cumhaz = nelsonaalen(cancer, timevar = time, statusvar = status))
# initialise
imp10 <- mice(cancer, seed = 2026, 
             m = 1, 
             maxit = 0, 
             printFlag = F)
pred_matrix5 <- imp10$predictorMatrix
# don't want to use inst or time to predict anything else  
pred_matrix5[, c("inst", "time")] <- 0 
# don't want to impute inst
method4 <- c("", "", "", "", "", "polyreg", "pmm", "pmm", "pmm", "pmm", "")

imp10 <- mice(cancer, seed = 2026, 
             m = 10, 
             maxit = 10, 
             printFlag = F, 
             predictorMatrix = pred_matrix5, 
             method = method4)

# inspect imputations 
stripplot(imp10)
bwplot(imp10)
densityplot(imp10)

# fit the cox model
coxph2_imp <- with(imp10, 
                   coxph(Surv(time = time, 
                              event = status) ~ sex +
                           age + ph.ecog + meal.cal))

summary(pool(coxph2_imp), conf.int = T, exponentiate = T)

#### Extra: descriptive statistics are MI ####
imp2
imp2_complete <- complete(imp2, action = "long", include = F)
mean_bmi <- imp2_complete %>% group_by(.imp) %>% 
  summarise(mean_bmi = mean(bmi)) %>% ungroup() %>% select(mean_bmi) %>% pull()
sd_bmi <- imp2_complete %>% group_by(.imp) %>% 
  summarise(sd_bmi = sd(bmi)) %>% ungroup() %>% select(sd_bmi) %>% pull()

mean(mean_bmi) # average of means
mean(sd_bmi) # average of standard deviations 

# frequency and proportion of hypertension 
mean(table(imp2_complete$hyp, imp2_complete$.imp)[1, ])
mean(table(imp2_complete$hyp, imp2_complete$.imp)[2, ])
mean(prop.table(table(imp2_complete$hyp, imp2_complete$.imp), margin = 2)[1, ])
mean(prop.table(table(imp2_complete$hyp, imp2_complete$.imp), margin = 2)[2, ])
