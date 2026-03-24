# Division of Epidemiology and Biostatistics 
# R workshop advanced modelling 
# 2026

#### Install packages ####
# only run this once if you have never installed the packages before

# install.packages("tidyverse")
# install.packages("geepack")
# install.packages("broom")
# install.packages("nlme")
# install.packages("lme4")
# install.packages("broom.mixed")
# install.packages("GLMMadaptive")
# install.packages("missForest")

#### Load libraries ####
library(tidyverse) # for data manipulation and visualisation 
library(geepack) # for GEE modelling 
library(broom) # for obtaining confidence intervals from GEE models 
library(nlme) # for LME modelling
library(lme4) # for GLME modelling
library(broom.mixed) # for obtaining confidence intervals from GLME models
library(GLMMadaptive) # more GLME modelling (extensions)
library(missForest) # to randomly generate some missing data

pacman::p_load(
  tidyverse,
  geepack,
  broom,
  nlme,
  lme4,
  broom.mixed,
  GLMMadaptive,
  missForest
  )


# remember not all of these packages are essential for every instance of fitting models for clustered/longitudinal data analysis 

# Set working directory
setwd() # change this to the working directory that contains the data on your device

# check what directory you are in
getwd() 

#### Understanding the data ####
# Read in the data 
stroke_wide <- read.csv("workshop_2/stroke.csv", header = T)

# first 6 rows
head(stroke_wide)

# structure of the variables
str(stroke_wide)

# summary statistics 
summary(stroke_wide)

# change subject and group to factors
stroke_wide$subject <- as.factor(stroke_wide$subject)
stroke_wide$group <- factor(stroke_wide$group, 
                            levels = c("1", "2", "3"), 
                            labels = c("A", "B", "C"))

# Reshape dataset from wide to long 
stroke_long <- stroke_wide %>% pivot_longer(cols = c(week1:week8), 
                                       names_to = "time", 
                                       values_to = "score", 
                                       names_prefix = "week") %>% 
  as.data.frame()

# Can reshape back to wide
stroke_wide <- stroke_long %>% pivot_wider(names_from = time, 
                                           values_from = score, 
                                           names_prefix = "week") %>% 
  as.data.frame()

# first 8 rows of long dataset
head(stroke_long, 8)

# find subject 12
stroke_long %>% filter(subject == "12")

# how many observations 
dim(stroke_long)

# how many unique subjects
length(unique(stroke_long$subject))

# structure of dataset
str(stroke_long)

# change time to a numeric variable
stroke_long$time <- as.numeric(stroke_long$time)

# time starts at week 1 but let's change this to make time start from 0 (baseline)
# you don't need to do this, but it will make the intercept more interpretable 
stroke_long$time <- stroke_long$time - 1
range(stroke_long$time)

#### Exploratory data analysis ####
# mean score at each time point 
stroke_long %>% group_by(time) %>% 
  summarise(mean(score))

# number of participants in each group
table(stroke_wide$group)

# boxplot of scores over time in each group (base R)
boxplot(score ~ time*group, data = stroke_long)

# boxplot of scores over time in each group (ggplot)
ggplot(data = stroke_long, aes(x = as.factor(time), 
                               y = score)) + 
  geom_boxplot() + 
  facet_wrap(~group) + 
  labs(x = "Week", y = "Stroke Score") +
  theme_classic()

# interaction plot: shows the trajectory of scores over time in the different groups (base R)
interaction.plot(stroke_long$time, stroke_long$group, 
                 stroke_long$score, 
                 fun = mean, ylab = "Mean score",
                 xlab = "Week")

# interaction plot: shows the trajectory of scores over time in the different groups (ggplot)
ggplot(data = stroke_long, aes(x = time, 
                               y = score, 
                               colour = group)) + 
  geom_smooth(method = "loess", se = F) + # change method = "lm" for straight lines
  labs(x = "Week", y = "Stroke score") +
  theme_classic()

# select just the weeks data from stroke_wide
weeks <- stroke_wide %>% select(week1:week8)
head(weeks)
# view the correlation between scores over time (helps get a sense of which correlation structure to pick for GEE)
cor(weeks)

#### GEE models for a continuous response ####
# use the geepack library to fit GEE models 
# can also use gee library instead, gives very similar results 

# independent correlation structure (INCORRECT!)
model_independence <- geeglm(score ~ time + group, 
                             id = subject, 
                             family = "gaussian", 
                             corstr = "independence", 
                             data = stroke_long)
summary(model_independence)
# robust variance-covariance matrix
model_independence$geese$vbeta

# exchangeable within subject correlation
model_exchangeable <- geeglm(score ~ time + group, 
                             id = subject, 
                             family = "gaussian", 
                             corstr = "exchangeable", 
                             data = stroke_long)
summary(model_exchangeable)
# obtain confidence intervals 
broom::tidy(model_exchangeable, conf.int = TRUE)

# autoregressive within subject correlation
model_autoregressive <- geeglm(score ~ time + group, 
                               id = subject,  
                               family = "gaussian", 
                               corstr = "ar1", 
                               data = stroke_long)
summary(model_autoregressive)
# obtain confidence intervals 
broom::tidy(model_autoregressive, conf.int = TRUE)

# unstructured within subject correlation
model_unstructured <- geeglm(score ~ time + group, 
                             id = subject, 
                             family = "gaussian", 
                             corstr = "unstructured", 
                             data = stroke_long)
summary(model_unstructured)

# Which model do we use? 
# which variables to include, which correlation structure to use 

# can compare standard errors of the models
rbind(summary(model_exchangeable)$coefficients$Std.err, 
      summary(model_autoregressive)$coefficients$Std.err, 
      summary(model_unstructured)$coefficients$Std.err)

# make predictions on all the data (same as using the fitted() function)
predict(model_autoregressive)
# add predictions to original dataframe 
stroke_long$GEE_preds_ar1 <- predict(model_autoregressive)
head(stroke_long)
# observed mean score in each group at each time point and predicted mean score in each group at each time point 
obs_pred_mean_ar1 <- stroke_long %>% group_by(time, group) %>% 
  summarise(observed_mean = mean(score), 
            predicted_mean = mean(GEE_preds_ar1))

# plot the mean score observed for each group at each time point with the fitted regression line obtained from the model
ggplot(data = obs_pred_mean_ar1) + 
  geom_point(aes(x = time, y = observed_mean, colour = group)) +
  geom_line(aes(x = time, y = predicted_mean, colour = group)) +
  labs(x = "Time", y = "Score") + theme_classic()

# do the same for the exchangeable model 
stroke_long$GEE_preds_exch <- predict(model_exchangeable)
# observed mean score in each group at each time point and predicted mean score in each group at each time point 
obs_pred_mean_exch <- stroke_long %>% group_by(time, group) %>% 
  summarise(observed_mean = mean(score), 
            predicted_mean = mean(GEE_preds_exch))

# plot the mean score observed for each group at each time point with the fitted regression line obtained from the model
ggplot(data = obs_pred_mean_exch) + 
  geom_point(aes(x = time, y = observed_mean, colour = group)) +
  geom_line(aes(x = time, y = predicted_mean, colour = group)) +
  labs(x = "Time", y = "Score") + theme_classic()

#### Linear mixed effects modelling for a continuous response #### 
# create a grouped data object if you want to make a spaghetti plot in base R
stroke_long_grouped <- groupedData(score ~ time | subject, 
                                   outer = ~ group,
                                   data = stroke_long)
# make a spaghetti plot (nlme plot) 
plot(stroke_long_grouped) # one panel for each subject 
# one panel for each subject (nlme plot)
plot(stroke_long_grouped, outer = T) # one panel for each group 

# make a spaghetti plot (ggplot)
ggplot(data = stroke_long, aes(x = time, y = score, group = subject)) +
  geom_line(aes(colour = subject)) + labs(x = "Time (weeks)", y = "Score") +
  theme_classic() +
  theme(legend.position = "none") 

# make a spaghetti plot, one panel for each group (ggplot)
ggplot(data = stroke_long, aes(x = time, y = score, group = subject)) +
  geom_line(aes(colour = subject)) + labs(x = "Time (weeks)", y = "Score") +
  facet_wrap(~group) + theme_classic() +
  theme(legend.position = "none") 

# fit an empty LME model with random effect on the intercept
lme_mod_0 <- lme(score ~ 1, 
                 random = ~ 1 | subject, 
                 data = stroke_long, 
                 method = "REML")
summary(lme_mod_0)
# calculate the ICC: (Intercept)^2/(Intercept^2 + Residual^2)
19.4^2/(19.4^2 + 14.7^2)

# fit an LME model with random effect on the intercept
lme_mod_1 <- lme(score ~ time + group, 
                 random = ~ 1 | subject, 
                 data = stroke_long, 
                 method = "REML")
summary(lme_mod_1)
# confidence intervals
intervals(lme_mod_1)

# fit a model with random effects on the intercept and the slope for time
lme_mod_2 <- lme(score ~ time + group,
                 random = ~ time | subject, 
                 data = stroke_long, 
                 method = "REML")
summary(lme_mod_2)
# confidence intervals
intervals(lme_mod_2)

# fit a model with random effects on the slope for time
lme_mod_3 <- lme(score ~ time + group,
                 random = ~ time - 1 | subject, 
                 data = stroke_long, 
                 method = "REML")
summary(lme_mod_3)
# confidence intervals
intervals(lme_mod_3)

# which model has more support from the data to be the best model
anova(lme_mod_1, lme_mod_2, lme_mod_3)

# include interactions 
lme_mod_1.1 <- lme(score ~ time*group, 
                 random = ~ 1 | subject, 
                 data = stroke_long, 
                 method = "REML")
summary(lme_mod_1.1)
# confidence intervals
intervals(lme_mod_1.1)

lme_mod_2.1 <- lme(score ~ time*group,
                 random = ~ time | subject, 
                 data = stroke_long, 
                 method = "REML")
summary(lme_mod_2.1)
# confidence intervals
intervals(lme_mod_2.1)

# fit a model with random effects on the slope for time
lme_mod_3.1 <- lme(score ~ time*group,
                 random = ~ time - 1 | subject, 
                 data = stroke_long, 
                 method = "REML")
summary(lme_mod_3.1)
# confidence intervals
intervals(lme_mod_3.1)

# compare model 2 with and without interaction
anova(lme_mod_2, lme_mod_2.1) 

# checking assumptions 
# fitted (predicted) values at the population level
head(fitted(lme_mod_2, level = 0), 16)
# fitted (predicted observations) values at the subject-specific level
head(fitted(lme_mod_2, level = 1), 16)
# fitted values at both levels
head(fitted(lme_mod_2, level = 0:1), 16)
# can also get the residuals at both levels
head(residuals(lme_mod_2, level = 0:1), 16)
# can extract the actual random effects
ranef(lme_mod_2)
# intercept = b_0i
# time = b_1i

# plot of fitted vs residuals 
plot(lme_mod_2)
# check for heteroskedasticity and a linear relationship

# check for normality in the residuals 
qqnorm(lme_mod_2) 

# we also assume that the random effects are normally distributed
# create random effects data frame
ranef_df <- ranef(lme_mod_2)
# qqplot for intercept random effects with 45 degree line
qqnorm(ranef_df$`(Intercept)`)
qqline(ranef_df$`(Intercept)`, col = "black", lwd = 0.1)
# qqplot for time random effects with 45 degree line
qqnorm(ranef_df$time)
qqline(ranef_df$time, col = "black", lwd = 0.1)

# let's make a cool plot to see how our mixed effects model performs at the population and subject level
# first plot the actual observations of the outcome over time from our dataset (one panel for each subject)
p1 <- ggplot(data = stroke_long) + 
  geom_point(aes(x = time, y = score, colour = "Observed"), size = 1) +
  geom_line(aes(x = time, y = score, colour = "Observed")) + 
  facet_wrap(~ subject) + theme_classic()
p1

# include predictions at population level (same line over time for each subject but differing by group)
p2 <- p1 + 
  geom_point(aes(x = time, y = predict(lme_mod_2, level = 0), 
                 colour = "Predicted population level"), size = 1) +
  geom_line(aes(x = time, y = predict(lme_mod_2, level = 0), 
                colour = "Predicted population level"))

p2

# include predictions at the subject-specific level (each subject will have their own specific trajectory based on the population estimated effects and the subject-specific random effects)
p3 <- p2 + geom_point(aes(x = time, y = predict(lme_mod_2, level = 1), 
                          colour = "Predicted patient level"), size = 1) +
  geom_line(aes(x = time, y = predict(lme_mod_2, level = 1), 
                colour = "Predicted patient level"))
p3

# clean up the plot 
p3 + scale_colour_manual(labels = c("Observed", "Predicted patient", 
                                    "Predicted population"), 
                         values = c("black", "blue1", "darkorange1"), 
                         name = "") +
  labs(x = "Time (weeks)", y = "Score") + 
  theme(legend.position = "top")

# can include this plot in a single chunk of code
ggplot(data = stroke_long) + 
  geom_point(aes(x = time, y = score, colour = "Observed"), size = 1) +
  geom_line(aes(x = time, y = score, colour = "Observed")) +
  facet_wrap(~ subject) + theme_classic() + 
  geom_point(aes(x = time, y = predict(lme_mod_2, level = 0), 
                 colour = "Predicted population level"), size = 1) +
  geom_line(aes(x = time, y = predict(lme_mod_2, level = 0), 
                colour = "Predicted population level")) + 
  geom_point(aes(x = time, y = predict(lme_mod_2, level = 1), 
                 colour = "Predicted patient level"), size = 1) +
  geom_line(aes(x = time, y = predict(lme_mod_2, level = 1), 
                colour = "Predicted patient level")) +
  scale_colour_manual(labels = c("Observed", "Predicted patient", "Predicted population"), 
                      values = c("black", "blue1", "darkorange1"), name = "") +
  labs(x = "Time", y = "Score") + theme(legend.position = "top") 

#### GEE models for binary outcomes ####
# create a binary outcome if stroke score is <= 40
stroke_long <- stroke_long %>% mutate(score_binary = 
                                        ifelse(score <= 40, 0, 1))
# GEE with autoregressive correlation structure
log_model_autoregressive <- geeglm(score_binary ~ time + group, id = subject, 
                                   family = binomial(link = "logit"), 
                                   corstr = "ar1",
                                   data = stroke_long)
summary(log_model_autoregressive)

exp(coef(log_model_autoregressive))

broom::tidy(log_model_autoregressive, conf.int = TRUE, exponentiate= TRUE)

#### Generalised linear mixed effects for non-normal responses ####
## WARNING!!!! ##
# Different interpretation of fixed effects
# the expected response is now conditional on the fixed effects AND random effects 
# conditional vs marginal estimates
# check out this for more info: https://www.stata.com/support/faqs/statistics/random-effects-versus-population-averaged/

# it is possible to fit a non-normal response in the mixed effects framework:
glme_mod_logistic <- glmer(score_binary ~ time + (1 | subject), # excluding group for now otherwise subsequent models won't run
                           family = binomial(link = "logit"), 
                           data = stroke_long, 
                           nAGQ = 25) # nAGQ: a parameter you can play with if your model is not converging 
summary(glme_mod_logistic)
tidy(glme_mod_logistic, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  select(effect, term , OR, conf.low, conf.high)
# BE CAREFUL OF WHAT THIS INTERPRETATION MEANS! 

# If you want to use a GLMM for non-normal responses I would recommend using the "GLMMadaptive" package, read more: https://drizopoulos.github.io/GLMMadaptive/
# has a function that allows you to obtain marginal effects from a GLMM

# using glmmadapative to get the subject level
glme_mod_logistic_2 <- mixed_model(fixed = score_binary ~ time, 
                                   random = ~ 1 | subject, 
                                   family = binomial(link = "logit"), 
                                   data = stroke_long, 
                                   nAGQ = 25)
summary(glme_mod_logistic_2)
tidy(glme_mod_logistic_2, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>%
  select(term , OR, conf.low, conf.high)

# obtain the population estimates using the model at the subject level
glme_mod_logistic_2_marginal <- marginal_coefs(glme_mod_logistic_2, 
                                               std_errors = T)
glme_mod_logistic_2_marginal
exp(glme_mod_logistic_2_marginal$betas)
# useful to read for understanding: https://stats.stackexchange.com/questions/397578/computation-and-interpretation-of-marginal-effects-in-a-glmm

#### Extensions (if we have time) ####

# score measurements are nested within subjects who are nested within groups
lme_mod_4 <- lme(score ~ time,
                 random = ~ 1 | group/subject, 
                 data = stroke_long, 
                 method = "REML")
summary(lme_mod_4)
intervals(lme_mod_4)

# introduce some missing data in the outcome 
set.seed(2026) # to make it reproducible
stroke_long$score_missing <- prodNA(stroke_long[, c(-1, -3, -5), ], noNA = 0.2)$score
summary(stroke_long$score_missing)

# remove the rows with missing outcome values (unbalanced data)
stroke_long_miss <- stroke_long %>% filter(!is.na(score_missing))
length(unique(stroke_long_miss$subject))
stroke_long_miss %>% group_by(subject) %>% count()

# fit a GEE
model_autoregressive_missing <- geeglm(score_missing ~ time + group, 
                                       id = subject,
                                       waves = time, # important for missing data
                                       family = "gaussian", 
                                       corstr = "ar1", 
                                       data = stroke_long_miss)
summary(model_autoregressive_missing)

model_autoregressive_missing2 <- geeglm(score_missing ~ time + group, 
                                       id = subject,
                                       waves = time, # important for missing data
                                       family = "gaussian", 
                                       corstr = "ar1", 
                                       data = stroke_long)
summary(model_autoregressive_missing)


lme_mod_2_missing <- lme(score_missing ~ time + group,
                 random = ~ time | subject, 
                 data = stroke_long_miss, 
                 method = "REML")
summary(lme_mod_2_missing)

lme_mod_2_missing2 <- lme(score_missing ~ time + group,
                         random = ~ time | subject, 
                         data = stroke_long, 
                         method = "REML")

# change settings to na.action = "na.omit"
lme_mod_2_missing2 <- lme(score_missing ~ time + group,
                         random = ~ time | subject, 
                         data = stroke_long, 
                         method = "REML", 
                         na.action = "na.omit")
# now it runs
summary(lme_mod_2_missing2)
