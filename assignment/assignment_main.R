
# -----
# Script Author: Jofrey Segeza Amos
# Script Puropose: Biostat III Assignment
# Last Edited: 24 - Mar - 2026
# -----

# Load packages 

pacman::p_load(
  tidyverse, # for data manipulation and visualisation 
  geepack, # for GEE modelling 
  broom, # for obtaining confidence intervals from GEE models 
  nlme, # for LME modelling
  lme4, # for GLME modelling
  broom.mixed, # for obtaining confidence intervals from GLME models
  GLMMadaptive, # more GLME modelling (extensions)
  missForest # to randomly generate some missing data
)

# Import data

data_raw <- read.csv('assignment/assign_satisfaction.csv', header = T)

str(data_raw)

