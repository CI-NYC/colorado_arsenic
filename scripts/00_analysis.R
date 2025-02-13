# -------------------------------------
# Script: 00_analysis
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
library(fastDummies)
library(ife)
#devtools::install_github("nt-williams/riesznet")
library(riesznet)
#devtools::install_github("nt-williams/crumble@riesznet")
library(crumble) # use riesznet version 
library(ranger)
library(xgboost)
library(mlr3extralearners)
library(torch) # might need this?

# reading data
dat <- read.csv("data/simulation_data.csv")

# summary(dat)

# 2000 rows
# Variables
# W1: YEAR
# W2: Education_factor (convert to dummy), BASEAGE (continuous), Sex (convert to 0, 1), Ethnic2 (convert to dummy)
# Z: Income_factor (convert to dummy), CURAGMWK (continuous), TOTTRAIN (convert to dummy), PACKYRS (continuous), BMI (continuous)
# Exposure: drought (binary)
# Mediator: As_value (continuous)
# Outcomes: diab_bin (convert to 0, 1), cvd (binary)

dat <- dat |>
  mutate(#drought = factor(drought, levels = c("0", "1")),
    SEX = ifelse(SEX == 1, 0, 1),
    diab_bin = ifelse(diab_bin == 1, 0, 1),
    Education_factor = factor(Education_factor, levels = c("Less than High School", "High School Graduate", "College Graduate")), # relevel
    Income_factor = factor(Income_factor, levels = c("<$15,000", "$15,000-$34,999", ">$34,000")), # relevel
    TOTTRAIN = factor(TOTTRAIN, levels = c(1, 2, 3)),
    YEAR = YEAR - min(YEAR)) #shifting minimum year to 0 (could also consider factor)

# converting factor variables to dummy columns
dummy_cols <- c("Education_factor",
                "Ethnic2",
                "Income_factor",
                "TOTTRAIN")

dat_with_dummies <- dummy_cols(dat, dummy_cols, remove_first_dummy = TRUE, remove_selected_columns = TRUE) |>
  janitor::clean_names()

# summary(dat_with_dummies)

# baseline covariates
W <- c(# W1
  "year",
  # W2
  # education dummy variables
  "education_factor_high_school_graduate",
  "education_factor_college_graduate",
  "baseage",
  "sex",
  # ethnicity dummy variables
  "ethnic2_hispanic",
  "ethnic2_white"
)

# post-exposure confounders (include W2 here)
Z <- c(# income dummy variables
  "income_factor_15_000_34_999",
  "income_factor_34_000",
  "curagmwk",
  # TOTTRAIN dummy variables
  "tottrain_2",
  "tottrain_3",
  "packyrs",
  "bmi"
)

# exposure
A <- c("drought")

# mediator
M <- c("as_value")

# ANALYSIS

# function to run code
run_crumble <- function(data,
                        trt,
                        outcome,
                        covar,
                        mediators,
                        moc)
  
{
  res <- crumble(
    data = data,
    trt = trt, 
    outcome = outcome,
    covar = covar,
    mediators = mediators,
    moc = moc,
    d0 = \(data, trt) rep(0, nrow(data)), # shifting all exposure to unexposed
    d1 = \(data, trt) rep(1, nrow(data)), # shifting all exposure to exposed
    effect = "RT",
    learners = c("mean", "glm", "ranger", "earth", "xgboost"), # algorithms from different families, can adjust
    #nn_module = sequential_module(),
    control = crumble_control(crossfit_folds = 5L, # may want to increase
                              epochs = 500L, # start at 500L, can decrease if data is large
                              mlr3superlearner_folds = 5L, # can adjust depending on sample size
                              zprime_folds = 5L, # keeping low, can adjust (increase may speed up run)
                              learning_rate = 0.01, # tune this -- start with 0.01
                              batch_size = 64) # can do 64 with this data -- increase size to speed up run (if data is larger)
  )
  
  res
}

# outcomes
y1 <- "diab_bin"
y2 <- "cvd"

# running on Diab_bin outcome

finished <- FALSE # marker for the code not being finished

set.seed(1)
while(!finished){ # this will continue running the code until a successful iteration -- should print out error message
  set.seed(1)
  tryCatch({
    results_diab_bin <- run_crumble(data = dat_with_dummies,
                                    trt = A,
                                    outcome = y1,
                                    covar = W,
                                    mediators = M,
                                    moc = Z)
    
    finished <- TRUE # if the run is successul, the marker will be finished = TRUE which will stop the loop
  }, error = function(e){
    cat("Error....",
        e$message)})
}

saveRDS(results_diab_bin, "results_diab_bin.rds") # saving results

# running on CVD outcome

finished <- FALSE

set.seed(1)
while(!finished){  # this will continue running the code until a successful iteration -- should print out error message
  set.seed(1)
  tryCatch({
    results_CVD <- run_crumble(data = dat_with_dummies,
                                    trt = A,
                                    outcome = y2,
                                    covar = W,
                                    mediators = M,
                                    moc = Z)
    
    finished <- TRUE
  }, error = function(e){
    cat("Error....",
        e$message)})
}



saveRDS(results_CVD, "results_CVD.rds") # saving results




