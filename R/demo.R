library(ms)
library(survival)
library(Matrix)
library(MASS)
library(boot)
library(stringr)

##----- Example

df <- read.csv("~/Documents/LocalGit/causalMediation/data/Mbin_int_data_10000.txt", sep = " ")

cm <- causmed$new(data = df,
                  outcome = "Y_cont_int",
                  treatment = 'A',
                  mediator = 'M_bin',
                  covariates = "C",
                  interaction = TRUE,
                  yreg = "linear", mreg = "logistic",
                  boot = TRUE, nboot = 10, event = NULL, a_star = 0, a = 1, m = 3)
cm
cm$create_formulas()
cm$outcome_formula
cm$run_regressions()
cm$outcome_regression
cm$mediator_formula
cm$mediator_regression
cm$get_coef()
cm$vcov_block
cm$CDE_estimate()
cm$cde
cm$CDE_bin()

cm$boostrap()
