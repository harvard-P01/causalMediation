library(dplyr)

o <- causal_mediation(data = Mbin_int_data,
                      outcome = "Y_cont_int",
                      treatment = 'A',
                      mediator = 'M_bin',
                      covariates = "C",
                      vecc = 100,
                      interaction = TRUE,
                      yreg = "linear", mreg = "logistic",
                      boot = FALSE, nboot = 50, event = NULL, a_star = 0, 
                      a = 1, m = 3)
  
delta_marginal(o)

print_delta(o)

