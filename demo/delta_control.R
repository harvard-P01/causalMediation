library(causalMediation)

df <- data.frame('smoking'    = c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
                 'lbw'        = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'death'      = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'drinking'   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
                 'agebelow20' = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

result_delta_bin_bin_noint <- causalMediationDelta(data = df, 
                                                   outcome = 'death', 
                                                   treatment = 'smoking',
                                                   mediator = 'lbw',
                                                   covariates = 'drinking',
                                                   vecc = 1,
                                                   interaction = FALSE,
                                                   yreg = "logistic", mreg = "logistic")
result_delta_bin_bin_noint

result_delta_bin_bin_int <- causalMediationDelta(data = df, 
                                                 outcome = 'death', 
                                                 treatment = 'smoking',
                                                 mediator = 'lbw',
                                                 covariates = 'drinking',
                                                 vecc = 1,
                                                 interaction = TRUE,
                                                 yreg = "logistic", mreg = "logistic")
result_delta_bin_bin_int

result_delta_cont_bin_noint <- causalMediationDelta(data = df, 
                                                    outcome = 'death', 
                                                    treatment = 'smoking',
                                                    mediator = 'lbw',
                                                    covariates = 'drinking',
                                                    vecc = 1,
                                                    interaction = FALSE,
                                                    yreg = "linear", mreg = "logistic")
result_delta_cont_bin_noint

result_delta_cont_bin_int <- causalMediationDelta(data = df, 
                                                  outcome = 'death', 
                                                  treatment = 'smoking',
                                                  mediator = 'lbw',
                                                  covariates = 'drinking',
                                                  vecc = 1,
                                                  interaction = TRUE,
                                                  yreg = "linear", mreg = "logistic")
result_delta_cont_bin_int

result_delta_bin_cont_noint <- causalMediationDelta(data = df, 
                                                    outcome = 'death', 
                                                    treatment = 'smoking',
                                                    mediator = 'lbw',
                                                    covariates = 'drinking',
                                                    vecc = 1,
                                                    interaction = FALSE,
                                                    yreg = "logistic", mreg = "linear")
result_delta_bin_cont_noint

result_delta_bin_cont_int <- causalMediationDelta(data = df, 
                                                  outcome = 'death', 
                                                  treatment = 'smoking',
                                                  mediator = 'lbw',
                                                  covariates = 'drinking',
                                                  vecc = 1,
                                                  interaction = TRUE,
                                                  yreg = "logistic", mreg = "linear")
result_delta_bin_cont_int

result_delta_cont_cont_noint <- causalMediationDelta(data = df, 
                                                     outcome = 'death', 
                                                     treatment = 'smoking',
                                                     mediator = 'lbw',
                                                     covariates = 'drinking',
                                                     vecc = 1,
                                                     interaction = FALSE,
                                                     yreg = "linear", mreg = "linear")
result_delta_cont_cont_noint

result_delta_cont_cont_int <- causalMediationDelta(data = df, 
                                                   outcome = 'death', 
                                                   treatment = 'smoking',
                                                   mediator = 'lbw',
                                                   covariates = 'drinking',
                                                   vecc = 1,
                                                   interaction = TRUE,
                                                   yreg = "linear", mreg = "linear")
result_delta_cont_cont_int


