df <- data.frame('smoking'    = c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
                 'lbw'        = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'death'      = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'drinking'   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
                 'agebelow20' = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)


result_delta_bin <- causalMediationDelta(data = df, 
                                         outcome = 'death', 
                                         treatment = 'smoking',
                                         mediator = 'lbw',
                                         covariates = 'drinking',
                                         vecc = 1,
                                         interaction = FALSE,
                                         yreg = "logistic", mreg = "logistic")
result_delta_bin
