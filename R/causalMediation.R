causalMediation <- function(data, outcome, treatment, mediator, covariates, vecc = NULL,
                            interaction = TRUE, boot = TRUE, nboot = 100,
                            mreg = c("linear", "logistic"),
                            yreg = c("linear", "logistic", "loglinear", "poisson",
                                     "quasipoisson", "negbin", "coxph", "aft_exp", "aft_weibull"),
                            event, m = 0, a_star = 0, a = 1,
                            casecontrol = FALSE, baseline = 0) {
  
  if (boot) {
    if (nboot < 2)
      stop("Not enough bootstrap simulations, please specify a larger value for 'nboot'.")
    if (nboot < 49)
      warning("Low number of boostrap simulations 'nboot'.")
    result <-  boot(data = data, statistic = causalMediationAll, R = nboot,
                    outcome = outcome, 
                    treatment = treatment,
                    mediator = mediator,
                    covariates = covariates,
                    vecc = vecc,
                    interaction = interaction,
                    yreg = yreg, mreg = mreg, boot = boot)
    return(result)
  } else {
    result <- causalMediationAll(data = data, outcome = outcome, treatment = treatment, mediator = mediator,
                                 covariates = covariates, vecc = vecc,
                                 interaction = interaction,
                                 mreg = mreg, yreg = yreg,
                                 m = m, event = event,
                                 a_star = a_star, a = a, 
                                 casecontrol = casecontrol, baseline = baseline, boot = boot)
    class(result) <- "causmed.delta"
  }
  return(result)
}

print.boot <- function(x) {
  print(format_df_boot(x))
}

print.causmed.delta <- function(x) {
  print(format_df_delta(x))
}



