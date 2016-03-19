causalMediation <- function(data, outcome, treatment, mediator, covariates, cval = NULL,
                            interaction = TRUE, boot = TRUE, nboot = 100,
                            mreg = c("linear", "logistic"),
                            yreg = c("linear", "logistic", "loglinear", "poisson",
                                     "quasipoisson", "negbin", "coxph", "aft_exp", "aft_weibull"),
                            event,
                            casecontrol = FALSE, baseline = 0) {
  
  if (boot) {
    result <-  boot(data = data, statistic = causalMediationOneStep, R = nboot,
                    outcome = outcome, 
                    treatment = treatment,
                    mediator = mediator,
                    covariates = covariates,
                    cval = cval,
                    interaction = interaction,
                    yreg = yreg, mreg = mreg)
  } else {
      return("'causalMediationDelta' not implemented yet!")
    }
  class(result) <- "causmed"
  return(result)
}

print.causmed <- function(x) {
  boot.ci(x, type = "perc")
}
