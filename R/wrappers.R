causal_mediation <- function(data, outcome, treatment, mediator, covariates, vecc = NULL,
                             interaction = TRUE, boot = NULL, nboot = 100,
                             mreg = c("linear", "logistic"),
                             yreg = c("linear", "logistic", "loglinear", "poisson",
                                      "quasipoisson", "negbin", "coxph", "aft_exp", "aft_weibull"),
                             event = NULL, m = 0, a_star = 1, a = 0,
                             casecontrol = FALSE, baseline = 0) {
  obj <- causmed$new(data = data,
                    outcome = outcome,
                    treatment = treatment,
                    mediator = mediator,
                    covariates = covariates,
                    vecc = vecc,
                    interaction = interaction,
                    boot = boot,
                    nboot = nboot,
                    mreg = mreg,
                    yreg = yreg,
                    event = event,
                    m = m,
                    a_star = a_star,
                    a = a,
                    casecontrol = casecontrol,
                    baseline = baseline)
  return(obj)
}

delta_marginal <- function(obj) {
  obj$delta_marginal()
}

print_delta <- function(obj, type = "marginal") {
  obj$print_delta(type = type)
}
