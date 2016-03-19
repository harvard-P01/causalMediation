causalMediationOneStep <- function(data, indices, outcome, treatment, mediator, covariates, cval = NULL,
                                   interaction = TRUE,
                                   mreg = c("linear", "logistic"),
                                   yreg = c("linear", "logistic", "loglinear", "poisson",
                                            "quasipoisson", "negcont", "coxph", "aft_exp", "aft_weibull"),
                                   event = NULL,
                                   casecontrol = FALSE, baseline = 0) {
  data <- data[indices, ]
  # print(head(data))
  ### Perform regressions (http://www.statmethods.net/stats/regression.html)
  #   Build formulas for mediator and outcome regression
  
  mediator.basic <- paste(mediator, treatment, sep=' ~ ')
  outcome.basic  <- paste(paste(outcome, treatment, sep=' ~ '), mediator, sep=' + ')
  
  if (interaction == TRUE) {
    outcome.basic <- paste(outcome.basic, paste(treatment, mediator, sep = '*'), sep = ' + ')
  }
  
  if (length(covariates) == 0) {
    mediator.formula <- mediator.basic
    outcome.formula  <- outcome.basic
  } else {
    mediator.formula <- paste(mediator.basic, paste(covariates, collapse = " + "), sep = ' + ')
    outcome.formula  <- paste(outcome.basic,  paste(covariates, collapse = " + "), sep = ' + ')
  }
  
  ### FIXME: hardcode to validate with SAS macro
  #   mediator.binary <- all(unique(data[, mediator]) %in% 0:1)
  #   outcome.binary <- all(unique(data[, outcome])  %in% 0:1)
  
  if (mreg == "linear") {
    mediator.regression <- lm(mediator.formula, data = data)
  } else {
    if (casecontrol == TRUE) {
      data <- data[data[[outcome]] == baseline, ]
    }
    mediator.regression <- glm(mediator.formula, family = binomial(), data = data)
  }
  
  if (yreg == "linear") {
    outcome.regression  <- lm(outcome.formula, data = data)
  }
  if (yreg == "logistic") {
    outcome.regression  <- glm(outcome.formula, family = binomial(), data = data)
  }
  if (yreg == "loglinear") {
    outcome.regression  <- glm(outcome.formula, family = binomial("log"), data = data)
  }
  if (yreg == "poisson") {
    outcome.regression  <- glm(outcome.formula, family = poisson(), data = data)
  }
  if (yreg == "quasipoisson") {
    outcome.regression  <- glm(outcome.formula, family = quasipoisson(), data = data)
  }
  if (yreg == "negbin") {
    outcome.regression  <- glm.nb(outcome.formula, data = data)
  }
  if (yreg == "coxph") {
    l <- strsplit(outcome.formula, split = "~")
    l[[1]][1] <- paste0("Surv(", outcome, ", ", event, ")")
    outcome.formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    outcome.regression <- coxph(as.formula(outcome.formula), data = data)
  }
  if (yreg == "aft_exp") {
    l <- strsplit(outcome.formula, split = "~")
    l[[1]][1] <- paste0("Surv(", outcome, ", ", event, ")")
    outcome.formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    outcome.regression <- survreg(as.formula(outcome.formula), dist = "exponential", data = data)
  }
  if (yreg == "aft_weibull") {
    l <- strsplit(outcome.formula, split = "~")
    l[[1]][1] <- paste0("Surv(", outcome, ", ", event, ")")
    outcome.formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    outcome.regression <- survreg(as.formula(outcome.formula), dist = "weibull", data = data)
  }
  ## Store coefficients from regression
  betas  <- coefficients(mediator.regression)
  thetas <- coefficients(outcome.regression)
  
  # print(betas)
  
  variance <- (summary(mediator.regression)$sigma)^2
  
  if (mreg != "linear" & yreg != "linear") {
    cde <- CDE_bin(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnde <- pure_NDE_binbin(betas = betas, thetas = thetas, treatment = treatment,
                            mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    
    tnde <- total_NDE_binbin(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    pnie <- pure_NIE_binbin(betas = betas, thetas = thetas, treatment = treatment,
                            mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnie <- total_NIE_binbin(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    te <- pnde * tnie
  } else if (mreg != "linear" & yreg == "linear") {
    cde <- CDE_cont(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnde <- pure_NDE_bincont(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnde <- total_NDE_bincont(betas = betas, thetas = thetas, treatment = treatment,
                              mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    pnie <- pure_NIE_bincont(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnie <- total_NIE_bincont(betas = betas, thetas = thetas, treatment = treatment,
                              mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    te <- pnde + tnie
  } else if (mreg == "linear" & yreg != "linear") {
    cde <- CDE_bin(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnde <- pure_NDE_contbin(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, 
                             variance = variance, interaction = interaction)
    tnde <- total_NDE_contbin(betas = betas, thetas = thetas, treatment = treatment,
                              mediator = mediator, covariates = covariates, cval = cval, 
                              variance = variance, interaction = interaction)
    pnie <- pure_NIE_contbin(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnie <- total_NIE_contbin(betas = betas, thetas = thetas, treatment = treatment,
                              mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    te <- pnde * tnie
  } else if (mreg == "linear" & yreg == "linear") {
    cde <- CDE_cont(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnde <- pure_NDE_contcont(betas = betas, thetas = thetas, treatment = treatment,
                              mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnde <- total_NDE_contcont(betas = betas, thetas = thetas, treatment = treatment,
                               mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    pnie <- pure_NIE_contcont(betas = betas, thetas = thetas, treatment = treatment,
                              mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnie <- total_NIE_contcont(betas = betas, thetas = thetas, treatment = treatment,
                               mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    te <- pnde + tnie
  }
  
  return(list(cde = cde, pnde = pnde, tnde = tnde, pnie = pnie, tnie = tnie, te = te))
}