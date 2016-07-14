causalMediationRegressions <- function(data, indices, outcome, treatment, mediator,
                                       covariates = NULL, vecc = NULL, interaction = TRUE,
                                       mreg = c("linear", "logistic"),
                                       yreg = c("linear", "logistic", "loglinear", "poisson",
                                                "quasipoisson", "negbin", "coxph", "aft_exp", "aft_weibull"),
                                       event = NULL,
                                       m = 0,
                                       a_star = 0, a = 1,
                                       casecontrol = FALSE, baseline = 0, boot = TRUE) {
  delta <- !boot
  
  if (boot)
    data <- data[indices, ]
  
  if (is.null(covariates) & !is.null(vecc)) {
    warning("Incompatible arguments")
  } else if (!is.null(covariates) & is.null(vecc)) {
    vecc <- colMeans(as.data.frame(data[, covariates]))
  }
  
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
  return(list(mediator.formula = mediator.formula,
              mediator.regression =  mediator.regression,
              outcome.formula = outcome.formula,
              outcome.regression =  outcome.regression))
}

causalMediationAll <- function(data, indices, outcome, treatment, mediator,
                               covariates = NULL, vecc = NULL, interaction = TRUE,
                               mreg = c("linear", "logistic"),
                               yreg = c("linear", "logistic", "loglinear", "poisson",
                                        "quasipoisson", "negbin", "coxph", "aft_exp", "aft_weibull"),
                               event = NULL,
                               m = 0,
                               a_star = 0, a = 1,
                               casecontrol = FALSE, baseline = 0, boot = TRUE) {
  
  delta <- !boot
  
  if (boot)
    data <- data[indices, ]
  
  if (is.null(covariates) & !is.null(vecc)) {
    warning("Incompatible arguments")
  } else if (!is.null(covariates) & is.null(vecc)) {
    vecc <- colMeans(as.data.frame(data[, covariates]))
  }
  
  l.regressions <- causalMediationRegressions(data = data,
                                              indices = indices,
                                              outcome = outcome,
                                              treatment = treatment,
                                              mediator = mediator,
                                              covariates = covariates, vecc = vecc, interaction = interaction,
                                              mreg = mreg,
                                              yreg = yreg,
                                              event = event,
                                              m = m,
                                              a_star = a_star, a = a,
                                              casecontrol = casecontrol, baseline = baseline, boot = boot)
  
  mediator.formula <- l.regressions$mediator.formula
  mediator.regression <- l.regressions$mediator.regression
  outcome.formula <- l.regressions$outcome.formula
  outcome.regression <- l.regressions$outcome.regression
 
   ## Store coefficients from regression
  betas  <- coefficients(mediator.regression)
  thetas <- coefficients(outcome.regression)
  ## Store covariances from regression
  vcov_betas <- vcov(mediator.regression)
  vcov_thetas <- vcov(outcome.regression)
  ## Build block diagonal matrix
  vcov_block <- bdiag(vcov_thetas, vcov_betas)
  
  variance <- (summary(mediator.regression)$sigma)^2
  
  cde <- CDE_estimate(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, mreg = mreg, yreg = yreg,
                      m = m, a_star = a_star, a = a)
  if (delta)
    cded <- CDE_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, mreg = mreg, yreg = yreg,
                      m = m, a_star = a_star, a = a)
  
  nde <- NDE_estimate(betas = betas, thetas = thetas, treatment = treatment, mediator = mediator, covariates = covariates,
                      vecc = vecc,
                      interaction = interaction, mreg = mreg, yreg = yreg,
                      m = m, a_star = a_star, a = a, variance = variance)
  if (delta)
    nded <- NDE_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, mreg = mreg, yreg = yreg,
                      m = m, vecc = vecc, a_star = a_star, a = a, variance = variance)
  
  nie <- NIE_estimate(betas = betas, thetas = thetas, treatment = treatment, mediator = mediator, covariates = covariates,
                      vecc = vecc,
                      interaction = interaction, mreg = mreg, yreg = yreg,
                      m = m, a_star = a_star, a = a)
  if (delta)
    nied <- NIE_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, mreg = mreg, yreg = yreg,
                      m = m, vecc = vecc, a_star = a_star, a = a)
  
  pnde <- nde$pnde
  tnde <- nde$tnde
  pnie <- nie$pnie
  tnie <- nie$tnie
  
  if (delta) {
    pnded <- nded$pnded
    tnded <- nded$tnde
    pnied <- nied$pnied
    tnied <- nied$tnie
  }
  
  if (mreg != "linear" & yreg != "linear") {
    
    if (delta) {
      ted <- total_effect_delta(ycont = FALSE)
      pmd <- proportion_mediated_delta(ycont = FALSE)
    }
    te <- total_effect(pnde, tnie, ycont = FALSE)
    pm <- proportion_mediated(pnde, tnie, te, ycont = FALSE)
    
  } else if (mreg != "linear" & yreg == "linear") {
    
    if (delta) {
      ted <- total_effect_delta(ycont = TRUE)
      pmd <- proportion_mediated_delta(ycont = TRUE)
    }
    te <- total_effect(pnde, tnie, ycont = TRUE)
    pm <- proportion_mediated(pnde, tnie, te, ycont = TRUE)
    
  } else if (mreg == "linear" & yreg != "linear") {
    
    if (delta) {
      ted <- total_effect_delta(ycont = FALSE)
      pmd <- proportion_mediated_delta(ycont = FALSE)
    }
    te <- total_effect(pnde, tnie, ycont = FALSE)
    pm <- proportion_mediated(pnde, tnie, te, ycont = FALSE)
    
  } else if (mreg == "linear" & yreg == "linear") {
    
    if (delta) {
      ted <- total_effect_delta(ycont = TRUE)
      pmd <- proportion_mediated_delta(ycont = TRUE)
    }
    te <- total_effect(pnde, tnie, ycont = TRUE)
    pm <- proportion_mediated(pnde, tnie, te, ycont = TRUE)
    
  }
  
  if (delta) {
    se.cde <- deltamethod(cded, thetas, vcov_thetas)
    se.pnde <- deltamethod(pnded, c(thetas, betas), vcov_block)
    se.tnde <- deltamethod(tnied,  c(thetas, betas), vcov_block)
    se.pnie <- deltamethod(pnied,  c(thetas, betas), vcov_block)
    se.tnie <- deltamethod(tnied,  c(thetas, betas), vcov_block)
    se.te <- deltamethod(ted, c(pnde, tnie), bdiag(se.pnde^2, se.tnie^2))
    se.pm <- deltamethod(pmd, c(pnde, tnie, te), bdiag(se.pnde^2, se.tnie^2, se.te^2))
  }
  
  if (delta)
    return(c(cded = cded, cde = cde, se.cde = se.cde, pnded = pnded, pnde = pnde, 
             se.pnde = se.pnde, tnded = tnded, tnde = tnde, se.tnde = se.tnde,
             pnied = pnied, pnie = pnie, se.pnie = se.pnie, tnied = tnied, tnie = tnie, se.tnie = se.tnie,
             te = te, se.te = se.te,
             pm = pm, se.pm = se.pm))
  else
    return(as.numeric(c(cde = cde, pnde = pnde, tnde = tnde, pnie = pnie, tnie = tnie, te = te, pm = pm)))
}