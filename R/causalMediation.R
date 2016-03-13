causalMediationDelta <- function(data, outcome, treatment, mediator, covariates, cval = NULL,
                                 interaction = TRUE,
                                 mreg = c("linear", "logistic"),
                                 yreg = c("linear", "logistic", "loglinear", "poisson",
                                          "quasipoisson", "negbin", "coxph", "aft_exp", "aft_weibull"),
                                 event = NULL,
                                 m = 0,
                                 casecontrol = FALSE, baseline = 0) {
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
  betas  <<- coefficients(mediator.regression)
  thetas <<- coefficients(outcome.regression)
  ## Store covariances from regression
  vcov_betas <<- vcov(mediator.regression)
  vcov_thetas <<- vcov(outcome.regression)
  ## Build block diagonal matrix
  vcov_block <<- bdiag(vcov_thetas, vcov_betas)
  
  # print(betas)
  
  variance <- (summary(mediator.regression)$sigma)^2
  
  if (mreg != "linear" & yreg != "linear") {
    cded <- CDE_bin_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnded <- pure_NDE_binbin_delta(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnded <- total_NDE_binbin_delta(betas = betas, thetas = thetas, treatment = treatment,
                                  mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    pnied <- pure_NIE_binbin_delta(betas = betas, thetas = thetas, treatment = treatment,
                                   mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnied <- total_NIE_binbin_delta(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    ted <- total_effect_delta(ycont = FALSE)
    pmd <- proportion_mediated_delta(ycont = FALSE)
    
    
    cde <- CDE_bin(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnde <- pure_NDE_binbin(betas = betas, thetas = thetas, treatment = treatment,
                                   mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnde <- total_NDE_binbin(betas = betas, thetas = thetas, treatment = treatment,
                                    mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    pnie <- pure_NIE_binbin(betas = betas, thetas = thetas, treatment = treatment,
                                   mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnie <- total_NIE_binbin(betas = betas, thetas = thetas, treatment = treatment,
                                    mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    te <- total_effect(pnde, tnie, ycont = FALSE)
    pm <- proportion_mediated(pnde, tnie, te, ycont = FALSE)
    
   te <- pnde * tnie
  } else if (mreg != "linear" & yreg == "linear") {
    cded <- CDE_cont_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnded <- pure_NDE_bincont_delta(betas = betas, thetas = thetas, treatment = treatment,
                       mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnded <- total_NDE_bincont_delta(betas = betas, thetas = thetas, treatment = treatment,
                               mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    pnied <- pure_NIE_bincont_delta(betas = betas, thetas = thetas, treatment = treatment,
                                     mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnied <- total_NIE_bincont_delta(betas = betas, thetas = thetas, treatment = treatment,
                       mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    ted <- total_effect_delta(ycont = TRUE)
    pmd <- proportion_mediated_delta(ycont = TRUE)
    
    
    cde <- CDE_cont(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnde <- pure_NDE_bincont(betas = betas, thetas = thetas, treatment = treatment,
                                    mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnde <- total_NDE_bincont(betas = betas, thetas = thetas, treatment = treatment,
                                     mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    pnie <- pure_NIE_bincont(betas = betas, thetas = thetas, treatment = treatment,
                                    mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnie <- total_NIE_bincont(betas = betas, thetas = thetas, treatment = treatment,
                                     mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    te <- total_effect(pnde, tnie, ycont = TRUE)
    pm <- proportion_mediated(pnde, tnie, te, ycont = TRUE)
    
       # te <- pnde + tnie
  } else if (mreg == "linear" & yreg != "linear") {
    cded <- CDE_bin_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnded <- pure_NDE_contbin_delta(betas = betas, thetas = thetas, treatment = treatment,
                       mediator = mediator, covariates = covariates, cval = cval, 
                       variance = variance, interaction = interaction)
    tnded <- total_NDE_contbin_delta(betas = betas, thetas = thetas, treatment = treatment,
                                      mediator = mediator, covariates = covariates, cval = cval, 
                                      variance = variance, interaction = interaction)
    pnied <- pure_NIE_contbin_delta(betas = betas, thetas = thetas, treatment = treatment,
                                     mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnied <- total_NIE_contbin_delta(betas = betas, thetas = thetas, treatment = treatment,
                       mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    ted <- total_effect_delta(ycont = FALSE)
    pmd <- proportion_mediated_delta(ycont = FALSE)
    
     
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
     te <- total_effect(pnde, tnie, ycont = FALSE)
     pm <- proportion_mediated(pnde, tnie, te, ycont = FALSE)
     
          #   te <- pnde * tnie
  } else if (mreg == "linear" & yreg == "linear") {
    
    cded <<- CDE_cont_delta(thetas = thetas, interaction = interaction)
    pnded <<- pure_NDE_contcont_delta(thetas = thetas, vecc = cval, interaction = interaction)
    tnded <<- total_NDE_contcont_delta( thetas = thetas, vecc = cval, interaction = interaction)
    pnied <- pure_NIE_contcont_delta(thetas = thetas, interaction = interaction)
    tnied <<- total_NIE_contcont_delta( thetas = thetas, interaction = interaction)
    ted <<- total_effect_delta(ycont = TRUE)
    pmd <<- proportion_mediated_delta(ycont = TRUE)
    
    cde <<- CDE_cont(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction)
    pnde <<- pure_NDE_contcont(betas = betas, thetas = thetas, treatment = treatment,
                            mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnde <<- total_NDE_contcont(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    pnie <<- pure_NIE_contcont(betas = betas, thetas = thetas, treatment = treatment,
                            mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    tnie <<- total_NIE_contcont(betas = betas, thetas = thetas, treatment = treatment,
                             mediator = mediator, covariates = covariates, cval = cval, interaction = interaction)
    te <<- total_effect(pnde, tnie, ycont = TRUE)
    pm <<- proportion_mediated(pnde, tnie, te, ycont = TRUE)
    
  }
  
  se.cde <- deltamethod(cded, thetas, vcov_thetas)
  se.pnde <- deltamethod(pnded, c(thetas, betas), vcov_block)
  se.tnde <- deltamethod(tnded,  c(thetas, betas), vcov_block)
  se.pnie <- deltamethod(pnied,  c(thetas, betas), vcov_block)
  se.tnie <- deltamethod(tnied,  c(thetas, betas), vcov_block)
  se.te <- deltamethod(ted, c(pnde, tnie), bdiag(se.pnde, se.tnie))
  se.pm <- deltamethod(pmd, c(pnde, tnie, te), bdiag(se.pnde, se.tnie, se.te))
  
  return(c(cded = cded, cde = cde, se.cde = se.cde, pnded = pnded, pnde = pnde, 
           se.pnde = se.pnde, tnded = tnded, tnde = tnde, se.tnde = se.tnde,
           pnied = pnied, pnie = pnie, se.pnie = se.pnie, tnied = tnied, tnie = tnie, se.tnie = se.tnie,
           te = te, se.te = se.te,
           pm = pm, se.pm = se.pm))
}

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
    te <- nde * nie
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
    te <- nde + nie
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
    te <- nde * nie
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
    te <- nde + nie
  }
  
  return(c(cde, pnde,tndem, pnie,tnie,te))
}

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
