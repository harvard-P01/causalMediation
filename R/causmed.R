causmed <- setRefClass("causmed",
                       
                       fields = list(
                         data  = "ANY", # data frame
                         outcome = "character",
                         treatment = "character",
                         mediator = "character",
                         covariates = "ANY",
                         vecc = "ANY",
                         interaction = "logical", # interaction term
                         boot = "logical", # use bootstrap
                         nboot = "numeric", # number of bootstraps to use
                         mreg = "character", # regression type
                         yreg = "character", # regression type
                         event = "ANY",
                         m = "ANY",
                         a_star = "ANY",
                         a = "ANY",
                         casecontrol = "logical",
                         baseline = "ANY",
                         
                         mediator_formula = "ANY",
                         outcome_formula = "ANY",
                         
                         mediator_regression = "ANY",
                         outcome_regression = "ANY",
                         
                         betas = "ANY", # coefficients from mediator regression
                         thetas = "ANY", # coefficients from outcome regression
                         vcov_betas = "ANY", # covariance from mediator regression
                         vcov_thetas = "ANY", # covariance from outcome regression
                         vcov_block = "ANY", # covariances from regression
                         
                         cde_delta = "ANY",
                         cde_boot = "ANY", # TODO: rename to x_estimate?
                         se_cde_delta = "ANY",
                         
                         nde_delta = "ANY",
                         nde_boot = "ANY", # TODO: rename to x_estimate?
                         se_pnde_delta = "ANY",
                         se_tnde_delta = "ANY",
                         
                         nie_delta = "ANY",
                         nie_boot = "ANY", # TODO: rename to x_estimate?
                         se_pnie_delta = "ANY",
                         se_tnie_delta = "ANY",
                         
                         pm_boot = "ANY", # TODO: rename to x_estimate?S
                         pm_delta = "ANY",
                         se_pm_delta = "ANY",
                         
                         te_boot = "ANY", # TODO: rename to x_estimate?
                         te_delta = "ANY",
                         se_te_delta = "ANY",
                         
                         boot_out = "ANY", # bootstrap output
                         delta_out = "ANY", # delta output
                         
                         authors = "character" # Package authors
                       )
)

causmed$methods(
  initialize = function(data, outcome, treatment, mediator, covariates, vecc = NULL,
                      interaction = TRUE, boot = FALSE, nboot = 100,
                      mreg = c("linear", "logistic"),
                      yreg = c("linear", "logistic", "loglinear", "poisson",
                               "quasipoisson", "negbin", "coxph", "aft_exp", "aft_weibull"),
                      event, m, a_star, a,
                      casecontrol = FALSE, baseline = 0) {
    .self$authors <- "TBD"
    .self$data <- data
    .self$outcome <- outcome
    .self$treatment <- treatment
    .self$mediator <- mediator
    .self$covariates <- covariates
    .self$vecc <- vecc
    .self$interaction <- interaction
    .self$boot <- boot
    .self$nboot <- nboot
    .self$mreg <- mreg
    .self$yreg <- yreg
    .self$event <- event
    .self$m <- m
    .self$a_star <- a_star
    .self$a <- a
    .self$casecontrol <- casecontrol
    .self$baseline <- baseline
  }
)

causmed$methods(
  create_formulas = function() {
    
    mediator_formula_basic <- paste(mediator, treatment, sep = " ~ ")
    outcome_formula_basic  <- paste(paste(outcome, treatment, sep = " ~ "),
                                    mediator,
                                    sep = " + ")
    
    if (.self$interaction) {
      outcome_formula_basic <- paste(outcome_formula_basic,
                                     paste(treatment, mediator, sep = " * "),
                                     sep = ' + ')
    }
    
    if (length(.self$covariates) == 0) {
      .self$mediator_formula <- mediator_formula_basic
      .self$outcome_formula  <- outcome_formula_basic
    } else {
      .self$mediator_formula <- paste(mediator_formula_basic,
                                      paste(covariates, collapse = " + "),
                                      sep = " + ")
      .self$outcome_formula  <- paste(outcome_formula_basic,
                                      paste(covariates, collapse = " + "),
                                      sep = " + ")
    }
    
    if (yreg == "coxph") {
      l <- strsplit(.self$outcome_formula, split = " ~ ")
      l[[1]][1] <- paste0("Surv(", .self$outcome, ", ", .self$event, ")")
      .self$outcome_formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    }
    if (yreg == "aft_exp") {
      l <- strsplit(.self$outcome_formula, split = " ~ ")
      l[[1]][1] <- paste0("Surv(", .self$outcome, ", ", .self$event, ")")
      .self$outcome_formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    }
    if (yreg == "aft_weibull") {
      l <- strsplit(outcome_formula, split = " ~ ")
      l[[1]][1] <- paste0("Surv(", .self$outcome, ", ", .self$event, ")")
      .self$outcome_formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    }
  }
)

causmed$methods(
  run_regressions = function(data_regression = NULL) {
    
    if (is.null(data_regression)) {
      data_regression <- .self$data
    }
    
    if (is.null(.self$covariates) & !is.null(.self$vecc)) {
      warning("Incompatible arguments")
    } else if (!is.null(.self$covariates) & is.null(.self$vecc)) {
      .self$vecc <- colMeans(as.data.frame(data_regression[, .self$covariates]))
    }
    
    if (.self$mreg == "linear") {
      .self$mediator_regression <- lm(.self$mediator_formula, data = data_regression)
    } else {
      if (.self$casecontrol) {
        data_regression <- data_regression[data_regression[[.self$outcome]] == .self$baseline, ] # TODO: don't overwrite data
      }
      .self$mediator_regression <- glm(.self$mediator_formula, family = binomial(), data = data_regression)
    }
    
    if (yreg == "linear") {
      .self$outcome_regression  <- lm(.self$outcome_formula, data = data_regression)
    }
    if (yreg == "logistic") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = binomial(), data = data_regression)
    }
    if (yreg == "loglinear") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = binomial("log"), data = data_regression)
    }
    if (yreg == "poisson") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = poisson(), data = data_regression)
    }
    if (yreg == "quasipoisson") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = quasipoisson(), data = data_regression)
    }
    if (yreg == "negbin") {
      .self$outcome_regression  <- glm.nb(.self$outcome_formula, data = data_regression)
    }
    if (yreg == "coxph") {
      .self$outcome_regression <- coxph(as.formula(.self$outcome_formula), data = data_regression)
    }
    if (yreg == "aft_exp") {
      .self$outcome_regression <- survreg(as.formula(.self$outcome_formula), dist = "exponential", data = data_regression)
    }
    if (yreg == "aft_weibull") {
      .self$outcome_regression <- survreg(as.formula(.self$outcome_formula), dist = "weibull", data = data_regression)
    }
  }
)

causmed$methods(
  get_coef = function() {
    ## Store coefficients from regressions
    .self$betas  <- coefficients(.self$mediator_regression)
    .self$thetas <- coefficients(.self$outcome_regression)
    ## Store covariances from regressions
    .self$vcov_betas <- vcov(.self$mediator_regression)
    .self$vcov_thetas <- vcov(.self$outcome_regression)
    ## Build block diagonal matrix
    .self$vcov_block <- bdiag(.self$vcov_thetas, .self$vcov_betas)
  }
)

##----- CDE

causmed$methods(
  CDE_boot = function() {
      .self$cde_boot <- CDE_boot_function(.self$thetas, .self$treatment,.self$mediator,
                                          .self$m, .self$a_star, .self$a, .self$interaction)
      .self$cde_boot<- CDE_cont(.self$thetas, .self$treatment, .self$mediator,
                                .self$m, .self$a_star, .self$a, .self$interaction)
  }
)

causmed$methods(
  CDE_delta = function() {
      .self$cde_delta <- CDE_delta_function(.self$thetas, .self$treatment, .self$mediator,
                                            .self$m, .self$a_star, .self$a, .self$interaction)
    .self$se_cde_delta <- deltamethod(.self$cde_delta, .self$thetas, .self$vcov_thetas)
  }
)

##----- NDE

causmed$methods(
  NDE_boot = function() {
    .self$nde_boot<- NDE_boot_function(.self$betas, .self$thetas,
                                       .self$treatment, .self$mediator, .self$covariates, .self$vecc,
                                       .self$m, .self$interaction, .self$a_star, .self$a, .self$variance,
                                       .self$mreg, .self$yreg)
  }
)

causmed$methods(
  NDE_delta = function() {
    .self$nde_delta <- NDE_delta_function(.self$thetas, .self$treatment, .self$mediator,
                                          .self$m, .self$interaction, .self$vecc,
                                          .self$a_star, .self$a, .self$variance,
                                          .self$mreg, .self$yreg)
    .self$se_pnde_delta <- deltamethod(.self$nde_delta$pnded,
                                       c(.self$thetas, .self$betas),
                                       .self$vcov_block)
    .self$se_tnde_delta<- deltamethod(.self$nde_delta$tnded,
                                      c(.self$thetas, .self$betas),
                                      .self$vcov_block)
  }
)

##----- NIE

causmed$methods(
  NIE_boot = function() {
    .self$nie_boot<- NIE_boot_function(.self$betas, .self$thetas, .self$treatment,
                                       .self$mediator, .self$covariates, .self$vecc,
                                       .self$m, .self$interaction,
                                       .self$a_star, .self$a,
                                       .self$mreg, .self$yreg)
  }
)

causmed$methods(
  NIE_delta = function() {
    .self$nie_delta <- NIE_delta_function(.self$thetas,.self$treatment, .self$mediator, 
                                          .self$m, .self$vecc, .self$interaction,
                                          .self$a_star, .self$a,
                                          .self$mreg, .self$yreg)
    .self$se_pnie_delta <- deltamethod(.self$nie_delta$pnied,
                                       c(.self$thetas, .self$betas),
                                       .self$vcov_block)
    .self$se_tnie_delta<- deltamethod(.self$nie_delta$tnied,
                                      c(.self$thetas, .self$betas),
                                      .self$vcov_block)
  }
)

##----- Total effect

causmed$methods(
  total_effect_boot = function() {
    .self$te_boot <- total_effect_boot_function(.self$nde_boot$pnde,
                                                .self$nie_boot$tnie,
                                                ycont = (.self$yreg == "linear"))
  }
)

causmed$methods(
  total_effect_delta = function() {
    .self$te_delta <- total_effect_delta_function(ycont = (.self$yreg == "linear"))
    .self$se_te_delta <- deltamethod(.self$te_delta,
                                     c(.self$nde_boot$pnde, .self$nie_boot$tnie),
                                     bdiag(.self$se_pnde_delta^2, .self$se_tnie_delta^2))
  }
)

##----- Proportion mediated

causmed$methods(
  proportion_mediated_boot = function() {
    .self$pm_boot <- proportion_mediated_boot_function(.self$nde_boot$pnde,
                                                       .self$nie_boot$tnie,
                                                       .self$te_boot,
                                                       ycont = (.self$yreg == "linear"))
  }
)

causmed$methods(
  proportion_mediated_delta = function() {
    .self$pm_delta <-  proportion_mediated_delta_function(ycont = (.self$yreg == "linear"))
    .self$se_pm_delta <- deltamethod(.self$pm_delta,
                                     c(.self$nde_boot$pnde, .self$nie_boot$tnie, .self$te_boot),
                                     bdiag(.self$se_pnde_delta^2, .self$se_tnie_delta^2, .self$se_te_delta^2))
  }
)

##----- Bootstrap

causmed$methods(
  boostrap_step = function(data, indices) {
    data_boot <- data[indices, ]
    .self$run_regressions(data_regression = data_boot)
    .self$get_coef()
    .self$CDE_boot()
    .self$NDE_boot()
    .self$NIE_boot()
    .self$total_effect_boot()
    .self$proportion_mediated_boot()
    return(as.numeric(c(cde = .self$cde_boot,
                        pnde = .self$nde_boot$pnde, tnde = .self$nde_boot$tnde,
                        pnie = .self$nie_boot$pnie, tnie = .self$nie_boot$tnie,
                        te = .self$te_boot,
                        pm = .self$pm_boot)))
  }
)

##----- 'Main' methods

causmed$methods(
  bootstrap = function() {
    .self$boot_out <- boot(
      data = .self$data,
      statistic = .self$boostrap_step,
      R = .self$nboot
    )
  }
)

causmed$methods(
  delta = function() {
    .self$create_formulas()
    .self$run_regressions(data_regression = .self$data)
    .self$get_coef()
    .self$CDE_boot(); .self$CDE_delta()
    .self$NDE_boot(); .self$NDE_delta()
    .self$NIE_boot(); .self$NIE_delta()
    .self$total_effect_boot(); .self$total_effect_delta()
    .self$proportion_mediated_boot(); .self$proportion_mediated_boot()
  }
)

##----- Print methods

# causmed$methods(
#   show = function() {
#     # TODO
#   }
# )

causmed$methods(
  print_boot = function() {
    format_df_boot(.self$boot_out)
  }
)

causmed$methods(
  print_delta = function() {
    format_df_boot(.self$delta_out)
  }
)


