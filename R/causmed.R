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
                         
                         vecc_marginal = "ANY",
                         vecc_conditional = "ANY",
                         
                         data_name = "character", # data name
                         
                         mediator_formula = "ANY",
                         outcome_formula = "ANY",
                         
                         mediator_regression = "ANY",
                         outcome_regression = "ANY",
                         
                         betas = "ANY", # coefficients from mediator regression
                         thetas = "ANY", # coefficients from outcome regression
                         vcov_betas = "matrix", # covariance from mediator regression
                         vcov_thetas = "matrix", # covariance from outcome regression
                         vcov_block = "dgCMatrix", # covariances from regression
                         
                         cde_delta = "ANY",
                         cde_boot = "ANY", # TODO: rename to x_estimate?
                         se_cde_delta = "numeric",
                         
                         nde_delta = "ANY",
                         nde_boot = "ANY", # TODO: rename to x_estimate?
                         se_pnde_delta = "numeric",
                         se_tnde_delta = "numeric",
                         
                         nie_delta = "ANY",
                         nie_boot = "ANY", # TODO: rename to x_estimate?
                         se_pnie_delta = "numeric",
                         se_tnie_delta = "numeric",
                         
                         pm_boot = "ANY", # TODO: rename to x_estimate?
                         pm_delta = "ANY",
                         se_pm_delta = "numeric",
                         
                         te_boot = "ANY", # TODO: rename to x_estimate?
                         te_delta = "ANY",
                         se_te_delta = "numeric",
                         
                         boot_out = "ANY", # bootstrap output
                         delta_out = "list", # delta output
                         
                         boot_out_marginal = "ANY", # bootstrap output
                         delta_out_marginal = "list", # delta output
                         
                         boot_out_conditional = "ANY", # bootstrap output
                         delta_out_conditional = "list", # delta output
                         
                         conf = "numeric", # confidence level
                         
                         authors = "character" # Package authors
                       )
)

causmed$methods(
  initialize = function(data, outcome, treatment, mediator, covariates, vecc = NULL,
                        interaction = TRUE, boot = NULL, nboot = 100,
                        mreg = c("linear", "logistic"),
                        yreg = c("linear", "logistic", "loglinear", "poisson",
                                 "quasipoisson", "negbin", "coxph", "aft_exp", "aft_weibull"),
                        event = NULL, m = 0, a_star = 1, a = 0,
                        casecontrol = FALSE, baseline = 0) {
    .self$data_name <- deparse(substitute(data))
    .self$authors <- "TBD"
    .self$data <- data
    .self$outcome <- outcome
    .self$treatment <- treatment
    .self$mediator <- mediator
    .self$covariates <- covariates
    .self$vecc <- vecc
    .self$vecc_marginal <- vecc
    .self$vecc_conditional <- colMeans(as.data.frame(.self$data[, .self$covariates]))
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
    .self$conf <- 0.95
    .self$delta_out <- list()
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
                                     sep = " + ")
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
    
    if (.self$yreg == "coxph") {
      l <- strsplit(.self$outcome_formula, split = " ~ ")
      l[[1]][1] <- paste0("Surv(", .self$outcome, ", ", .self$event, ")")
      .self$outcome_formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    }
    if (.self$yreg == "aft_exp") {
      l <- strsplit(.self$outcome_formula, split = " ~ ")
      l[[1]][1] <- paste0("Surv(", .self$outcome, ", ", .self$event, ")")
      .self$outcome_formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    }
    if (.self$yreg == "aft_weibull") {
      l <- strsplit(.self$outcome_formula, split = " ~ ")
      l[[1]][1] <- paste0("Surv(", .self$outcome, ", ", .self$event, ")")
      .self$outcome_formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    }
  }
)

causmed$methods(
  run_regressions = function(data_regression = NULL, reduced = FALSE) {
    
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
    
    if (.self$yreg == "linear") {
      .self$outcome_regression  <- lm(.self$outcome_formula, data = data_regression)
    }
    if (.self$yreg == "logistic") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = binomial(), data = data_regression)
    }
    if (.self$yreg == "loglinear") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = binomial("log"), data = data_regression)
    }
    if (.self$yreg == "poisson") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = poisson(), data = data_regression)
    }
    if (.self$yreg == "quasipoisson") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = quasipoisson(), data = data_regression)
    }
    if (.self$yreg == "negbin") {
      .self$outcome_regression  <- glm.nb(.self$outcome_formula, data = data_regression)
    }
    if (.self$yreg == "coxph") {
      .self$outcome_regression <- coxph(as.formula(.self$outcome_formula), data = data_regression)
    }
    if (.self$yreg == "aft_exp") {
      .self$outcome_regression <- survreg(as.formula(.self$outcome_formula), dist = "exponential", data = data_regression)
    }
    if (.self$yreg == "aft_weibull") {
      .self$outcome_regression <- survreg(as.formula(.self$outcome_formula), dist = "weibull", data = data_regression)
    }
    ## Fix formulas
    ## To replace: glm(formula = .self$mediator_formula, family = binomial(),
    ##                 data = data_regression)
    ## with:       glm(formula = "M_bin ~ A + C", family = binomial(),
    ##                 data = my_data_name)
    .self$mediator_regression$call$formula <- as.formula(.self$mediator_formula)
    .self$outcome_regression$call$formula <- as.formula(.self$outcome_formula)
    .self$mediator_regression$call$data <- .self$data_name
    .self$outcome_regression$call$data <- .self$data_name
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
    if (!exists(".self$mediator_formula") | !exists(".self$outcome_formula"))
      .self$create_formulas()
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
    .self$boot <- TRUE
    .self$boot_out <- boot(
      data = .self$data,
      statistic = .self$boostrap_step,
      R = .self$nboot
    )
    return(boot(
      data = .self$data,
      statistic = .self$boostrap_step,
      R = .self$nboot
    ))
    .self$run_regressions(data_regression = .self$data) # regressions on the whole dataset
  }
)

causmed$methods(
  delta = function() {
    .self$boot <- FALSE
    .self$create_formulas()
    .self$run_regressions(data_regression = .self$data)
    .self$get_coef()
    .self$CDE_boot(); .self$CDE_delta()
    .self$NDE_boot(); .self$NDE_delta()
    .self$NIE_boot(); .self$NIE_delta()
    .self$total_effect_boot(); .self$total_effect_delta()
    .self$proportion_mediated_boot(); .self$proportion_mediated_delta()
    ## Populate delta_out field
    return(list(
      cded = .self$cde_boot$cde,
      se_cded = .self$se_cde_delta,
      pnde = .self$nde_boot$pnde,
      se_pnde = .self$se_pnde_delta,
      tnde = .self$nde_boot$tnde,
      se_tnde = .self$se_pnie_delta,
      pnie = .self$nie_boot$pnie,
      se_pnie = .self$se_pnie_delta,
      tnie = .self$nie_boot$tnie,
      se_tnie = .self$se_tnie_delta,
      te = .self$te_boot,
      se_te = .self$se_te_delta,
      pm = .self$pm_boot,
      se_pm = .self$se_pm_delta))
  }
)

##----- Marginal vs conditional

causmed$methods(
  bootstrap_marginal = function() {
    .self$vecc <- .self$vecc_marginal
    .self$boot_out_marginal <- .self$bootstrap()
  }
)

causmed$methods(
  bootstrap_conditional = function() {
    .self$vecc <- .self$vecc_conditional
    .self$boot_out_conditional <- .self$bootstrap()
  }
)

causmed$methods(
  delta_marginal = function() {
    .self$vecc <- .self$vecc_marginal
    .self$delta_out_marginal <- .self$delta()
  }
)

causmed$methods(
  delta_conditional = function() {
    .self$vecc <- .self$vecc_conditional
    .self$delta_out_conditional <- .self$delta()
  }
)

##----- Print methods

# causmed$methods(
#   show = function() {
#     # TODO
#   }
# )

causmed$methods(
  print_boot = function(digits = 2, conf = 0.95, type = c("marginal", "conditional")) {
    if (!is.null(conf))
      .self$conf <- conf
    if (type == "marginal")
      printCoefmat(format_df_boot(.self$boot_out_marginal, conf = .self$conf, n = nrow(.self$data)), digits = digits)
    else if (type == "conditional")
      printCoefmat(format_df_boot(.self$boot_out_conditional, conf = .self$conf, n = nrow(.self$data)), digits = digits)
  }
)

causmed$methods(
  print_delta = function(digits = 2, conf = 0.95, type = c("marginal", "conditional")) {
    if (!is.null(conf))
      .self$conf <- conf
    if (type == "marginal")
      printCoefmat(format_df_delta(.self$delta_out_marginal, conf = .self$conf, n = nrow(.self$data)),
                   digits = digits, has.Pvalue = TRUE)
    else if (type == "conditional")
      printCoefmat(format_df_delta(.self$delta_out_conditional, conf = .self$conf, n = nrow(.self$data)), ,
                         digits = digits, has.Pvalue = TRUE)
  }
)

causmed$methods(
  print_output = function(digits = 2, conf = 0.95, summary = TRUE, type = c("reduced", "full")) {
    if (!is.null(conf))
      .self$conf <- conf
    summary_mediator <- summary(.self$mediator_regression)
    summary_outcome <- summary(.self$outcome_regression)
    if (type == "reduced") {
      if (.self$boot)
        summary_coef <- .self$print_boot(digits = digits, conf = .self$conf, type = "marginal")
      else
        summary_coef <- .self$print_delta(digits = digits, conf = .self$conf, type = "marginal")
      cat("##----- MEDIATOR ------#\n")
      print(summary_mediator, digits = digits)
      cat("##----- OUTCOME -----#\n")
      print(summary_outcome, digits = digits)
      cat("##----- MARGINAL -----#\n\n")
      printCoefmat(summary_coef[c("cde", "pnde", "tnie", "te", "pm"), ], digits = digits, has.Pvalue = TRUE)
    }
    if (type == "full") {
      if (.self$boot) {
        summary_coef_marginal <- .self$print_boot(digits = digits, conf = .self$conf, type = "marginal")
        summary_coef_conditional <- .self$print_boot(digits = digits, conf = .self$conf, type = "conditional")
      }
      else {
        summary_coef_marginal <- .self$print_delta(digits = digits, conf = .self$conf, type = "marginal")
        summary_coef_conditional <- .self$print_delta(digits = digits, conf = .self$conf, type = "conditional")
      }
      cat("##----- MEDIATOR ------#\n")
      print(summary_mediator, digits = digits)
      cat("##----- OUTCOME -----#\n")
      print(summary_outcome, digits = digits)
      cat("##----- MARGINAL -----#\n\n")
      printCoefmat(summary_coef_marginal, digits = digits, has.Pvalue = TRUE)
      cat("\n##----- CONDITIONAL -----#\n\n")
      printCoefmat(summary_coef_conditional, digits = digits, has.Pvalue = TRUE)
    }
  }
)


##----- Integrate with other packages

causmed$methods(
  mediation = function(robustSE = TRUE, sims = 100, ...) {
    result <- mediation::mediate(.self$mediator_regression,
                                 .self$outcome_regression,
                                 treat = .self$treatment,
                                 mediator = .self$mediator,
                                 robustSE = robustSE,
                                 sims = sims, ...)
    return(summary(result))
  }
)

causmed$methods(
  medflex = function() {
    medflex_data <- medflex::neWeight(as.formula(.self$mediator_formula), data = .self$data)
    s <- gsub(pattern = .self$treatment,
              replacement = paste0("(", .self$treatment, "0 + ",   .self$treatment, "1",  ")"),
              .self$outcome_formula)
    medflex_formula <- gsub(pattern = .self$mediator, replacement = "0", s)
    result <- medflex::neModel(medflex_formula, expData = medflex_data)
    return(summary(result))
  }
)
