causmed <- setRefClass("causmed",
                       
                       fields = list(
                         data  = "ANY", # data frame
                         outcome = "ANY",
                         treatment = "ANY",
                         mediator = "ANY",
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
                         
                         betas ="ANY", # coefficients from mediator regression
                         thetas = "ANY", # coefficients from outcome regression
                         vcov_betas = "ANY", # covariance from mediator regression
                         vcov_thetas = "ANY", # covariance from outcome regression
                         vcov_block = "ANY", # covariances from regression
                         
                         cde = "ANY",
                         nde = "ANY",
                         nie = "ANY",
                         
                         se_cde = "ANY",
                         se_nde = "ANY",
                         se_nie = "ANY",
                         
                         cde_boot = "ANY",
                         nde_boot = "ANY",
                         nie_boot = "ANY",
                         
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
    
    mediator_formula_basic <- paste(mediator, treatment, sep=' ~ ')
    outcome_formula_basic  <- paste(paste(outcome, treatment, sep=' ~ '), mediator, sep=' + ')
    
    if (.self$interaction) {
      outcome_formula_basic <- paste(outcome_formula_basic, paste(treatment, mediator, sep = '*'), sep = ' + ')
    }
    
    if (length(.self$covariates) == 0) {
      .self$mediator_formula <- mediator_formula_basic
      .self$outcome_formula  <- outcome_formula_basic
    } else {
      .self$mediator_formula <- paste(mediator_formula_basic, paste(covariates, collapse = " + "), sep = ' + ')
      .self$outcome_formula  <- paste(outcome_formula_basic,  paste(covariates, collapse = " + "), sep = ' + ')
    }
    
    if (yreg == "coxph") {
      l <- strsplit(.self$outcome_formula, split = "~")
      l[[1]][1] <- paste0("Surv(", .self$outcome, ", ", .self$event, ")")
      .self$outcome_formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    }
    if (yreg == "aft_exp") {
      l <- strsplit(.self$outcome_formula, split = "~")
      l[[1]][1] <- paste0("Surv(", .self$outcome, ", ", .self$event, ")")
      .self$outcome_formula <- paste(l[[1]][1], l[[1]][2], sep = " ~ ")
    }
    if (yreg == "aft_weibull") {
      l <- strsplit(outcome_formula, split = "~")
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
    ## Store coefficients from regression
    .self$betas  <- coefficients(.self$mediator_regression)
    .self$thetas <- coefficients(.self$outcome_regression)
    ## Store covariances from regression
    .self$vcov_betas <- vcov(.self$mediator_regression)
    .self$vcov_thetas <- vcov(.self$outcome_regression)
    ## Build block diagonal matrix
    .self$vcov_block <- bdiag(.self$vcov_thetas, .self$vcov_betas)
  }
)

##----- CDE

causmed$methods(
  CDE_bin = function() { 
    ORcde <- exp(.self$thetas[.self$treatment] + 
                   ifelse(.self$interaction, .self$thetas[paste(.self$treatment, .self$mediator, sep = ':')] * .self$m, 0) * (.self$a - .self$a_star))
    unname(ORcde)
  }
)

causmed$methods(
  CDE_bin_delta = function() {
    s <- ifelse(.self$interaction,
                paste0("~ exp((x2 + x", length(.self$thetas), " * m) * (a - a_star))"),
                paste0(" ~exp(x2 * (a - a_star))"))
    s <- stringr::str_replace_all(s, pattern = c("\\ba_star\\b" = .self$a_star, "\\ba\\b" = a, "\\bm\\b" = .self$m))
    return(as.formula(s))
  }
)

causmed$methods(
  CDE_cont = function() {
    .self$cde <- (.self$thetas[.self$treatment] +
                    ifelse(.self$interaction, .self$thetas[paste(.self$treatment, .self$mediator, sep = ':')] * m, 0)) * (.self$a - .self$a_star)
    unname(.self$cde)
  }
)

causmed$methods(
  CDE_cont_delta = function() {
    s <- ifelse(.self$interaction,
                paste0("~ (x2 + x", length(.self$thetas), " * m) * (a - a_star)"),
                paste0(" ~ x2 * (a - a_star)"))
    s <- stringr::str_replace_all(s, pattern = c("\\ba_star\\b" = .self$a_star, "\\ba\\b" = .self$a, "\\bm\\b" = .self$m))
    return(as.formula(s))
  }
)

causmed$methods(
  CDE_estimate = function() {
    if (.self$mreg != "linear" & .self$yreg != "linear")
      .self$cde <- .self$CDE_bin()
    else if (.self$mreg != "linear" & .self$yreg == "linear")
      .self$cde <- .self$CDE_cont()
    else if (.self$mreg == "linear" & .self$yreg != "linear")
      .self$cde <- .self$CDE_bin()
    else if (.self$mreg == "linear" & .self$yreg == "linear")
      .self$cde <- .self$CDE_cont() # TODO: should be cde_boot
  }
)

causmed$methods(
  CDE_delta = function() {
    if (.self$mreg != "linear" & yreg != "linear")
      .self$cde <- .self$CDE_bin_delta()
    else if (mreg != "linear" & yreg == "linear")
      .self$cde <- .self$CDE_cont_delta()
    else if (mreg == "linear" & yreg != "linear")
      .self$cde <- .self$CDE_bin_delta()
    else if (mreg == "linear" & yreg == "linear")
      .self$cde <- .self$CDE_cont_delta()
  }
)

##----- Bootstrap

causmed$methods(
  boostrap_step = function(data, indices) {
    data <- data[indices, ]
    .self$run_regressions(data_regression = data)
    .self$get_coef()
    .self$CDE_estimate()
    return(.self$cde)
  }
)

causmed$methods(
  boostrap = function() {
    boot(
      data = .self$data,
      statistic = .self$boostrap_step,
      R = .self$nboot
    )
  }
)


##----- Print methods

# causmed$methods(
#   show = function() {
#     # TODO
#   }
# )


