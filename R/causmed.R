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
    .self$m <- event
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
    
    if (.self$interaction == TRUE) {
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
  run_regressions = function() {
    if (is.null(.self$covariates) & !is.null(.self$vecc)) {
      warning("Incompatible arguments")
    } else if (!is.null(.self$covariates) & is.null(.self$vecc)) {
      .self$vecc <- colMeans(as.data.frame(.self$data[, .self$covariates]))
    }
    
    if (.self$mreg == "linear") {
      .self$mediator_regression <- lm(.self$mediator_formula, data = .self$data)
    } else {
      if (.self$casecontrol == TRUE) {
        .self$data <- .self$data[.self$data[[.self$outcome]] == .self$baseline, ] # TODO: don't overwrite data
      }
      .self$mediator_regression <- glm(.self$mediator_formula, family = binomial(), data = .self$data)
    }
    
    if (yreg == "linear") {
      .self$outcome_regression  <- lm(.self$outcome_formula, data = .self$data)
    }
    if (yreg == "logistic") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = binomial(), data = .self$data)
    }
    if (yreg == "loglinear") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = binomial("log"), data = .self$data)
    }
    if (yreg == "poisson") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = poisson(), data = .self$data)
    }
    if (yreg == "quasipoisson") {
      .self$outcome_regression  <- glm(.self$outcome_formula, family = quasipoisson(), data = .self$data)
    }
    if (yreg == "negbin") {
      .self$outcome_regression  <- glm.nb(.self$outcome_formula, data = .self$data)
    }
    if (yreg == "coxph") {
      .self$outcome_regression <- coxph(as.formula(.self$outcome_formula), data = .self$data)
    }
    if (yreg == "aft_exp") {
      .self$outcome_regression <- survreg(as.formula(.self$outcome_formula), dist = "exponential", data = .self$data)
    }
    if (yreg == "aft_weibull") {
      .self$outcome_regression <- survreg(as.formula(.self$outcome_formula), dist = "weibull", data = .self$data)
    }
  }
)

##----- Example

df <- read.csv("~/Documents/LocalGit/causalMediation/data/Mbin_int_data_10000.txt", sep = " ")

cm <- causmed$new(data = df,
                  outcome = "Y_cont_int",
                  treatment = 'A',
                  mediator = 'M_bin',
                  covariates = "C",
                  interaction = TRUE,
                  yreg = "linear", mreg = "logistic",
                  boot = TRUE, nboot = 500, event = NULL, a_star = 0, a = 1)
cm
cm$create_formulas()
cm$outcome_formula
cm$run_regressions()
cm$outcome_regression
cm$mediator_formula
cm$mediator_regression      
