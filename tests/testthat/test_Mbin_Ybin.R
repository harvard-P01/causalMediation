# Ybin Mbin

d <- read.csv("../../inst/data/Mbin_int_data.csv")
names(d)
#int & delta
cm <- causmed$new(data = d,
                  outcome = "Y_bin_int",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  vecc = 1,
                  interaction = TRUE,
                  boot = FALSE,
                  nboot = 0,
                  mreg = "logistic",
                  yreg = "logistic",
                  event = NULL,
                  m = 0,
                  a_star = 0,
                  a = 1,
                  casecontrol = FALSE,
                  baseline = NULL)
#int & boot
cm <- causmed$new(data = d,
                  outcome = "Y_bin_int",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  vecc = 1,
                  interaction = TRUE,
                  boot = TRUE,
                  nboot = 1000,
                  mreg = "logistic",
                  yreg = "logistic",
                  event = NULL,
                  m = 0,
                  a_star = 0,
                  a = 1,
                  casecontrol = FALSE,
                  baseline = NULL)


#int & delta & case control
d <- read.csv("../../inst/data/Mbin_cc_int_data.csv")
names(d)
cm <- causmed$new(data = d,
                  outcome = "Y_bin_int",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  vecc = 1,
                  interaction = TRUE,
                  boot = FALSE,
                  nboot = 0,
                  mreg = "logistic",
                  yreg = "logistic",
                  event = NULL,
                  m = 0,
                  a_star = 0,
                  a = 1,
                  casecontrol = TRUE,
                  baseline = NULL)
#int & boot & case control
cm <- causmed$new(data = d,
                  outcome = "Y_bin_int",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  vecc = 1,
                  interaction = TRUE,
                  boot = TRUE,
                  nboot = 1000,
                  mreg = "logistic",
                  yreg = "logistic",
                  event = NULL,
                  m = 0,
                  a_star = 0,
                  a = 1,
                  casecontrol = TRUE,
                  baseline = NULL)

#######################################
d <- read.csv("../../inst/data/Mbin_noint_data.csv")
names(d)
#noint & delta
cm <- causmed$new(data = d,
                  outcome = "Y_bin",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  vecc = 1,
                  interaction = FALSE,
                  boot = FALSE,
                  nboot = 0,
                  mreg = "logistic",
                  yreg = "logistic",
                  event = NULL,
                  m = 0,
                  a_star = 0,
                  a = 1,
                  casecontrol = FALSE,
                  baseline = NULL)
#noint & boot
cm <- causmed$new(data = d,
                  outcome = "Y_bin",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  vecc = 1,
                  interaction = FALSE,
                  boot = TRUE,
                  nboot = 1000,
                  mreg = "logistic",
                  yreg = "logistic",
                  event = NULL,
                  m = 0,
                  a_star = 0,
                  a = 1,
                  casecontrol = FALSE,
                  baseline = NULL)


#noint & delta & case control
d <- read.csv("../../inst/data/Mbin_cc_noint_data.csv")
names(d)
cm <- causmed$new(data = d,
                  outcome = "Y_bin",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  vecc = 1,
                  interaction = FALSE,
                  boot = FALSE,
                  nboot = 0,
                  mreg = "logistic",
                  yreg = "logistic",
                  event = NULL,
                  m = 0,
                  a_star = 0,
                  a = 1,
                  casecontrol = TRUE,
                  baseline = NULL)
#noint & boot & case control
cm <- causmed$new(data = d,
                  outcome = "Y_bin",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  vecc = 1,
                  interaction = FALSE,
                  boot = TRUE,
                  nboot = 1000,
                  mreg = "logistic",
                  yreg = "logistic",
                  event = NULL,
                  m = 0,
                  a_star = 0,
                  a = 1,
                  casecontrol = TRUE,
                  baseline = NULL)
