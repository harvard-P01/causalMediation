## SAS code line 401
## PDF pp. 1--5

d <- read.csv("../../inst/data/Mbin_int_data.csv")
names(d)

cm <- causmed$new(
  data = d,
  outcome = "Y_cont_int",
  treatment = "A",
  mediator = "M_bin",
  covariates = "C",
  vecc = 1,
  interaction = TRUE,
  boot = FALSE,
  nboot = 100,
  mreg = "logistic",
  yreg = "linear",
  event = NULL,
  m = 0,
  a_star = 0,
  a = 1,
  casecontrol = FALSE,
  baseline = NULL
)

run_test(
  cm,
  filename = "Mbin_int/linear_delta.txt",
  sas = read.csv("../../inst/sasoutput/Mbin_int_linear_delta.csv")
)
