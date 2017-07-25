## PDF filename XXX of:
  # MBININTchecksoutput1132017.pdf
  # MBINNOINTchecksoutput1132017.pdf
  # MCONTINTchecksoutput1132017.pdf
  # MCONTNOINTchecksoutput1132017.pdf
### PDF page XXX

d <- read.csv("../../inst/data/XXX.csv")

cm <- causmed$new(
  data = d,
  outcome = "XXX",
  treatment = "XXX",
  mediator = "XXX",
  covariates = "XXX",
  vecc = 1,
  interaction = XXX,
  boot = FALSE,
  nboot = 100,
  mreg = "XXX",
  yreg = "XXX",
  event = XXX,
  m = 0,
  a_star = 0,
  a = 1,
  casecontrol = XXX,
  baseline = XXX
)

