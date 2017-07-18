d <- read.csv("../../inst/data/Mbin_int_data.csv")
names(d)

cm <- causmed$new(data = d,
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
                  baseline = NULL)

cm$delta_marginal()
cm$delta_conditional()
cm$print_output(type = "full")
cm$print_output(type = "reduced")
sink("Mbin_int/linear_delta.txt")
cm$print_output(type = "full")
sink()

sas <- read.csv("../../inst/sasoutput/Mbin_int_linear_delta.csv")
sas <- sas[!is.na(sas$Obs), ]
sas
