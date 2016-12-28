data(Mbin_int_data_10000)
data(Mbin_int_data)

set.seed(1234)

cm <- causmed$new(data = Mbin_int_data,
                  outcome = "Y_cont_int",
                  treatment = 'A',
                  mediator = 'M_bin',
                  covariates = "C",
                  interaction = TRUE,
                  yreg = "linear", mreg = "logistic",
                  boot = TRUE, nboot = 500)
cm$bootstrap()
cm$print_boot(digits = 4)
cm$delta()
cm$print_delta(digits = 4)

cm <- causmed$new(data = Mbin_int_data_10000,
                  outcome = "Y_cont_int",
                  treatment = 'A',
                  mediator = 'M_bin',
                  covariates = "C",
                  interaction = TRUE,
                  yreg = "linear", mreg = "logistic",
                  boot = TRUE, nboot = 500)
cm$bootstrap()
cm$print_boot(digits = 4)
cm$delta()
cm$print_delta(digits = 4)

