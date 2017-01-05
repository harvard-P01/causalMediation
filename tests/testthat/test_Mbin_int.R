# data(Mbin_int_data_10000)
data(Mbin_int_data)

set.seed(1234)

f <- function(outcome = "Y_cont_int", yreg = "linear", file_name = "Mbin_Ycont_int", event = NULL, casecontrol = FALSE) {
  cm <- causmed$new(data = Mbin_int_data,
                    outcome = outcome,
                    treatment = 'A',
                    mediator = 'M_bin',
                    covariates = "C",
                    vec = -2,
                    interaction = TRUE,
                    event = event,
                    casecontrol = casecontrol,
                    yreg = yreg, mreg = "logistic",
                    boot = TRUE, nboot = 500)
  
  files <- paste0(file_name, c("_delta.txt", "_boot.txt"))
  
  cm$delta_marginal()
  cm$delta_conditional()
  sink(files[1])
  cm$print_output(type = "full")
  sink()
  
  cm$bootstrap_marginal()
  cm$bootstrap_conditional()
  cm$print_output(type = "full")
  sink(files[2])
  cm$print_output(type = "full")
  sink()
}

f(outcome = "Y_cont_int", yreg = "linear", file_name = "Mbin_Ycont_int")
f(outcome = "Y_bin_int", yreg = "loglinear", file_name = "Mbin_Yloglin_int")
f(outcome = "Y_bin_int", yreg = "logistic", file_name = "Mbin_Ybin_int")
# f(outcome = "Y_bin_int", yreg = "logistic", file_name = "Mbin_Ybincc_int", casecontrol = TRUE) # TODO: generate Weibull simulations
f(outcome = "Y_count_int", yreg = "quasipoisson", file_name = "Mbin_Yqpoi_int")
f(outcome = "Y_count_int", yreg = "poisson", file_name = "Mbin_Ypoi_int")
f(outcome = "Y_count_int", yreg = "negbin", file_name = "Mbin_Ynegbin_int")

Mbin_int_data$event <- 1 - Mbin_int_data$delta

f(outcome = "Ycen_int", yreg = "coxph", file_name = "Mbin_Ycox_int", event = "event")
f(outcome = "Ycen_int", yreg = "aft_exp", file_name = "Mbin_Yexp_int", event = "event")
# f(outcome = "Ycen_int", yreg = "aft_weibull", file_name = "Mbin_Ywei_int", event = "event") # TODO: generate Weibull simulations
