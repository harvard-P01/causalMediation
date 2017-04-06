set.seed(1234)

f <- function(outcome = "Y_cont_int", yreg = "linear",
              file_name = "Ycont", folder = "Mbin_int",
              event = NULL, casecontrol = FALSE, data, mediation = TRUE) {
  cm <- causmed$new(data = data,
                    outcome = outcome,
                    treatment = 'A',
                    mediator = 'M_bin',
                    covariates = "C",
                    vec = 1,
                    m = 0,
                    interaction = TRUE,
                    event = event,
                    casecontrol = casecontrol,
                    yreg = yreg, mreg = "logistic",
                    boot = TRUE, nboot = 100)
  
  files <- paste0(paste0(folder, "/", file_name, c("_delta.txt", "_boot.txt", "_mediate.txt")))
  
  cm$delta_marginal()
  cm$delta_conditional()
  # cm$print_output(type = "full")
  sink(files[1])
  cm$print_output(type = "full")
  sink()
  
  cm$bootstrap_marginal()
  cm$bootstrap_conditional()
  # cm$print_output(type = "full")
  sink(files[2])
  cm$print_output(type = "full")
  sink()
  
  if (mediation) {
    cm$mediation()
    sink(files[3])
    print(cm$mediation())
    sink()
  }
  
  return(cm)
}

# data(Mbin_int_data)

# Mbin_int_data <- read.csv("../../inst/data/Mbin_int_data_10000.csv")
d <- read.csv("../../inst/data/Mbin_int_data_10000.csv")
o <- f(outcome = "Y_cont_int", yreg = "linear", file_name = "Ycont", folder = "Mbin_int", data = d)
o <- f(outcome = "Y_bin_int", yreg = "loglinear", file_name = "Yloglin", folder = "Mbin_int", data = d)
o <- f(outcome = "Y_bin_int", yreg = "logistic", file_name = "Ybin", folder = "Mbin_int", data = d)
o <- f(outcome = "Y_count_int", yreg = "quasipoisson", file_name = "Yqpoi", folder = "Mbin_int", data = d)
o <- f(outcome = "Y_count_int", yreg = "poisson", file_name = "Ypoi", folder = "Mbin_int", data = d)
o <- f(outcome = "Y_count_int", yreg = "negbin", file_name = "Ynegbin", folder = "Mbin_int", data = d)

Mbin_int_data$event <- 1 - Mbin_int_data$delta
o <- f(outcome = "Ycen_int", yreg = "coxph", file_name = "Ycox", event = "event", folder = "Mbin_int", data = Mbin_int_data, mediation = FALSE)
o <- f(outcome = "Ycen_int", yreg = "aft_exp", file_name = "Yexp", event = "event", folder = "Mbin_int", data = Mbin_int_data, mediation = FALSE)

# data(Mbin_wei_int_data)
Mbin_wei_int_data <- read.csv("../../inst/data/Mbin_wei_int_data.csv")
o <- Mbin_wei_int_data$event <- 1 - Mbin_wei_int_data$delta
f(outcome = "Ycen_int", yreg = "aft_weibull", file_name = "Ywei", event = "event", folder = "Mbin_int", data = Mbin_wei_int_data, mediation = FALSE)

# data(Mbin_cc_int_data)
Mbin_cc_int_data <- read.csv("../../inst/data/Mbin_cc_int_data.csv")
names(Mbin_cc_int_data)[4] <- "M_bin"
o <- f(outcome = "Y_bincc_int", yreg = "logistic", file_name = "Ybincc", casecontrol = TRUE, folder = "Mbin_int", data = Mbin_cc_int_data, mediation = FALSE)

