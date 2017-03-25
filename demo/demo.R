data(Mbin_int_data)

# Mbin_int_data <- read.csv("../../inst/data/Mbin_int_data.csv")
set.seed(1234)
Mbin_int_data$contexp <- rnorm(1000, 0, 10) 

cm <- causmed$new(data = Mbin_int_data,
                  outcome = "Y_cont_int",
                  treatment = 'A',
                  mediator = 'M_bin',
                  covariates = "C",
                  vecc = 100,
                  interaction = TRUE,
                  yreg = "linear", mreg = "logistic",
                  boot = FALSE, nboot = 50, event = NULL, a_star = 0, a = 1, m = 3)

cm$delta_marginal()
cm$print_delta(type = "marginal")

cm$delta_conditional()
cm$print_delta(type = "conditional")

cm$create_formulas()
cm$run_regressions()
cm$mediation()

cm$medflex_weight_categorical()
cm$medflex_weight_continuous()
cm$medflex_imputation_categorical()
cm$medflex_imputation_continuous()

cm <- causmed$new(data = Mbin_int_data,
                  outcome = "Y_cont_int",
                  treatment = 'contexp',
                  mediator = 'M_bin',
                  covariates = "C",
                  vecc = 1,
                  interaction = TRUE,
                  yreg = "linear", mreg = "logistic",
                  boot = FALSE, nboot = 50, event = NULL, a_star = 0, a = 1, m = 0)


# cm$print_output()
# cm$print_delta(digits = 4, type = "marginal")
# cm$print_delta()
# cm$print_delta(digits = 3, conf = .90, type = "marginal")
# 
# cm$print_output(type = "reduced")
# cm$print_output(type = "full")
# 
# cm$bootstrap_marginal()
# cm$boot_out_marginal
# cm$print_boot(type = "marginal")
# 
# cm$bootstrap_conditional()
# cm$boot_out_conditional
# cm$print_boot(type = "conditional")
# 
# cm$print_output(type = "reduced")
# cm$print_output(type = "full")
# 
# cm$delta_marginal()
# cm$delta_out_marginal
# 
# cm$delta_conditional()
# cm$delta_out_conditional
# 
# 
# cm$bootstrap()
# cm$print_boot()
# cm$print_output()
# cm$print_boot(digits = 3)
# cm$print_boot(digits = 3, conf = 0.90)
# 
# cm$mediation()
