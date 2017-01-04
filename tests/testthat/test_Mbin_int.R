# data(Mbin_int_data_10000)
data(Mbin_int_data)

set.seed(1234)

cm <- causmed$new(data = Mbin_int_data,
                  outcome = "Y_cont_int",
                  treatment = 'A',
                  mediator = 'M_bin',
                  covariates = "C",
                  vec = -2,
                  interaction = TRUE,
                  yreg = "linear", mreg = "logistic",
                  boot = TRUE, nboot = 500)

cm$bootstrap_marginal()
cm$delta_marginal()

write.csv(format_df_boot(cm$boot_out_marginal), "Mbin_int_data_boot.csv") 
write.csv(format_df_delta(cm$delta_out_marginal), "Mbin_int_data_delta.csv") 
