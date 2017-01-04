# library(causalMediation)
# 
# # df <- read.csv("data/Mcont_noint_data.txt", sep = " ")
# df <- read.csv("data/Mcont_noint_data_10000.txt", sep = " ")
# 
# ##----- Bootstrap - Y_cont_noint
# 
# set.seed(1234)
# s_boot_Y_cont_noint_M_cont <- causalMediation(data = df,
#                                               outcome = "Y_cont_noint",
#                                               treatment = 'A',
#                                               mediator = 'M_cont',
#                                               covariates = "C",
#                                               interaction = FALSE,
#                                               yreg = "linear", mreg = "linear",
#                                               boot = TRUE, nboot = 500)
# s_boot_Y_cont_noint_M_cont
# sink(file = "inst/simulations/s_boot_Y_cont_noint_M_cont.txt")
# s_boot_Y_cont_noint_M_cont
# sink()
# 
# ##----- Delta - Y_cont_noint
# 
# s_delta_Y_cont_noint_M_cont <- causalMediation(data = df,
#                                                outcome = "Y_cont_noint",
#                                                treatment = 'A',
#                                                mediator = 'M_cont',
#                                                covariates = "C",
#                                                interaction = FALSE,
#                                                yreg = "linear", mreg = "linear",
#                                                boot = FALSE)
# s_delta_Y_cont_noint_M_cont
# sink(file = "inst/simulations/s_delta_Y_cont_noint_M_cont.txt")
# s_delta_Y_cont_noint_M_cont
# sink()
# 
# ##----- Bootstrap - Y_bin_noint
# 
# set.seed(1234)
# s_boot_Y_bin_noint_M_cont <- causalMediation(data = df,
#                                              outcome = "Y_bin_noint",
#                                              treatment = 'A',
#                                              mediator = 'M_cont',
#                                              covariates = "C",
#                                              interaction = FALSE,
#                                              yreg = "logistic", mreg = "linear",
#                                              boot = TRUE, nboot = 500)
# s_boot_Y_bin_noint_M_cont
# sink(file = "inst/simulations/s_boot_Y_bin_noint_M_cont.txt")
# s_boot_Y_bin_noint_M_cont
# sink()
# 
# ##----- Delta - Y_bin_noint
# 
# s_delta_Y_bin_noint_M_cont <- causalMediation(data = df,
#                                               outcome = "Y_bin_noint",
#                                               treatment = 'A',
#                                               mediator = 'M_cont',
#                                               covariates = "C",
#                                               interaction = FALSE,
#                                               yreg = "logistic", mreg = "linear",
#                                               boot = FALSE)
# s_delta_Y_bin_noint_M_cont
# sink(file = "inst/simulations/s_delta_Y_bin_noint_M_cont.txt")
# s_delta_Y_bin_noint_M_cont
# sink()
# 
