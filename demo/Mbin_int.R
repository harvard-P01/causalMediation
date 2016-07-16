
# df <- read.csv("data/Mbin_int_data.txt", sep = " ")
#df <- read.csv("data/Mbin_int_data_10000.txt", sep = " ")

df <- read.csv("Mbin_int_data_10000.txt", sep = " ")

##----- Bootstrap - Y_cont_int

set.seed(1234)
s_boot_Y_cont_int_M_bin <- causalMediation(data = df,
                                            outcome = "Y_cont_int",
                                            treatment = 'A',
                                            mediator = 'M_bin',
                                            covariates = "C",
                                            interaction = TRUE,
                                            yreg = "linear", mreg = "logistic",
                                            boot = TRUE, nboot = 500)
s_boot_Y_cont_int_M_bin
sink(file = "s_boot_Y_cont_int_M_bin.txt")
s_boot_Y_cont_int_M_bin
sink()

##----- Delta - Y_cont_int

s_delta_Y_cont_int_M_bin <- causalMediation(data = df,
                                             outcome = "Y_cont_int",
                                             treatment = 'A',
                                             mediator = 'M_bin',
                                             covariates = "C",
                                             interaction = TRUE,
                                             yreg = "linear", mreg = "logistic",
                                             boot = FALSE)
s_delta_Y_cont_int_M_bin
sink(file = "s_delta_Y_cont_int_M_bin.txt")
s_delta_Y_cont_int_M_bin
sink()

##----- Bootstrap - Y_bin_int

set.seed(1234)
s_boot_Y_bin_int_M_bin <- causalMediation(data = df,
                                           outcome = "Y_bin_int",
                                           treatment = 'A',
                                           mediator = 'M_bin',
                                           covariates = "C",
                                           interaction = TRUE,
                                           yreg = "logistic", mreg = "logistic",
                                           boot = TRUE, nboot = 500)
s_boot_Y_bin_int_M_bin
sink(file = "s_boot_Y_bin_int_M_bin.txt")
s_boot_Y_bin_int_M_bin
sink()

##----- Delta - Y_bin_int

s_delta_Y_bin_int_M_bin <- causalMediation(data = df,
                                            outcome = "Y_bin_int",
                                            treatment = 'A',
                                            mediator = 'M_bin',
                                            covariates = "C",
                                            interaction = TRUE,
                                            yreg = "logistic", mreg = "logistic",
                                            boot = FALSE)
s_delta_Y_bin_int_M_bin
sink(file = "s_delta_Y_bin_int_M_bin.txt")
s_delta_Y_bin_int_M_bin
sink()
