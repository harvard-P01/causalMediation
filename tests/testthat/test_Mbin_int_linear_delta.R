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

filename <- "Mbin_int/linear_delta.txt"

cm$delta_marginal()
cm$delta_conditional()
cm$print_output(type = "full")

cm$summary_coef_conditional
r_marginal <- cm$summary_coef_marginal[1:6, c(1:4, 6)]

sas <- read.csv("../../inst/sasoutput/Mbin_int_linear_delta.csv")
sas <- sas[!is.na(sas$Obs), ]

sas_marginal <- sas[1:6, c(3, 4, 6, 7, 5)]

print("r_marginal")
r_marginal
print("sas_marginal")
sas_marginal
print("r_marginal - sas_marginal")
r_marginal - sas_marginal
print("sum(r_marginal - sas_marginal)")
sum(r_marginal - sas_marginal)

sink(filename)
print("r_marginal")
r_marginal
print("sas_marginal")
sas_marginal
print("r_marginal - sas_marginal")
r_marginal - sas_marginal
print("sum(r_marginal - sas_marginal)")
sum(r_marginal - sas_marginal)
sink()


