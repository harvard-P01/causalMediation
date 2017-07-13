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
                  nboot = 0,
                  mreg = "logistic",
                  yreg = "linear",
                  event = NULL,
                  m = 0,
                  a_star = 0,
                  a = 1,
                  casecontrol = FALSE,
                  baseline = NULL)


files <- paste0(paste0(folder, "/", file_name, c("_delta.txt", "_boot.txt", "_mediate.txt")))

cm$delta_marginal()
cm$delta_conditional()
cm$print_output(type = "full")
sink(files[1])
cm$print_output(type = "full")
sink()

summary(cm$mediator_regression)
summary(cm$outcome_regression)

cm$bootstrap_marginal()
cm$bootstrap_conditional()
cm$print_output(type = "full")
sink(files[2])
cm$print_output(type = "full")
sink()

if (mediation) {
  cm$mediation()
  sink(files[3])
  print(cm$mediation())
  sink()
}