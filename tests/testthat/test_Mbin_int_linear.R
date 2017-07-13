d <- read.csv("../../inst/data/Mbin_int_data.csv")
names(d)

cm <- causmed$new(data = d,
                  outcome = "Y_cont_int",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  vecc = 1,
                  interaction = interaction,
                  boot = boot,
                  nboot = nboot,
                  mreg = mreg,
                  yreg = yreg,
                  event = event,
                  m = 0,
                  a_star = a_star,
                  a = a,
                  casecontrol = casecontrol,
                  baseline = baseline)

cm <- causmed$new(data = d,
                  outcome = "Y_cont_int",
                  treatment = "A",
                  mediator = "M_bin",
                  covariates = "C",
                  yreg = "linear",
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
cm$print_output(type = "full")
sink(files[1])
cm$print_output(type = "full")
sink()

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