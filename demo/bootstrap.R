library(causalMediation)
### first example
df <- read.table(file='~/Downloads/Linda.csv', header=TRUE, sep = ';')

result <- causalMediationOneStep(df, interaction = TRUE, debug = TRUE, yreg = "binary", mreg = "binary",
                                 outcome = "satis", mediator = "attrib", treatment = "therapy")

df     <- read.table(file='~/Downloads/Linda.csv', header=TRUE, sep = ';')
head(df)

debug(causalMediationOneStep)
result <- causalMediationOneStep(data = df, 
                          outcome = 'satis', 
                          treatment = 'therapy',
                          mediator = 'attrib',
                          covariates = NULL,
                          interaction = FALSE,
                          yreg = "linear", mreg = "linear")
result
is.vector(result)
library(boot)

boot(data= df, statistic = causalMediationOneStep, R = 10,
     outcome = 'satis', 
     treatment = 'therapy',
     mediator = 'attrib',
     covariates = NULL,
     interaction = FALSE,
     yreg = "linear", mreg = "linear")

