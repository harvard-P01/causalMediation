library(causalMediation)
### first example
df <- read.table(file='~/Downloads/Linda.csv', header=TRUE, sep = ';')

# result <- causalMediationOneStep(df, interaction = TRUE, yreg = "binary", mreg = "binary",
#                                  outcome = "satis", mediator = "attrib", treatment = "therapy")

df <- read.table(file='~/Downloads/Linda.csv', header=TRUE, sep = ';')
head(df)
df$cov <- sample(df$Yint) + runif(1)

# debug(causalMediationOneStep)
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

fit <- lm(satis ~ therapy + attrib, data = df)
summary(fit)

result_delta <- causalMediationDelta(data = df, 
                                     outcome = 'satis', 
                                     treatment = 'therapy',
                                     mediator = 'attrib',
                                     covariates = 'cov',
                                     cval = 5,
                                     interaction = TRUE,
                                     yreg = "linear", mreg = "linear")
result_delta


result_delta_bin <- causalMediationDelta(data = df, 
                                     outcome = 'death', 
                                     treatment = 'smoking',
                                     mediator = 'lbw',
                                     covariates = 'drinking',
                                     cval = 1,
                                     interaction = FALSE,
                                     yreg = "logistic", mreg = "logistic")
result_delta_bin


r <- causalMediation(data = df, 
                     outcome = 'satis', 
                     treatment = 'therapy',
                     mediator = 'attrib',
                     covariates = NULL,
                     interaction = FALSE,
                     yreg = "linear", mreg = "linear",
                     boot = FALSE)
                     # nboot = 10)
r

df <- read.table("data/simdatasurv.csv", header = TRUE)

boot(data= df, statistic = causalMediationOneStep, R = 10,
     outcome = 'y', 
     treatment = 'x',
     mediator = 'm',
     covariates = 'c',
     interaction = FALSE,
     yreg = "linear", mreg = "linear")

r <- causalMediation(data = df, outcome = 'y', 
                     treatment = 'x',
                     mediator = 'm',
                     covariates = 'c',
                     interaction = FALSE,
                     yreg = "linear", mreg = "linear",
                     boot = TRUE,
                     nboot = 10)

print(r)

