library(causalMediation)
### first example
df <- read.table(file='~/Downloads/Linda.csv', header=TRUE, sep = ';')

set.seed(123)

r <- boot(data = df, statistic = causalMediationAll, R = 100,
          outcome = 'satis', 
          treatment = 'therapy',
          mediator = 'attrib',
          covariates = NULL,
          interaction = FALSE,
          yreg = "linear", mreg = "linear",
          boot = TRUE)

r

format_df_boot(r)

set.seed(123)

s <- causalMediation(data = df,
                outcome = "satis",
                treatment = 'therapy',
                mediator = 'attrib',
                covariates = NULL,
                interaction = FALSE,
                yreg = "linear", mreg = "linear",
                boot = TRUE, nboot = 50)

s
l

d <- causalMediation(data = df,
                     outcome = "satis",
                     treatment = 'therapy',
                     mediator = 'attrib',
                     covariates = NULL,
                     interaction = FALSE,
                     yreg = "linear", mreg = "linear",
                     boot = FALSE)

d

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
                                     vecc = 5,
                                     interaction = TRUE,
                                     yreg = "linear", mreg = "linear")
result_delta


r <- causalMediation(data = df, 
                     outcome = 'satis', 
                     treatment = 'therapy',
                     mediator = 'attrib',
                     covariates = NULL,
                     interaction = FALSE,
                     yreg = "linear", mreg = "linear",
                     boot = TRUE)
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

