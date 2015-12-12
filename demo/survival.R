df <- read.table("data/simdatasurv.csv", header = TRUE)

result <- causalMediationOneStep(data = df, 
                          outcome = 'y', 
                          treatment = 'x',
                          mediator = 'm',
                          covariates = 'c',
                          # nbootstraps = 100, 
                          interaction = FALSE,
                          yreg = 'linear',
                          mreg = 'linear')
result

result2 <- causalMediationOneStep(data = df, 
                           outcome = 'y', 
                           treatment = 'x',
                           mediator = 'm',
                           covariates = 'c',
                           # nbootstraps = 100, 
                           interaction = FALSE,
                           event = 'cens',
                           yreg = 'coxph',
                           mreg = 'linear')
result2

debug(causalMediationOneStep)
