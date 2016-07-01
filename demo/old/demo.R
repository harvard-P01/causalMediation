rm(list=ls())

### install package
# install.packages('C:/Users/659014/Downloads/causalMediation_1.0.tar.gz', repos=NULL, type='source')

### load package
# library(causalMediation, lib.loc='C:/Users/659014/Documents/R/win-library/3.1')
library(causalMediation)
### first example
df <- data.frame('smoking'    = c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
                 'lbw'        = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'death'      = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'drinking'   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
                 'agebelow20' = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

result <- causalMediation::causalMediation(nbootstraps = 10, interaction = TRUE, yreg = "binary", mreg = "binary")
print(result)

# result <- causalMediation::causalMediation(nbootstraps = 1, interaction = TRUE)

### second example
df     <- read.table(file='~/Downloads/Linda.csv', header=TRUE, sep = ';')
head(df)


result <- causalMediation(data = df, 
                          outcome = 'satis', 
                          treatment = 'therapy',
                          mediator = 'attrib',
                          covariates = c(),
                          nbootstraps = 0, 
                          interaction = FALSE)
print(result)


### third example
df     <- read.table(file='~/Downloads/Linda-duplicated.csv', header=TRUE, sep = ';')
result <- causalMediation::causalMediation(data = df, 
                                           outcome = 'satis', 
                                           treatment = 'therapy',
                                           mediator = 'attrib',
                                           covariates = c(),
                                           nbootstraps = 1000, 
                                           interaction = FALSE)
print(result)

