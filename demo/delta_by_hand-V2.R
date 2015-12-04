df <- data.frame('smoking'    = c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
                 'lbw'        = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'death'      = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'drinking'   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
                 'agebelow20' = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

df <- read.csv("~/Downloads/Linda-duplicated.csv", sep = ";")
names(df)
data = df
#outcome = 'satis'
#treatment = 'therapy'
#mediator = 'attrib'

outcome = 'death'
treatment = 'smoking'
mediator = 'lbw'

covariates = c('drinking', 'agebelow20')
interaction = FALSE
vecc <- colMeans(cbind(df$drinking, df$agebelow20 + 0.1))

mediator.basic <- paste(mediator, treatment, sep=' ~ ')
outcome.basic  <- paste(paste(outcome, treatment, sep=' ~ '), mediator, sep = ' + ')

if (interaction == TRUE) {
  outcome.basic <- paste(outcome.basic, paste(treatment, mediator, sep = '*'), sep = ' + ')
}

if (length(covariates) == 0) {
  mediator.formula <- mediator.basic
  outcome.formula  <- outcome.basic
} else {
  mediator.formula <- paste(mediator.basic, paste(covariates, collapse = " + "), sep = ' + ')
  outcome.formula  <- paste(outcome.basic,  paste(covariates, collapse = " + "), sep = ' + ')
}


### FIXME: hardcode to validate with SAS macro
mediator.binary <- all(unique(data[, mediator]) %in% 0:1)
outcome.binary <- all(unique(data[, outcome])  %in% 0:1)

##----- Delta method
if (! mediator.binary) {
  mediator.regression <<- lm(mediator.formula, data = data)
}else{
  mediator.regression <<- glm(mediator.formula, family = binomial(), data = data)
}

if(!outcome.binary) {
  outcome.regression  <<- lm(outcome.formula, data = data)
}else{
  outcome.regression  <<- glm(outcome.formula, family = binomial(), data = data)
}

## Store coefficients from regression
betas  <- coefficients(mediator.regression)
thetas <- coefficients(outcome.regression)

## Store covariances from regression
vcov_betas <- vcov(mediator.regression)
vcov_thetas <- vcov(outcome.regression)

## Build block diagonal matrix
vcov_block <- bdiag(vcov_betas, vcov_thetas)

cde <- CDE_bin(thetas, treatment, mediator)
s <- CDE_cont_delta(thetas, treatment, mediator, interaction = FALSE)
s
deltamethod(s, thetas, vcov_thetas)

s_test <- CDE_cont_delta_test(thetas, treatment, mediator, interaction = FALSE)
s_test
deltamethod(s_test, c(thetas, betas), vcov_block)

s_test2 <- NIE_contcont_delta(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE)
deltamethod(s_test2, c(thetas, betas), vcov_block)

nde <- NDE_binbin(betas, thetas, treatment, mediator, covariates)
nie <- NIE_binbin(betas, thetas, treatment, mediator, covariates)

s_test3 <- NIE_binbin_delta(betas, thetas, vecc, treatment, mediator, a_star = 0, a = 1, interaction = FALSE)
deltamethod(s_test3, c(thetas, betas), vcov_block)


tde <- nde*nie

attach(data)
m <- 1
a <- 1
a_star <- 0


length(thetas)
paste("x")

exp()

g <- exp((thetas[2]+thetas[length(thetas)]*m)*(a-a_star))

deltamethod(~ exp((x2+x6*m)*(a-a_star)), thetas, vcov_thetas)
s <- CDE_bin_delta(thetas, "smoking", "lbw", interaction = FALSE)
s



syms <- paste("x", 1:n, sep = "")

deltamethod (~ 3, thetas, vcov_thetas) 
deltamethod (~ 1 / (x1 + x2), estmean, estvar) 

# attr(mediator.regression, "terms")

## Simple linear regression, E(y) = alpha + beta x
set.seed(02138)
x <- 1:100
y <- rnorm(100, 4*x, 5)
toy.lm <- lm(y ~ x)
estmean <- coef(toy.lm)
estvar <- summary(toy.lm)$cov.unscaled * summary(toy.lm)$sigma^2

## Estimate of (1 / (alphahat + betahat))
1 / (estmean[1] + estmean[2])
## Approximate standard error
deltamethod (~ 1 / (x1 + x2), estmean, estvar) 

# Simple linear regression, E(y) = alpha + beta x 
set.seed(02138)
a <- 1:100
b <- rnorm(100, 4*x, 5)
toy.lm <- lm(b ~ a)
estmean <- coef(toy.lm)
estvar <- summary(toy.lm)$cov.unscaled * summary(toy.lm)$sigma^2

## Estimate of (1 / (alphahat + betahat))
1 / (estmean[1] + estmean[2])
## Approximate standard error
deltamethod (~ 1 / (x1 + x2), estmean, estvar) 
