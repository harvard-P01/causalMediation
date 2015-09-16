\name{causalMediation}
\alias{causalMediation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Causal Mediation Analysis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Implementation of Causal Mediation Analysis as published by Valeri and VanderWeele (2013, doi: 10.1037/a0031034).
}
\usage{
causalMediation(data = df, outcome = "death", treatment = "smoking", 
				mediator = "lbw", covariates = c("drinking", "agebelow20"), 
				interaction = FALSE, nbootstraps = 0, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
The dataframe used for the analysis.
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
The column name in the dataframe which contains the outcome values.
}
  \item{treatment}{
%%     ~~Describe \code{treatment} here~~
The column name in the dataframe which contains the treatment values.
}
  \item{mediator}{
%%     ~~Describe \code{mediator} here~~
The column name in the dataframe which contains the mediator values.
}
  \item{covariates}{
%%     ~~Describe \code{covariates} here~~
The column names in the dataframe which contain the covariate values.
}
  \item{interaction}{
%%     ~~Describe \code{interaction} here~~
Boolean whether or not to include treatment-mediator interaction.
}
  \item{nbootstraps}{
%%     ~~Describe \code{nbootstraps} here~~
Integer number of bootstraps to perform to evaluate Standard Error (SE).
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
Boolean whether or not to display some intermediate output to screen.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
Valeri and VanderWeele (2013, doi: 10.1037/a0031034).
}
\author{
%%  ~~who you are~~
Egge van der Poel
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
df <- data.frame('smoking'    = c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
                 'lbw'        = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'death'      = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                 'drinking'   = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
                 'agebelow20' = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
                )
result <- causalMediation(nbootstraps = 0, interaction = TRUE, debug = TRUE)
print(result)

## The function is currently defined as
function (data = df, outcome = "death", treatment = "smoking", 
    mediator = "lbw", covariates = c("drinking", "agebelow20"), 
    interaction = FALSE, nbootstraps = 0, debug = FALSE) 
{
    mediator.basic <- paste(mediator, treatment, sep = " ~ ")
    outcome.basic <- paste(paste(outcome, treatment, sep = " ~ "), 
        mediator, sep = " + ")
    if (interaction == TRUE) {
        outcome.basic <- paste(outcome.basic, paste(treatment, 
            mediator, sep = "*"), sep = " + ")
    }
    if (length(covariates) == 0) {
        mediator.formula <- mediator.basic
        outcome.formula <- outcome.basic
    }
    else {
        mediator.formula <- paste(mediator.basic, paste(covariates, 
            collapse = " + "), sep = " + ")
        outcome.formula <- paste(outcome.basic, paste(covariates, 
            collapse = " + "), sep = " + ")
    }
    if (debug) {
        print(paste("MEDIATION FORMULA", mediator.formula, sep = " : "))
        print(paste("  OUTCOME FORMULA", outcome.formula, sep = " : "))
    }
    mediator.binary = all(unique(data[, mediator]) \%in\% 0:1)
    outcome.binary = all(unique(data[, outcome]) \%in\% 0:1)
    if (debug) {
        print(paste("MEDIATOR BINARY = ", mediator.binary, sep = ""))
        print(paste(" OUTCOME BINARY = ", outcome.binary, sep = ""))
    }
    cdes <- vector()
    ndes <- vector()
    nies <- vector()
    tdes <- vector()
    betasVec <- vector()
    thetasVec <- vector()
    for (i in seq(1:nbootstraps + 1)) {
        if (i\%\%100 == 0) {
            print(paste("Running bootstrap sample", i, "out of", 
                nbootstraps, sep = " "))
        }
        if (i == 1) {
            data <- df
            debug <- TRUE
        }
        else {
            data <- df[sample(nrow(df), replace = TRUE), ]
            debug <- FALSE
        }
        if (!mediator.binary) {
            mediator.regression <<- lm(mediator.formula, data = data)
        }
        else {
            mediator.regression <<- glm(mediator.formula, family = binomial(), 
                data = data)
        }
        if (!outcome.binary) {
            outcome.regression <<- lm(outcome.formula, data = data)
        }
        else {
            outcome.regression <<- glm(outcome.formula, family = binomial(), 
                data = data)
        }
        betas <- coefficients(mediator.regression)
        thetas <- coefficients(outcome.regression)
        if (debug) {
            print("BETAS:")
            print(coefficients(mediator.regression))
            print("THETAS:")
            print(coefficients(outcome.regression))
        }
        if (mediator.binary | outcome.binary) {
            cde <- CDE_bin(thetas, treatment, mediator, outcome, 
                covariates)
        }
        variance <- (summary(mediator.regression)$sigma)^2
        if (mediator.binary & outcome.binary) {
            cde <- CDE_bin(thetas, treatment, mediator)
            nde <- NDE_binbin(betas, thetas, treatment, mediator, 
                covariates)
            nie <- NIE_binbin(betas, thetas, treatment, mediator, 
                covariates)
            tde <- nde * nie
        }
        else if (mediator.binary & !outcome.binary) {
            cde <- CDE_cont(thetas, treatment, mediator)
            nde <- NDE_bincont(betas, thetas, treatment, mediator, 
                covariates)
            nie <- NIE_bincont(betas, thetas, treatment, mediator, 
                covariates)
            tde <- nde + nie
        }
        else if (!mediator.binary & outcome.binary) {
            cde <- CDE_bin(thetas, treatment, mediator)
            nde <- NDE_contbin(betas, thetas, treatment, mediator, 
                covariates, variance)
            nie <- NIE_contbin(betas, thetas, treatment, mediator, 
                covariates)
            tde <- nde * nie
        }
        else if (!mediator.binary & !outcome.binary) {
            cde <- CDE_cont(thetas, treatment, mediator)
            nde <- NDE_contcont(betas, thetas, treatment, mediator, 
                covariates)
            nie <- NIE_contcont(betas, thetas, treatment, mediator, 
                covariates)
            tde <- nde + nie
        }
        cdes <- c(cdes, cde)
        ndes <- c(ndes, nde)
        nies <- c(nies, nie)
        tdes <- c(tdes, tde)
        betasVec <- rbind(betasVec, betas)
        thetasVec <- rbind(thetasVec, thetas)
    }
    cde.mean <- cdes[1]
    cdes <- sort(cdes[2:length(cdes)])
    cde.quant <- quantile(cdes, c(0.025, 0.975))
    cde.se <- cde.quant[2] - cde.quant[1]
    nde.mean <- ndes[1]
    ndes <- sort(ndes[2:length(ndes)])
    nde.quant <- quantile(ndes, c(0.025, 0.975))
    nde.se <- nde.quant[2] - nde.quant[1]
    nie.mean <- nies[1]
    nies <- sort(nies[2:length(nies)])
    nie.quant <- quantile(nies, c(0.025, 0.975))
    nie.se <- nie.quant[2] - nie.quant[1]
    tde.mean <- tdes[1]
    tdes <- sort(tdes[2:length(tdes)])
    tde.quant <- quantile(tdes, c(0.025, 0.975))
    tde.se <- tde.quant[2] - tde.quant[1]
    t <- matrix(data = c(cde.mean, nde.mean, nie.mean, tde.mean, 
        cde.se, nde.se, nie.se, tde.se, cde.mean - 2 * cde.se, 
        nde.mean - 2 * nde.se, nie.mean - 2 * nie.se, tde.mean - 
            2 * tde.se, cde.mean + 2 * cde.se, nde.mean + 2 * 
            nde.se, nie.mean + 2 * nie.se, tde.mean + 2 * tde.se), 
        ncol = 4)
    rownames(t) <- c("CDE", "NDE", "NIE", "TE")
    colnames(t) <- c("estimate", "SE", "CI 95\% lower", "CI 95\% upper")
    dt <- as.data.frame.matrix(t)
    dt
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Causal }
\keyword{ Mediation }% __ONLY ONE__ keyword per line
