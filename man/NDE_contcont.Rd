\name{NDE_contcont}
\alias{NDE_contcont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Natural Direct Effect (NDE) function for cases when mediator and outcome are both continuous.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the Natural Direct Effect (NDE) for continuous mediator and continuous outcome as described in the publication of Valeri and VanderWeele (2013, doi: 10.1037/a0031034).
}
\usage{
NDE_contcont(betas, thetas, treatment, mediator, covariates, a_old = 1, a_new = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{betas}{
%%     ~~Describe \code{betas} here~~
The coefficients of the regression on the mediator variable.
}
  \item{thetas}{
%%     ~~Describe \code{thetas} here~~
The coefficients of the regression on the outcome variable.
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
  \item{a_old}{
%%     ~~Describe \code{a_old} here~~
The old value of the treatment at which to evaluate the result (a in the publication of Valeri and VanderWeele). Default = 1.
}
  \item{a_new}{
%%     ~~Describe \code{a_new} here~~
The new value of the treatment at which to evaluate the result (a* in the publication of Valeri and VanderWeele). Default = 0.
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
Egge van der Poel (e.vanderpoel@erasmusmc.nl).
}
\note{
%%  ~~further notes~~
This package is under continuous (but perhaps slow) development. Users are welcome to contribute to the development.
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

betas  <- c('(Intercept)' = -2.4916567, 'smoking' = 0.4905681, 'drinking' = -0.5967808, 
            'agebelow20' = 0.2305012)
thetas <- c('(Intercept)' = -6.5108483, 'smoking' = 0.5290551, 'lbw' = 3.5778582, 
            'drinking' = -0.3125368, 'agebelow20' = 0.2668849, 'smoking:lbw' = -0.5336013)
outcome='death'
treatment='smoking'
mediator='lbw'
covariates=c('drinking', 'agebelow20')

NDE_contcont(betas = betas, thetas = thetas, treatment = treatment, mediator = mediator, covariates = covariates)

## The function is currently defined as
function (betas, thetas, treatment, mediator, covariates, a_old = 1, 
    a_new = 0) 
{
    covariatesTerm <- 0
    for (c in covariates) {
        covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 
            2, mean, na.rm = TRUE)
    }
    interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, 
        sep = ":")]), 0, thetas[paste(treatment, mediator, sep = ":")])
    nde <- (thetas[treatment] + interactionTerm * betas[1] + 
        interactionTerm * betas[treatment] * a_new + interactionTerm * 
        covariatesTerm) * (a_old - a_new)
    unname(nde)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ NDE_contcont }
