\name{df}
\alias{df}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Birthweight data.
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Sample dataset with information on birth certificate-infant mortality from the National Center for Health Statistics.
}
\usage{data("df")}
\format{
  A data frame with 1000000 observations on the following 12 variables.
  \describe{
    \item{\code{mar}}{a numeric vector}
    \item{\code{lbw}}{a numeric vector}
    \item{\code{death}}{a numeric vector}
    \item{\code{smoking}}{a numeric vector}
    \item{\code{drinking}}{a numeric vector}
    \item{\code{hispanic}}{a numeric vector}
    \item{\code{black}}{a numeric vector}
    \item{\code{nativeamerican}}{a numeric vector}
    \item{\code{asian}}{a numeric vector}
    \item{\code{agebelow20}}{a numeric vector}
    \item{\code{ageabove35}}{a numeric vector}
    \item{\code{somecollege}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
The data are 2003 U.S. linked birth certificate-infant mortality files from the National Center for Health Statistics. To reduce data analysis time, a random sample of 1,000,000 records were selected.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
### Show top 5 rows for dataframe
head(df)
}
\keyword{datasets}
