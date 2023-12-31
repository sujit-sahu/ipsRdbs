% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datafiles.R
\docType{data}
\name{beanie}
\alias{beanie}
\title{Age and value of 50 beanie baby toys}
\format{
A data frame with 50 rows and 3 columns:
\describe{
  \item{name}{Name of the toy}
  \item{age}{Age of the toy in months}
  \item{value}{Market value of the toy in US dollars}
}
}
\source{
Beanie world magazine
}
\usage{
beanie
}
\description{
Age and value of 50 beanie baby toys
}
\examples{
 head(beanie)
 summary(beanie)
 plot(beanie$age, beanie$value, xlab="Age", ylab="Value", pch="*", col="red")
}
\keyword{datasets}
