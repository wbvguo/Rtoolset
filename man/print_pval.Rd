% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print_pval}
\alias{print_pval}
\title{print p-values in a more readable format}
\usage{
print_pval(pvalues, threshold = 1e-04, rm_tailing0 = TRUE)
}
\arguments{
\item{pvalues}{A numeric vector of p-values to be formatted.}

\item{threshold}{The threshold below which p-values are printed in scientific notation. Default is 1e-4.}

\item{rm_tailing0}{Logical. If TRUE, trailing zeros in the decimal representation are removed. Default is TRUE.}
}
\value{
A character vector of formatted p-values.
}
\description{
Formats a numeric vector of p-values into a more readable string. P-values below a
threshold are displayed in scientific notation. Optionally, trailing zeros in
the decimal representation can be removed.
}
\examples{
print_pval(c(0.0101, 0.00003, 0.00000001))
#[1] "0.0101"   "3.00e-05" "1.00e-08"
print_pval(c(0.0101, 0.00003, 0.00000001), threshold = 1e-3, rm_tailing0 = FALSE)
#[1] "0.0101"   "3.00e-05" "1.00e-08"
}
