% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{My_stratified_samplingC}
\alias{My_stratified_samplingC}
\title{A Stratified sampling function using Rcpp}
\arguments{
\item{data}{The data that contains the id, category, and value columns}

\item{samplesPerCategory}{The number of samples for each category}
}
\value{
A list of the sampling results including id and category \code{n}
}
\description{
Samples are drawn according to the idea of stratified sampling using Rcpp
}
\examples{
\dontrun{
    data <- data.frame(id = 1:9, category = rep(c("A", "B", "C"), each = 3), value = rnorm(9))
    samples_per_category <- list(A = 1, B = 2, C = 1)
    sampled_data <- My_stratified_samplingC(data, samples_per_category)
}
}
