% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_regression_6.R
\name{res_plot}
\alias{res_plot}
\title{Residuals vs fitted-values}
\usage{
res_plot(res, preds)
}
\arguments{
\item{res}{A \code{vector} of length \code{n} that represents the residuals.}

\item{preds}{A \code{vector} of length \code{n} that represents the predictions.}
}
\description{
Creates a plot for the residuals and fitted values.
}
\examples{
res_plot(model$res, model$preds)
}
\author{
Clayton Haley

Ukamaka Victoria Nnyaba

Robert Fett
}
