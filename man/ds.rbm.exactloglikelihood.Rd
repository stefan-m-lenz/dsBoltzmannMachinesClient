% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{ds.rbm.exactloglikelihood}
\alias{ds.rbm.exactloglikelihood}
\title{Exact calculation of the log-likelihood of an RBM}
\usage{
ds.rbm.exactloglikelihood(datasources, rbm = "rbm", data = "D")
}
\arguments{
\item{rbm}{The name of the model to sample from on the server-side. Defaults to \code{"rbm"}.}
}
\description{
Same as \code{\link{ds.bm.exactloglikelihood}}, only with parameter \code{bm} changed to \code{rbm}.
}
