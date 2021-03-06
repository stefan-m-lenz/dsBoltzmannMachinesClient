% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{ds.dbm.samples}
\alias{ds.dbm.samples}
\title{Generate samples from a deep Boltzmann machine}
\usage{
ds.dbm.samples(datasources, dbm = "dbm", ...)
}
\arguments{
\item{dbm}{The name of the model to sample from on the server-side. Defaults to \code{"dbm"}.}
}
\description{
Same as \code{\link{ds.bm.samples}}, only with parameter \code{bm} changed to \code{dbm}.
}
