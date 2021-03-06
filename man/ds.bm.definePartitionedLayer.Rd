% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{ds.bm.definePartitionedLayer}
\alias{ds.bm.definePartitionedLayer}
\title{Define parameters for training a partitioned RBM layer in a DBM or DBN}
\usage{
ds.bm.definePartitionedLayer(datasources, newobj, parts)
}
\arguments{
\item{datasources}{A list of Opal object(s) as a handle to the server-side session}

\item{newobj}{The name of the server-side object where the parameters are stored}

\item{parts}{A vector with names for \code{TrainLayer} objects which have been created
by \code{\link{ds.bm.defineLayer}} before.}
}
\description{
Creates an object at the server-side that encapsulates the parameters for training
a partitioned layer.
}
