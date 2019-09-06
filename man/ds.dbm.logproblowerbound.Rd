% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{ds.dbm.logproblowerbound}
\alias{ds.dbm.logproblowerbound}
\title{Estimation of Variational lower bound of log probability for a DBM model}
\usage{
ds.dbm.logproblowerbound(datasources, dbm = "dbm", data = "D",
  parallelized = NULL, ntemperatures = NULL, nparticles = NULL,
  burnin = NULL)
}
\arguments{
\item{datasources}{A list of Opal object(s) as a handle to the server-side session}

\item{data}{The name of the variable that holds the data on the server-side.
Defaults to \code{"D"}.}

\item{ntemperatures}{Number of temperatures for annealing from the starting model
to the target model, defaults to 100}

\item{nparticles}{Number of parallel chains and calculated weights in AIS, defaults to 100}

\item{burnin}{Number of steps to sample for the Gibbs transition between models in AIS}

\item{rbm}{The name of the DBM model on the server-side. Defaults to \code{"dbm"}.}
}
\description{
Estimates the variational lower bound of the likelihood of a DBM using
annealed importance sampling (AIS).
}