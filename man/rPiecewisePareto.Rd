% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{rPiecewisePareto}
\alias{rPiecewisePareto}
\title{Simulation of the Piecewise Pareto Distribution}
\usage{
rPiecewisePareto(
  n,
  t,
  alpha,
  truncation = NULL,
  truncation_type = "lp",
  scale_pieces = NULL
)
}
\arguments{
\item{n}{Numeric. Number of simulations}

\item{t}{Numeric vector. Thresholds of the piecewise Pareto distribution.}

\item{alpha}{Numeric vector. \code{alpha[i]} is the Pareto alpha in excess of \code{t[i]}.}

\item{truncation}{Numeric. If \code{truncation} is not \code{NULL} and \code{truncation > t}, then the distribution is truncated at \code{truncation}.}

\item{truncation_type}{Character. If \code{truncation_type = "wd"} then the whole distribution is truncated. If \code{truncation_type = "lp"} then a truncated Pareto is used for the last piece.}

\item{scale_pieces}{Numeric vector. If not \code{NULL} then the density of the i-th Pareto piece (on the interval (\code{t[i], t[i+1])}) is scaled with the factor \code{const * scale_pieces[i]} (where \code{const} is a normalization constant)}
}
\value{
A vector of \code{n} samples from the (truncated) piecewise Pareto distribution with parameter vectors \code{t} and \code{alpha}
}
\description{
Generates random deviates of a piecewise Pareto distribution
}
\examples{
t <- c(1000, 2000, 3000)
alpha <- c(1, 1.5, 2)
rPiecewisePareto(100, t, alpha)
rPiecewisePareto(100, t, alpha, truncation = 5000)
rPiecewisePareto(100, t, alpha, truncation = 5000, truncation_type = "lp")
rPiecewisePareto(100, t, alpha, truncation = 5000, truncation_type = "wd")

}
