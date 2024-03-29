% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{PiecewisePareto_ML_Estimator_Alpha}
\alias{PiecewisePareto_ML_Estimator_Alpha}
\title{Maximum Likelihood Estimation of the Alphas of the Piecewise Pareto Distribution}
\usage{
PiecewisePareto_ML_Estimator_Alpha(
  losses,
  t,
  truncation = NULL,
  truncation_type = "lp",
  reporting_thresholds = NULL,
  is.censored = NULL,
  weights = NULL,
  alpha_min = 0.001,
  alpha_max = 10
)
}
\arguments{
\item{losses}{Numeric vector. Losses that are used for the ML estimation.}

\item{t}{Numeric vector. Thresholds of the piecewise Pareto distribution.}

\item{truncation}{Numeric. If \code{truncation} is not \code{NULL} and \code{truncation > max(t)}, then the distribution is truncated at \code{truncation}.}

\item{truncation_type}{Character. If \code{truncation_type = "wd"} then the whole distribution is truncated. If \code{truncation_type = "lp"} then a truncated Pareto is used for the last piece.}

\item{reporting_thresholds}{Numeric vector. Allows to enter loss specific reporting thresholds. If \code{NULL} then all reporting thresholds are assumed to be less than or equal to \code{t[1]}.}

\item{is.censored}{Logical vector. \code{TRUE} indicates that a loss has been censored by the policy limit. The assumption is that the uncensored losses are piecewise Pareto distributed with the alphas we are looking for. \code{is.censored = NULL} means that no losses are censored.}

\item{weights}{Numeric vector. Weights for the losses. For instance \code{weights[i] = 2} and \code{weights[j] = 1} for \code{j != i} has the same effect as adding another loss of size \code{loss[i]}.}

\item{alpha_min}{Numeric. Lower bound for the estimated alphas (only used in truncated case).}

\item{alpha_max}{Numeric. Upper bound for the estimated alphas (only used in truncated case).}
}
\value{
Maximum likelihood estimator for the parameter \code{alpha} of a Pareto distribution with threshold \code{t} given the observations \code{losses}
}
\description{
Calculates the maximum likelihood estimator of the parameter vector alpha for a a piecewise Pareto distribution with given vector t
and (if applicable) a known truncation
}
\examples{
losses <- rPiecewisePareto(10000, t = c(100,200,300), alpha = c(1,2,3))
PiecewisePareto_ML_Estimator_Alpha(losses, c(100,200,300))

losses <- rPiecewisePareto(10000, t = c(100,200,300), alpha = c(1,2,3),
                           truncation = 500, truncation_type = "wd")
PiecewisePareto_ML_Estimator_Alpha(losses, c(100,200,300))
PiecewisePareto_ML_Estimator_Alpha(losses, c(100,200,300),
                                 truncation = 500, truncation_type = "wd")
reporting_thresholds <- rPareto(10000, 100, 3)
index <- losses > reporting_thresholds
losses <- losses[index]
reporting_thresholds <- reporting_thresholds[index]
PiecewisePareto_ML_Estimator_Alpha(losses, c(100,200,300),
                                 truncation = 500, truncation_type = "wd")
PiecewisePareto_ML_Estimator_Alpha(losses, c(100,200,300),
                                 truncation = 500, truncation_type = "wd",
                                 reporting_thresholds = reporting_thresholds)

losses <- c(140, 240, 490, 200, 110, 710, 120, 190, 210, 310)
w <- rep(1, length(losses))
w[1] <- 2
losses2 <- c(losses, losses[1])
PiecewisePareto_ML_Estimator_Alpha(losses, c(100,200,300), weights = w)
PiecewisePareto_ML_Estimator_Alpha(losses2, c(100,200,300))
}
