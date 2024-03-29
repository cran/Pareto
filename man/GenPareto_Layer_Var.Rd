% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{GenPareto_Layer_Var}
\alias{GenPareto_Layer_Var}
\title{Layer Variance of the Generalized Pareto Distribution}
\usage{
GenPareto_Layer_Var(
  Cover,
  AttachmentPoint,
  t,
  alpha_ini,
  alpha_tail,
  truncation = NULL
)
}
\arguments{
\item{Cover}{Numeric. Cover of the reinsurance layer. Use \code{Inf} for unlimited layers.}

\item{AttachmentPoint}{Numeric. Attachment point of the reinsurance layer.}

\item{t}{Numeric. Threshold of the Pareto distribution. If \code{t} is \code{NULL} (default) then \code{t <- Attachment Point} is used}

\item{alpha_ini}{Numeric. Initial Pareto alpha (at \code{t}).}

\item{alpha_tail}{Numeric. Tail Pareto alpha.}

\item{truncation}{Numeric. If \code{truncation} is not \code{NULL} and \code{truncation > t}, then the Pareto distribution is truncated at \code{truncation}.}
}
\value{
Variance of the (truncated) generalized Pareto distribution with parameters \code{t}, \code{alpha_ini} and \code{alpha_tail} in the layer
        \code{Cover} xs \code{AttachmentPoint}
}
\description{
Calculates the variance of a generalized Pareto distribution in a reinsurance layer
}
\examples{
GenPareto_Layer_Var(4000, 1000, 1000, 1, 2)
GenPareto_Layer_Var(4000, 1000, t = 1000, alpha_ini = 1, alpha_tail = 3)
GenPareto_Layer_Var(4000, 1000, t = 5000, alpha_ini = 1, alpha_tail = 3)
GenPareto_Layer_Var(4000, 1000, t = 1000, alpha_ini = 1, alpha_tail = 3, truncation = 5000)
GenPareto_Layer_Var(9000, 1000, t = 1000, alpha_ini = 1, alpha_tail = 3, truncation = 5000)

}
