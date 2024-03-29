% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PPPModel.R
\name{PPP_Model_Layer_Sd}
\alias{PPP_Model_Layer_Sd}
\title{Standard Deviation of a Reinsurance Layer}
\usage{
PPP_Model_Layer_Sd(Cover, AttachmentPoint, PPP_Model)
}
\arguments{
\item{Cover}{Numeric. Cover of the reinsurance layer. Use \code{Inf} for unlimited layers.}

\item{AttachmentPoint}{Numeric. Attachment point of the reinsurance layer.}

\item{PPP_Model}{PPP_Model object.}
}
\value{
The standard deviation of the loss in the layer \code{Cover} xs \code{AttachmentPoint} for the given \code{PPP_Model}
}
\description{
Calculates the standard deviation of the loss in a reinsurance layer for a PPP_Model
}
\examples{
PPPM <- PiecewisePareto_Match_Layer_Losses(Example1_AP, Example1_EL)
PPPM
Example1_Cov <- c(diff(Example1_AP), Inf)
Layer_Sd(PPPM, Example1_Cov, Example1_AP)

}
