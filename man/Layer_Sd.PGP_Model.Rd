% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CollectiveModelMethods.R
\name{Layer_Sd.PGP_Model}
\alias{Layer_Sd.PGP_Model}
\title{Standard Deviation of a Reinsurance Layer}
\usage{
\method{Layer_Sd}{PGP_Model}(CollectiveModel, Cover = Inf, AttachmentPoint = 0)
}
\arguments{
\item{CollectiveModel}{PGP_Model object.}

\item{Cover}{Numeric. Cover of the reinsurance layer. Use \code{Inf} for unlimited layers.}

\item{AttachmentPoint}{Numeric. Attachment point of the reinsurance layer.}
}
\value{
The standard deviation of the loss in the layer \code{Cover} xs \code{AttachmentPoint} for the given \code{CollectiveModel}
}
\description{
Calculates the standard deviation of the loss in a reinsurance layer for a PGP_model
}
\examples{
PGPM <- PGP_Model(2, 1000, 1, 2, dispersion = 2)
PGPM
Example1_Cov <- c(diff(Example1_AP), Inf)
Layer_Sd(PGPM, Example1_Cov, Example1_AP)

}
