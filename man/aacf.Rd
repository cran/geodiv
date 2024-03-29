% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aacf.R
\name{aacf}
\alias{aacf}
\title{Estimate the Areal Autocorrelation Function}
\usage{
aacf(x)
}
\arguments{
\item{x}{An n x n raster or matrix.}
}
\value{
A raster or matrix representation
  of the AACF. Both raster and matrix values are normalized
  so that the maximum is equal to 1.
}
\description{
Calculates the areal autocorrelation function (AACF) as the
inverse of the Fourier power spectrum. \code{aacf(x)} returns
the AACF in both matrix and raster format.
}
\examples{
# import raster image
data(normforest)
normforest <- terra::unwrap(normforest)

# calculate aacf img and matrix
aacf_out <- aacf(normforest)

# plot resulting aacf image
terra::plot(aacf_out)
}
