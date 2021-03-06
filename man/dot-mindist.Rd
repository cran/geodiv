% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aacf.R
\name{.mindist}
\alias{.mindist}
\title{Estimate Minimum Correlation Length}
\usage{
.mindist(threshold, aacfimg, distimg)
}
\arguments{
\item{threshold}{A number with a value between 0 and 1. Indicates
the autocorrelation value to which the rates of decline are measured.}

\item{aacfimg}{A raster of the areal autocorrelation function. This
is the AACF raster split in two in terms of height.}

\item{distimg}{A raster of distances to all pixels from the center of the
original image. Distances are in meters if original raster was
unprojected, and are in map units (usually meters) if raster was projected
(see raster::distance documentation for more details).}
}
\value{
A list containing the minimum distances from an
  autocorrelation value of 1 to the specified autocorrelation value < 1.
  Distances are meters if original raster was unprojected, and are in
  map units (usually meters) if raster was projected (see
  raster::distance documentation for more details).
}
\description{
Internal function to calculates the minimum distances to specified
autocorrelation values (e.g., 0.2) of the areal autocorrelation
function (AACF). All 180 degrees from the origin of the AACF image
are considered for the calculation.
}
