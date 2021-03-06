% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/movingwindow.R
\name{window_metric}
\alias{window_metric}
\title{Calculate Texture Metric for Single Pixel}
\usage{
window_metric(
  x,
  i,
  window_type = "square",
  size = 11,
  rownum,
  colnum,
  metric,
  args = NULL
)
}
\arguments{
\item{x}{A raster or matrix.}

\item{i}{Index of cell at which to calculate the metric.}

\item{window_type}{Character. Type of window, either circular or square.}

\item{size}{Numeric. Radius of window in number of pixels.}

\item{rownum}{Vector of row numbers at which to calculate the metric.}

\item{colnum}{Vector of column numbers at which to calculate the metric.}

\item{metric}{Character. Metric to calculate for each window. Metrics
from the geodiv package are listed below.}

\item{args}{List. Arguments from function to be applied over each window
(e.g., list(threshold = 0.2)).}
}
\value{
A raster with pixel values representative of the metric
value for the window surrounding that pixel.
}
\description{
Calculates the various texture metrics over a window centered
on an individual pixel.
}
\details{
Metrics from geodiv package:
\enumerate{
   \item{\code{'sa'}: average surface roughness}
   \item{\code{'sq'}: root mean square roughness}
   \item{\code{'s10z'}: ten-point height}
   \item{\code{'sdq'}: root mean square slope of surface, 2-point method}
   \item{\code{'sdq6'}: root mean square slope of surface, 7-point method}
   \item{\code{'sdr'}: surface area ratio}
   \item{\code{'sbi'}: surface bearing index}
   \item{\code{'sci'}: core fluid retention index}
   \item{\code{'ssk_adj'}: adjusted skewness}
   \item{\code{'ssk'}: skewness}
   \item{\code{'sku_exc'}: excess kurtosis}
   \item{\code{'sku'}: kurtosis}
   \item{\code{'sds'}: summit density}
   \item{\code{'sfd'}: 3d fractal dimension}
   \item{\code{'srw'}: dominant radial wavelength}
   \item{\code{'srwi'}: radial wavelength index}
   \item{\code{'shw'}: mean half wavelength}
   \item{\code{'std'}: angle of dominating texture}
   \item{\code{'stdi'}: texture direction index}
   \item{\code{'svi'}: valley fluid retention index}
   \item{\code{'stxr'}: texture aspect ratio}
   \item{\code{'ssc'}: mean summit curvature}
   \item{\code{'sv'}: maximum valley depth}
   \item{\code{'sph'}: maximum peak height}
   \item{\code{'sk'}: core roughness depth}
   \item{\code{'smean'}: mean peak height}
   \item{\code{'svk'}: reduced valley depth}
   \item{\code{'spk'}: reduced peak height}
   \item{\code{'scl'}: correlation length}
   \item{\code{'sdc'}: bearing area curve height interval}
}
}
\note{
Note that if calculating the metric at the edge of a raster or matrix,
the input raster/matrix must be padded. This can be done using the \code{pad_edges}
function.
}
\examples{
library(raster)

# import raster image
data(normforest)

# crop raster to much smaller area if on a smaller computer
x <- crop(normforest, extent(-123, -122.99, 43, 43.01))

# get coordinates, rownums, cellnums
pixlist <- seq(1, length(x), 1)
ext_x <- pad_edges(x, size = 4)
rownum <- rowFromCell(x, pixlist) + 4
colnum <- colFromCell(x, pixlist) + 4

# get a surface of root mean square roughness
sq_val <- window_metric(x = x, i = 40, window = 'square',
size = 4, rownum = rownum, colnum = colnum, metric = 'sq')
}
