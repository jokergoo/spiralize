\name{xy_to_cartesian}
\alias{xy_to_cartesian}
\title{
Convert data coordinates to the canvas coordinates
}
\description{
Convert data coordinates to the canvas coordinates
}
\usage{
xy_to_cartesian(x, y, track_index = current_track_index())
}
\arguments{

  \item{x}{X-locations of the data points.}
  \item{y}{Y-locations of the data points.}
  \item{track_index}{Index of the track. }

}
\details{
The canvas coordinates correspond to the "native" coordinates of the viewport where the graphics are to be drawn.

Note different settings of \code{flip} and \code{reverse} in \code{\link{spiral_initialize}} affect the conversion.
}
\value{
A data frame with two columns: x and y.
}
\examples{
# There is no example
NULL

}
