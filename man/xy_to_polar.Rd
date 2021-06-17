\name{xy_to_polar}
\alias{xy_to_polar}
\title{
Convert data coordinates to polar coordinates
}
\description{
Convert data coordinates to polar coordinates
}
\usage{
xy_to_polar(x, y, track_index = current_track_index())
}
\arguments{

  \item{x}{X-locations of the data points.}
  \item{y}{Y-locations of the data points.}
  \item{track_index}{Index of the track. }

}
\details{
Note different settings of \code{flip} and \code{reverse} in \code{\link{spiral_initialize}} affect the conversion.
}
\value{
A data frame with two columns: theta (in radians) and r (the radius).
}
\examples{
# There is no example
NULL

}
