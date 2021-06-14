\name{xy_to_cartesian}
\alias{xy_to_cartesian}
\title{
Convert coordinate from data coordinate to the canvas coordinate
}
\description{
Convert coordinate from data coordinate to the canvas coordinate
}
\usage{
xy_to_cartesian(x, y, track_index = current_track_index())
}
\arguments{

  \item{x}{X coordinates of the data points.}
  \item{y}{Y coordinates of the data points.}
  \item{track_index}{Index of the track. }

}
\details{
The canvas coordinate system corresponds to the "native unit" coordinate of the viewport where the graphics are to be drawn.

Note different settings of \code{flip} and \code{reverse} set in \code{\link{spiral_initialize}} affect the conversion.
}
\value{
A data frame with two columns: x and y.
}
\examples{
# There is no example
NULL

}
