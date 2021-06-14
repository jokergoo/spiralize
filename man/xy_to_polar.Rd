\name{xy_to_polar}
\alias{xy_to_polar}
\title{
Convert coordinate from data coordinate to polar coordinate
}
\description{
Convert coordinate from data coordinate to polar coordinate
}
\usage{
xy_to_polar(x, y, track_index = current_track_index())
}
\arguments{

  \item{x}{X coordinates of the data points.}
  \item{y}{Y coordinates of the data points.}
  \item{track_index}{Index of the track. }

}
\details{
Note different settings of \code{flip} and \code{reverse} set in \code{\link{spiral_initialize}} affect the conversion.
}
\value{
A data frame with two columns: theta and r.
}
\examples{
# There is no example
NULL

}
