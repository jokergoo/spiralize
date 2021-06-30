\name{spiral_polygon}
\alias{spiral_polygon}
\title{
Add polygons to a track
}
\description{
Add polygons to a track
}
\usage{
spiral_polygon(x, y, id = NULL, gp = gpar(), track_index = current_track_index())
}
\arguments{

  \item{x}{X-locations of the data points.}
  \item{y}{Y-locations of the data points.}
  \item{id}{A numeric vector used to separate locations in x and y into multiple polygons.}
  \item{gp}{Graphical parameters.}
  \item{track_index}{Index of the track. }

}
\value{
No value is returned.
}
\details{
Note the polygon must be closed, which means, the last data point should overlap to the first one.
}
\examples{
# There is no example
NULL

}
