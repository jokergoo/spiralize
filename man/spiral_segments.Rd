\name{spiral_segments}
\alias{spiral_segments}
\title{
Add segments to a track
}
\description{
Add segments to a track
}
\usage{
spiral_segments(x0, y0, x1, y1, gp = gpar(), arrow = NULL,
    track_index = current_track_index(), buffer = 10000)
}
\arguments{

  \item{x0}{X-locations of the start points of the segments.}
  \item{y0}{Y-locations of the start points of the segments.}
  \item{x1}{X-locations of the end points of the segments.}
  \item{y1}{Y-locations of the end points of the segments.}
  \item{gp}{Graphical parameters.}
  \item{arrow}{A \code{\link[grid]{arrow}} object.}
  \item{track_index}{Index of the track. }
  \item{buffer}{Number of segments to buffer.}

}
\details{
The segments on spiral are not straight lines while are more like curves. This means a spiral segment is formed by a list of real straight segments.
If there are n1 spiral segments, then there will be n2 straight segments where n2 is normally much larger than n1. To speed up drawing the spiral segments,
the locations of the "real" segments are filled to a temporary data frame with \code{buffer} rows, when the number of rows exceeds \code{buffer}, \code{\link[grid]{grid.segments}}
is called to draw all the buffered segments.
}
\value{
No value is returned.
}
\examples{
n = 1000
x0 = runif(n)
y0 = runif(n)
x1 = x0 + runif(n, min = -0.01, max = 0.01)
y1 = 1 - y0

spiral_initialize(xlim = range(c(x0, x1)))
spiral_track()
spiral_segments(x0, y0, x1, y1, gp = gpar(col = circlize::rand_color(n)))

n = 100
x0 = runif(n)
y0 = runif(n)
x1 = x0 + runif(n, min = -0.01, max = 0.01)
y1 = 1 - y0

spiral_initialize(xlim = range(c(x0, x1)))
spiral_track()
spiral_segments(x0, y0, x1, y1, arrow = arrow(length = unit(2, "mm")),
    gp = gpar(col = circlize::rand_color(n, luminosity = "bright"), lwd = runif(n, 0.5, 3)))
}
