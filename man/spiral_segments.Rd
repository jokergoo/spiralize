\name{spiral_segments}
\alias{spiral_segments}
\title{
Add segments to a track
}
\description{
Add segments to a track
}
\usage{
spiral_segments(x0, y0, x1, y1, gp = gpar(),
    track_index = current_track_index(), buffer = 10000)
}
\arguments{

  \item{x0}{X coordinates of the start points of the segments.}
  \item{y0}{Y coordinates of the start points of the segments.}
  \item{x1}{X coordinates of the end points of the segments.}
  \item{y1}{Y coordinates of the end points of the segments.}
  \item{gp}{Graphical parameters.}
  \item{track_index}{Index of the track. }
  \item{buffer}{Number of segments to buffer.}

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
}
