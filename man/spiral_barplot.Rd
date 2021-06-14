\name{spiral_barplot}
\alias{spiral_barplot}
\title{
Add rectangles to a track
}
\description{
Add rectangles to a track
}
\usage{
spiral_barplot(pos, value, baseline = get_track_data("ymin", track_index),
    bar_width = min(diff(pos)), gp = gpar(), track_index = current_track_index())
}
\arguments{

  \item{pos}{X coordinates of the left bottom angle of the rectangles.}
  \item{value}{Y coordinates of the left bottom angle of the rectangles.}
  \item{baseline}{Baseline of the bars. Note it only works when \code{value} is a simple vector.}
  \item{bar_width}{X coordinates of the right top angle of the rectangles.}
  \item{gp}{Graphical parameters.}
  \item{track_index}{Index of the track. }

}
\examples{
x = seq(1, 1000, by = 1) - 0.5
y = runif(1000)
spiral_initialize(xlim = c(0, 1000))
spiral_track(height = 0.8)
spiral_barplot(x, y)

y = matrix(runif(3*1000), ncol = 3)
y = y/rowSums(y)
spiral_initialize(xlim = c(0, 1000))
spiral_track(height = 0.8)
spiral_barplot(x, y, gp = gpar(fill = 2:4, col = NA))
}
