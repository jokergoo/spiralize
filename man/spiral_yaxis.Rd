\name{spiral_yaxis}
\alias{spiral_yaxis}
\title{
Draw y-axis
}
\description{
Draw y-axis
}
\usage{
spiral_yaxis(side = c("start", "end", "both"), at = NULL, labels = TRUE, ticks_length = unit(2, "bigpts"),
    ticks_gp = gpar(), labels_gp = gpar(fontsize = 6),
    track_index = current_track_index())
}
\arguments{

  \item{side}{On which side of the spiral the y-axis is drawn? "start" means the inside of the spiral and "end" means the outside of the spiral.}
  \item{at}{Break points.}
  \item{labels}{Corresponding labels for the break points.}
  \item{ticks_length}{Length of the tick. Value should be a \code{\link[grid]{unit}} object.}
  \item{ticks_gp}{Graphics parameters for ticks.}
  \item{labels_gp}{Graphics parameters for labels.}
  \item{track_index}{Index of the track.}

}
\examples{
spiral_initialize(); spiral_track(height = 0.8)
spiral_yaxis("start")
spiral_yaxis("end", at = c(0, 0.25, 0.5, 0.75, 1), labels = letters[1:5])
}
