\name{spiral_highlight}
\alias{spiral_highlight}
\title{
Highlight segments in the curve
}
\description{
Highlight segments in the curve
}
\usage{
spiral_highlight(x1, x2, type = c("rect", "line"),
    line_side = c("inside", "outside"), line_width = unit(1, "bigpts"),
    gp = gpar(fill = "red"), track_index = current_track_index())
}
\arguments{

  \item{x1}{Start of the segments.}
  \item{x2}{End of the segments.}
  \item{type}{Type of the highlighting. "rect" means drawing transparent rectangles covering the whole track. "line" means drawing annotation lines on top of the track or at the bottom of it.}
  \item{line_side}{If the highlight type is "line", it controls which side of the track to draw the lines.}
  \item{line_width}{Width of the annotation line. Value should be a \code{\link[grid]{unit}} object.}
  \item{gp}{Graphics parameters.}
  \item{track_index}{Index of the track.}

}
\examples{
spiral_initialize(); spiral_track()
spiral_highlight(0.4, 0.6)
spiral_highlight(0.1, 0.2, type = "line", gp = gpar(col = "blue"))
spiral_highlight(0.7, 0.8, type = "line", line_side = "outside")
}
