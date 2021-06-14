\name{spiral_arrow}
\alias{spiral_arrow}
\title{
Draw circular arrows
}
\description{
Draw circular arrows
}
\usage{
spiral_arrow(
    x1, x2,
    y = get_track_data("ycenter", track_index),
    width = get_track_data("yrange", track_index)/3,
    track_index = current_track_index(),
    arrow_head_length = unit(4, "mm"),
    arrow_head_width = width*2,
    arrow_position = c("end", "start"),
    tail = c("normal", "point"),
    gp = gpar())
}
\arguments{

  \item{x1}{Start of the arrow.}
  \item{x2}{End of the arrow.}
  \item{y}{Position of the arrow on y location.}
  \item{width}{Width of the arrow.}
  \item{track_index}{Index of the track. }
  \item{arrow_head_length}{Length of the arrow head.}
  \item{arrow_head_width}{Width of the arrow head.}
  \item{arrow_position}{Position of the arrow. If the value is \code{"end"}, then the arrow head is drawn at \code{x = x2}. If the value is \code{"start"}, then the arrow head is drawn at \code{x = x1}. }
  \item{tail}{The shape of the arrow tail.}
  \item{gp}{Graphics parameters.}

}
\examples{
spiral_initialize()
spiral_track()
spiral_arrow(0.3, 0.6, gp = gpar(fill = "red"))
spiral_arrow(0.8, 0.9, gp = gpar(fill = "blue"), tail = "point", arrow_position = "start")
}
