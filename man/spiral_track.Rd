\name{spiral_track}
\alias{spiral_track}
\title{
Add a new track or go to an existed track
}
\description{
Add a new track or go to an existed track
}
\usage{
spiral_track(ylim = c(0, 1), height = 0.8, background = TRUE,
    background_gp = gpar(col = NA, fill = "#EEEEEE"), reverse_y = FALSE,
    track_index = current_track_index() + 1)
}
\arguments{

  \item{ylim}{Data range of the y-locations.}
  \item{height}{Height of the track. The value can be the fraction of the distance of the two neighbour loops. The value can also be a \code{\link[grid]{unit}} object.}
  \item{background}{Whether to draw the background of the track, i.e. border and filled color of background.}
  \item{background_gp}{Graphics parameters of the background.}
  \item{reverse_y}{Whether reverse the direction of y-axis.}
  \item{track_index}{Index of the track. }

}
\details{
If the track is already existed, the function simply mark the track as the current track and does nothing else.
}
\value{
No value is returned.
}
\examples{
spiral_initialize()
spiral_track(height = 0.8)

spiral_initialize()
spiral_track(height = 0.4, background_gp = gpar(fill = "red"))
spiral_track(height = 0.2, background_gp = gpar(fill = "green"))
spiral_track(height = 0.1, background_gp = gpar(fill = "blue"))
}
