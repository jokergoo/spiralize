\name{spiral_track}
\alias{spiral_track}
\title{
Add a track or go to an existed track
}
\description{
Add a track or go to an existed track
}
\usage{
spiral_track(ylim = c(0, 1), height = 0.5, background = TRUE,
    background_gp = gpar(fill = "#EEEEEE"), track_index = current_track_index() + 1)
}
\arguments{

  \item{ylim}{Data range of the y coordinates.}
  \item{height}{Height of the track. The value should be the fraction of the distance of the two neighbouring circles.}
  \item{background}{Whether draw the background of the track, i.e. border and filled color of background.}
  \item{background_gp}{Graphics parameters of the background.}
  \item{track_index}{Index of the track. }

}
\details{
If the track is already existed, the function simply mark the track as the current track and does nothing else.
}
\examples{
spiral_initialize()
spiral_track(height = 0.8)

spiral_initialize()
spiral_track(height = 0.4, background_gp = gpar(fill = "red"))
spiral_track(height = 0.2, background_gp = gpar(fill = "green"))
spiral_track(height = 0.1, background_gp = gpar(fill = "blue"))
}
