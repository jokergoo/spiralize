\name{get_track_data}
\alias{get_track_data}
\title{
Get meta-data of a track
}
\description{
Get meta-data of a track
}
\usage{
get_track_data(field, track_index = current_track_index())
}
\arguments{

  \item{field}{Name, see Details section.}
  \item{track_index}{Which track?}

}
\details{
There are following fields that can be retrieved for a given track:

\itemize{
  \item ymin Lower boundary of the data.
  \item ymax Upper boundary of the data.
  \item ycenter \code{(ymin + ymax)/2}
  \item ylim \code{c(ylim, ymax)}
  \item yrange \code{ymax - ymin}
  \item height Height of the track, measured as the fraction of the distance between two neighbouring circles.
}

It is more suggested to directly use \code{\link{TRACK_META}} to retrieve meta data for the current track.
}
\value{
A numeric value of the corresponding field.
}
\examples{
# There is no example
NULL

}
