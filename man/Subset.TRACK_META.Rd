\name{$.TRACK_META}
\alias{Subset.TRACK_META}
\alias{$.TRACK_META}
\title{
Get meta data in the current track
}
\description{
Get meta data in the current track
}
\usage{
\method{$}{TRACK_META}(x, name)
}
\arguments{

  \item{x}{Always use \code{TRACK_META}.}
  \item{name}{Name of the meta name. For all supported names, type \code{names(TRACK_META)}.}

}
\details{
The variable \code{TRACK_META} can only be used to get meta data from the "current" track. If the current track
is not the one you want, you can first use \code{\link{set_current_track}} to set the current track.

There are following meta data for the current track:

\describe{
  \item{\code{xlim}:}{Data range on x-axis.}
  \item{\code{xmin}:}{\code{xlim[1]}.}
  \item{\code{xmax}:}{\code{xlim[2]}.}
  \item{\code{xrange}:}{\code{xlim[2] - xlim[1]}.}
  \item{\code{xcenter}:}{\code{mean(xlim)}.}
  \item{\code{theta_lim}:}{Range of the angles on the spiral, measured in radians.}
  \item{\code{theta_min}:}{\code{theta_lim[1]}.}
  \item{\code{theta_max}:}{\code{theta_lim[2]}.}
  \item{\code{theta_range}:}{\code{theta_lim[2] - theta_lim[1]}.}
  \item{\code{theta_center}:}{\code{mean(theta_lim)}.}
  \item{\code{ylim}:}{Data range on y-axis.}
  \item{\code{ymin}:}{\code{ylim[1]}.}
  \item{\code{ymax}:}{\code{ylim[2]}.}
  \item{\code{yrange}:}{\code{ylim[2] - ylim[1]}.}
  \item{\code{ycenter}:}{\code{mean(ylim)}.}
  \item{\code{rel_height}:}{Fraction of height of the track to the distance between two neighbouring loops.}
  \item{\code{abs_height}:}{The height of the track, which is \code{rel_height} multiplied by the distance between two neighbouring loops.}
  \item{\code{track_index}:}{Current track index.}
}
}
\value{
The corresponding value.
}
\examples{
# There is no example
NULL

}
