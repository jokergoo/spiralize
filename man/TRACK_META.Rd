\name{TRACK_META}
\docType{data}
\alias{TRACK_META}
\title{
Get meta data in the current track
}
\description{
Get meta data in the current track
}
\usage{
TRACK_META
}
\details{
The variable \code{TRACK_META} can only be used to get meta data from the "current" track. If the current track
is not the one you want, you can first use \code{\link{set_current_track}} to set the current track.

Don't directly use \code{TRACK_META}. The value of \code{\link{TRACK_META}} itself is meaningless. Always use in form of \code{TRACK_META$name}.

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
\examples{
# There is no example
NULL

}
