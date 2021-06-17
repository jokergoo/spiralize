\name{current_spiral}
\alias{current_spiral}
\title{
Get current spiral object
}
\description{
Get current spiral object
}
\usage{
current_spiral()
}
\details{
The returned value is an object of \code{spiral} reference class. The following methods might be useful:

\describe{
  \item{\code{$curve()}:}{It returns the radius for given angles (in radians).}
  \item{\code{$spiral_length()}:}{It returns the length of the spiral (from the origin) for a given angle (in radians), thus if you want to get the length of a spiral segment,  it will be \code{spiral$spiral_length(theta2) - spiral$spiral_length(theta1)} where \code{spiral} is the spiral object.}
}
}
\examples{
spiral_initialize()
s = current_spiral()
s$curve(2*pi*2)
s$spiral_length(2*pi*2)
}
