\name{solve_theta_from_spiral_length}
\alias{solve_theta_from_spiral_length}
\title{
Get theta from given spiral lengths
}
\description{
Get theta from given spiral lengths
}
\usage{
solve_theta_from_spiral_length(len, interval = NULL, offset = 0)
}
\arguments{

  \item{len}{A vector of spiral lengths.}
  \item{interval}{Interval to search for the solution.}
  \item{offset}{Offset of the spiral. In the general form: \code{r = a + r*theta}, offset is the value of \code{a}.}

}
\details{
The length of the spiral has a complicated form, see \url{https://downloads.imagej.net/fiji/snapshots/arc_length.pdf} .
Let's say the form is \code{l = f(theta)}, \code{\link{solve_theta_from_spiral_length}} tries to find theta by a known \code{l}.
It uses \code{\link[stats]{uniroot}} to search solutions.
}
\value{
The theta value.
}
\examples{
spiral_initialize()
s = current_spiral()
theta = pi*seq(2, 3, length = 10)
len = s$spiral_length(theta)
solve_theta_from_spiral_length(len) # should be very similar as theta
}
