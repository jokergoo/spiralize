\name{spiral_highlight_by_theta}
\alias{spiral_highlight_by_theta}
\title{
Highlight a sector in the curve
}
\description{
Highlight a sector in the curve
}
\usage{
spiral_highlight_by_theta(theta1, theta2, gp = gpar(fill = "red"))
}
\arguments{

  \item{theta1}{Start of the sector in the circle. Values are measured in degrees.}
  \item{theta2}{End of the sector in the circle. Values are measured in degrees.}
  \item{gp}{Graphics parameters.}

}
\examples{
spiral_initialize(); spiral_track()
spiral_highlight_by_theta(30, 60)
}
