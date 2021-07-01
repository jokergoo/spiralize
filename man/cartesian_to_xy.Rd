\name{cartesian_to_xy}
\alias{cartesian_to_xy}
\title{
Convert canvas coordinates to the data coordinates
}
\description{
Convert canvas coordinates to the data coordinates
}
\usage{
cartesian_to_xy(x, y, track_index = current_track_index())
}
\arguments{

  \item{x}{X-locations of the data points in canvas coordinates.}
  \item{y}{Y-locations of the data points in canvas coordinates.}
  \item{track_index}{Index of the track. }

}
\details{
The data points are assigned to the nearest inner loops. Denote the a data point has a coordinate (r, theta)
in the polar coordinate system, r_k and r_{k+1} are the radius of the two loops at theta + 2*pi*a and theta + 2*pi*(a+1) that below and above the data point,
the data point is assigned to the loop k.
}
\value{
A data frame with two columns: x and y.
}
\examples{
x = runif(100, -5, 5)
y = runif(100, -5, 5)
spiral_initialize()
spiral_track()
df = cartesian_to_xy(x, y)
# directly draw in the viewport
grid.points(x, y, default.units="native")
# check whether the converted xy are correct (should overlap to the previous points)
spiral_points(df$x, df$y, pch = 16)
}
