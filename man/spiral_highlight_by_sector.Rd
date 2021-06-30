\name{spiral_highlight_by_sector}
\alias{spiral_highlight_by_sector}
\title{
Highlight a sector
}
\description{
Highlight a sector
}
\usage{
spiral_highlight_by_sector(x1, x2, x3 = NULL, x4 = NULL, padding = unit(1, "mm"),
    gp = gpar(fill = "red"))
}
\arguments{

  \item{x1}{Start location which determines the start of the sector.}
  \item{x2}{End location which determines the end of the sector. Note x2 should be larger than x1 and the angular difference between x1 and x2 should be smaller than a circle.}
  \item{x3}{Start location which determines the start of the sector on the upper border.}
  \item{x4}{End location which determines the end of the sector on the upper border.}
  \item{padding}{It controls the radial extension of the sector. The value should be a \code{\link[grid]{unit}} object with length one or two.}
  \item{gp}{Graphics parameters.}

}
\details{
x1 and x2 determine the position of the highlighted sector. If x3 and x4 are not set, the sector extends until the most outside loop.
If x3 and x4 are set, they determine the outer border of the sector. In this case, if x3 and x4 are set, x3 should be larger than x2.
}
\value{
No value is returned.
}
\examples{
spiral_initialize(xlim = c(0, 360*4), start = 360, end = 360*5)
spiral_track()
spiral_axis()
spiral_highlight_by_sector(36, 72)
spiral_highlight_by_sector(648, 684)
spiral_highlight_by_sector(216, 252, 936, 972, gp = gpar(fill = "blue"))
}
