\name{spiral_initialize_by_gcoor}
\alias{spiral_initialize_by_gcoor}
\title{
Initialize the spiral with genomic coordinates
}
\description{
Initialize the spiral with genomic coordinates
}
\usage{
spiral_initialize_by_gcoor(xlim, scale_by = "curve_length", ...)
}
\arguments{

  \item{xlim}{Range of the genomic coordinates.}
  \item{scale_by}{For genomic plot, axis is linearly scaled by the curve length.}
  \item{...}{All pass to \code{\link{spiral_initialize}}.}

}
\details{
It is basically the same as \code{\link{spiral_initialize}}. The only difference is the axis labels are automatically
formated for genomic coordinates.
}
\value{
No value is returned.
}
\examples{
spiral_initialize_by_gcoor(c(0, 1000000000))
spiral_track()
spiral_axis()
}
