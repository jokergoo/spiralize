\name{spiral_initialize_by_gcoor}
\alias{spiral_initialize_by_gcoor}
\title{
Initialize the spiral with genomic coordinates
}
\description{
Initialize the spiral with genomic coordinates
}
\usage{
spiral_initialize_by_gcoor(x = NULL, xlim = range(x), scale_by = "curve_length", ...)
}
\arguments{

  \item{x}{A vector of genomic coordinates.}
  \item{xlim}{Range of the genomic coordinates.}
  \item{scale_by}{For genomic plot, axis is linearly scaled by the curve length.}
  \item{...}{All pass to \code{\link{spiral_initialize}}.}

}
\details{
It is basically the same as \code{\link{spiral_initialize}}. The only difference is the axis labels are automatically
formated for genomic coordinates.
}
\examples{
spiral_initialize_by_gcoor(c(1, 1000000000))
spiral_track()
spiral_axis()
}