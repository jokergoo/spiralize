\name{spiral_initialize_by_date}
\alias{spiral_initialize_by_date}
\title{
Initialize the spiral from Date object
}
\description{
Initialize the spiral from Date object
}
\usage{
spiral_initialize_by_date(x = NULL, xlim = range(x), start = NULL, end = NULL,
    year_per_loop = 1, ...)
}
\arguments{

  \item{x}{A Date object.}
  \item{xlim}{Range of the dates.}
  \item{start}{Start of the spiral, in degrees. By default it is automatically calculated.}
  \item{end}{End of the spiral, in degrees. By default it is automatically calculated.}
  \item{year_per_loop}{How many years to put in a loop?}
  \item{...}{All pass to \code{\link{spiral_initialize}}.}

}
\details{
Each year (actually 364 days) is in a loop.
}
\examples{
spiral_initialize_by_date(xlim = as.Date(c("2014-01-01", "2021-06-17")))
spiral_track(height = 0.6)
spiral_axis()
}
