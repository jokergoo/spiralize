\name{spiral_initialize_by_time}
\alias{spiral_initialize_by_time}
\title{
Initialize the spiral from time objects
}
\description{
Initialize the spiral from time objects
}
\usage{
spiral_initialize_by_time(xlim, start = NULL, end = NULL,
    unit_on_axis = c("days", "months", "weeks", "hours", "mins", "secs"),
    period = c("years", "months", "weeks", "days", "hours", "mins"),
    period_per_loop = 1, polar_lines_by = NULL, verbose = TRUE, ...)
}
\arguments{

  \item{xlim}{Range of the time. The value can be time object such as \code{Date}, \code{POSIXlt} or \code{POSIXct}. The value can also be characters and it is converted to time objects automatically.}
  \item{start}{Start of the spiral, in degrees. By default it is automatically calculated.}
  \item{end}{End of the spiral, in degrees. By default it is automatically calculated.}
  \item{unit_on_axis}{Units on the axis.}
  \item{period}{Which period to use?}
  \item{period_per_loop}{How many periods to put in a loop?}
  \item{polar_lines_by}{By default different value of \code{polar_lines_by} is set for different \code{period}. E.g. 360/7 is set if \code{period} is "weeks" or 360/24 is set if \code{peroid} is set to "hours".}
  \item{verbose}{Whether to print messages?}
  \item{...}{All pass to \code{\link{spiral_initialize}}.}

}
\details{
"start" and "end" are automatically calculated for different "unit_on_axis" and "period". For example, if "unit_on_axis" is "days" and "period" is "years", then
the first day of each each year is always put on theta = 0 + 2*pi*k where k is the index of loops.
}
\value{
No value is returned.
}
\examples{
spiral_initialize_by_time(xlim = c("2014-01-01", "2021-06-17"))
spiral_track(height = 0.6)
spiral_axis()

spiral_initialize_by_time(xlim = c("2021-01-01 00:00:00", "2021-01-05 00:00:00"))
spiral_track(height = 0.6)
spiral_axis()

spiral_initialize_by_time(xlim = c("2021-01-01 00:00:00", "2021-01-01 00:10:00"),
    unit_on_axis = "secs", period = "mins")
spiral_track(height = 0.6)
spiral_axis()
}
