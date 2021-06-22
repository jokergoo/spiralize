\name{spiral_horizon}
\alias{spiral_horizon}
\title{
Draw horizon chart along the spiral
}
\description{
Draw horizon chart along the spiral
}
\usage{
spiral_horizon(x, y, n_slices = 4, slice_size, pos_fill = "#D73027", neg_fill = "#313695",
    use_bars = FALSE, bar_width = min(diff(x)),
    negative_from_top = FALSE, track_index = current_track_index())
}
\arguments{

  \item{x}{X-locations of the data points.}
  \item{y}{Y-locations of the data points.}
  \item{n_slices}{Number of slices.}
  \item{slice_size}{Size of the slices. The final number of sizes is \code{ceiling(max(abs(y))/slice_size)}.}
  \item{pos_fill}{Colors for positive values. }
  \item{neg_fill}{Colors for negative values.}
  \item{use_bars}{Whether to use bars?}
  \item{bar_width}{Width of bars.}
  \item{negative_from_top}{Should negative distribution be drawn from the top?}
  \item{track_index}{Index of the track. }

}
\details{
Since the track height is very small in the spiral, horizon chart visualization is a efficient way to visualize
distribution-like graphics.
}
\value{
A list of the following objects:

\itemize{
  \item a color mapping function for colors.
  \item a vector of intervals that split the data.
}
}
\examples{
\donttest{
# df = cranlogs::cran_downloads("ggplot2", from="2014-01-01")
# the data frame `df` is already downloaded
df = readRDS(system.file("extdata", "ggplot2_download.rds", package = "spiralize"))
df = df[df$date < as.Date("2021-01-01"), ]
day_diff = as.numeric(df$date[nrow(df)] - df$date[1])
v = df$count
v[v > quantile(v, 0.95)] = quantile(v, 0.95)
spiral_initialize(xlim = c(0, nrow(df)), start = 360, end = 360*(day_diff/364)) # a circle 52 weeks
spiral_track(height = 0.9)
spiral_horizon(1:nrow(df), v)

spiral_initialize(xlim = c(0, nrow(df)), start = 360, end = 360*(day_diff/364))
spiral_track(height = 0.9)
spiral_horizon(1:nrow(df), v, use_bars = TRUE)
}
}
