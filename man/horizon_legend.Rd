\name{horizon_legend}
\alias{horizon_legend}
\title{
Legend for the horizon chart
}
\description{
Legend for the horizon chart
}
\usage{
horizon_legend(lt, title = "", format = "\%.2f",
    template = "[{x1}, {x2}]", direction = c("both", "positive", "negative"), ...)
}
\arguments{

  \item{lt}{The object returned by \code{\link{spiral_horizon}}.}
  \item{title}{Title of the legend.}
  \item{format}{Number format of the legend labels.}
  \item{template}{Template to construct the labels.}
  \item{direction}{Whether only show the positive or negative values in the legend or both.}
  \item{...}{Pass to \code{\link[ComplexHeatmap]{Legend}}.}

}
\examples{
# There is no example
NULL

}
