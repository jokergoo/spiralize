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
    template = "[{x1}, {x2}]", ...)
}
\arguments{

  \item{lt}{The object returned by \code{\link{spiral_horizon}}.}
  \item{title}{Title of the legend.}
  \item{format}{Number format of the legend labels.}
  \item{template}{Template to construct the labels.}
  \item{...}{Pass to \code{\link[ComplexHeatmap]{Legend}}.}

}
\value{
A \code{\link[ComplexHeatmap]{Legend}} object.
}
\examples{
# There is no example
NULL

}
