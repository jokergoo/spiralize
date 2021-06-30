\name{spiral_opt}
\alias{spiral_opt}
\title{
Global options
}
\description{
Global options
}
\usage{
spiral_opt(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE)
}
\arguments{

  \item{...}{Arguments for the parameters, see "details" section.}
  \item{RESET}{Whether to reset to default values.}
  \item{READ.ONLY}{Please ignore.}
  \item{LOCAL}{Please ignore.}
  \item{ADD}{Please ignore.}

}
\details{
There are following global parameters:

\describe{
  \item{\code{min_segment_len}}{Minimal length of the segment that partitions a curve.}
}

To access the value of an option: \code{spiral_opt$name} where \code{name} is the name of the option. To set a new value
for an option: \code{spiral_opt$name = new_value}.
}
\value{
A list of options.
}
\examples{
# There is no example
NULL

}
