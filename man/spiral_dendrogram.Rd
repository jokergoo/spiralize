\name{spiral_dendrogram}
\alias{spiral_dendrogram}
\title{
Draw dendrogram
}
\description{
Draw dendrogram
}
\usage{
spiral_dendrogram(dend, gp = gpar(), track_index = current_track_index())
}
\arguments{

  \item{dend}{A \code{stats::dendrogram} object.}
  \item{gp}{Graphics parameters of the dendrogram edges.}
  \item{track_index}{Index of the track. }

}
\details{
Note the dendrogram edges can be rendered with the \code{\link[dendextend:dendextend-package]{dendextend}} package.
}
\value{
Height of the dendrogram.
}
\examples{
k = 500
dend = as.dendrogram(hclust(dist(runif(k))))
spiral_initialize(xlim = c(0, k), start = 360, end = 360*3)
spiral_track(height = 0.8, background_gp = gpar(fill = "#EEEEEE", col = NA))

\donttest{
require(dendextend)
dend = color_branches(dend, k = 4)
spiral_initialize(xlim = c(0, k), start = 360, end = 360*3)
spiral_track(height = 0.8, background_gp = gpar(fill = "#EEEEEE", col = NA))
}
}
