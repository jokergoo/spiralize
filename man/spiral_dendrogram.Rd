\name{spiral_dendrogram}
\alias{spiral_dendrogram}
\title{
Draw dendrogram
}
\description{
Draw dendrogram
}
\usage{
spiral_dendrogram(dend, facing = c("inside", "outside"), gp = gpar(),
    track_index = current_track_index())
}
\arguments{

  \item{dend}{A \code{stats::dendrogram} object.}
  \item{facing}{Facing of the dendrogram.}
  \item{gp}{Graphics parameters of the dendrogram edges.}
  \item{track_index}{Index of the track. }

}
\details{
Note the dendrogram can be rendered with the dendextend package.
}
\examples{
k = 500
dend = as.dendrogram(hclust(dist(runif(k))))
spiral_initialize(xlim = c(0, k), start = 360, end = 360*3)
spiral_track(height = 0.8, background_gp = gpar(fill = "#EEEEEE", col = NA))
spiral_dendrogram(dend, facing = "inside")
}
