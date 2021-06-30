\name{phylo_to_dendrogram}
\alias{phylo_to_dendrogram}
\title{
Convert a phylo object to a dendrogram object
}
\description{
Convert a phylo object to a dendrogram object
}
\usage{
phylo_to_dendrogram(obj, log = FALSE)
}
\arguments{

  \item{obj}{A \code{phylo} object.}
  \item{log}{Whether the height of the phylogenetic tree should be log-transformed (log10(x + 1)).}

}
\details{
The motivation is that phylogenetic tree may contain polytomies, which means at a certain node,
there are more than two children branches. Available tools that do the conversion only support binary trees.

The returned \code{dendrogram} object is not in its standard format which means it can not be properly
drawn by the \code{plot.dendrogram} function. However, you can still apply dendextend::\code{\link[dendextend:cutree-methods]{cutree}} to the returned
\code{dendrogram} object with no problem and the dendrogram can be properly drawn with the ComplexHeatmap package.
}
\value{
A \code{dendrogram} object.
}
\examples{
require(ape)
data(bird.families)
d = phylo_to_dendrogram(bird.families)

require(ComplexHeatmap)
grid.dendrogram(d, test = TRUE)
}
