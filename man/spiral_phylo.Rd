\name{spiral_phylo}
\alias{spiral_phylo}
\title{
Draw phylogenetic tree
}
\description{
Draw phylogenetic tree
}
\usage{
spiral_phylo(obj, gp = gpar(), log = FALSE, reverse = FALSE,
    group = NULL, group_col = NULL, track_index = current_track_index())
}
\arguments{

  \item{obj}{A \code{phylo} object.}
  \item{gp}{Graphics parameters of the tree edges.}
  \item{log}{Whether the height of the tree should be log-transformed (log10(x + 1))?}
  \item{reverse}{Whether the tree should be reversed?}
  \item{group}{A categorical variable for splitting the tree.}
  \item{group_col}{A named vector which contains group colors.}
  \item{track_index}{Index of the track. }

}
\value{
Height of the phylogenetic tree.
}
\examples{
require(ape)
data(bird.families)
n = length(bird.families$tip.label)
spiral_initialize(xlim = c(0, n), start = 360, end = 360*3)
spiral_track(height = 0.8)
spiral_phylo(bird.families)
}
