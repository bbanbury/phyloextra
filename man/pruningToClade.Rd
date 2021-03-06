% Generated by roxygen2 (4.0.2): do not edit by hand
\name{pruningToClade}
\alias{pruningToClade}
\title{Prune Tree to a subclade}
\usage{
pruningToClade(phy, node)
}
\arguments{
\item{phy}{A phylogenetic tree in the class "phylo"}

\item{node}{An internal node in the tree phy}
}
\value{
Returns pruned tree
}
\description{
This function will prune to a subclade f the tree
}
\examples{
tree <- rcoal(10)
pruningToClade(tree, 14)
}

