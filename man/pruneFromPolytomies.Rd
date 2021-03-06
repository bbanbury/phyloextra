% Generated by roxygen2 (4.0.2): do not edit by hand
\name{pruneFromPolytomies}
\alias{pruneFromPolytomies}
\title{Randomly Prune Out Polytomies}
\usage{
pruneFromPolytomies(phy)
}
\arguments{
\item{phy}{A phylogenetic tree in the class "phylo"}
}
\value{
Returns pruned tree
}
\description{
This function will randomly remove taxa from a tree with polytomies until all polytomies are gone and the tree is bifurcating. Note: this does not optimize the number of taxa, it simply randomly removes them.  To optimize over a tree, you will need to loop it or use optimizePolytomyPruning.
}
\examples{
tree <- rcoal(10)
tree <- collapseNode(tree, 18)
pruneFromPolytomies(tree)
}
\seealso{
\link{optimizePolytomyPruning}
}

