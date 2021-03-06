% Generated by roxygen2 (4.0.2): do not edit by hand
\name{TreeNameCheck}
\alias{TreeNameCheck}
\title{Check taxa match in two trees}
\usage{
TreeNameCheck(phy1, phy2)
}
\arguments{
\item{phy1}{A phylogenetic tree in the class "phylo"}

\item{phy2}{A phylogenetic tree in the class "phylo"}
}
\value{
Returns either "OK" if all taxa match, or it returns a list with taxa from each tree that does not match the other.
}
\description{
This function will check that taxonomic names in two trees match
}
\examples{
tree1 <- rtree(10)
tree2 <- rtree(9)
TreeNameCheck(tree1, tree2)
}
\seealso{
\link{phyDataMatch} \link{matchTreeTaxa}
}

