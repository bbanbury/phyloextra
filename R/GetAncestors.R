#' Get Tree Ancestors
#' 
#' This function will traverse a tree rootward, and return the all ancestors for a node.
#' @param phy A phylogenetic tree in the class "phylo"
#' @param node A numerical node within phy
#' @export
#' @return Returns a node number
#' @seealso \link{whichNodesArePolytomies} \link{node.offspring}
#' @examples
#' tree <- rtree(10)
#' GetAncestors(tree, 9)

GetAncestors <- function(tree, tip) {
  Root <- Ntip(tree) + 1
  is.done <- FALSE
  desc <- tip
  desc.vector <- tip
  while(!is.done){
    a <- which(tree$edge[, 2] == desc)
    b <- tree$edge[a, 1]
    desc.vector <- c(desc.vector, b)
    if(b == Root)
      is.done <- TRUE
    else
      desc <- b
  }
  return(desc.vector)
}