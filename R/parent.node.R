#' Single Parent Node
#' 
#' This function will traverse a tree rootward, and return the most recent ancestral node
#' @param phy A phylogenetic tree in the class "phylo"
#' @param node A numerical node within phy
#' @export
#' @return Returns a single ancestral node
#' @seealso \link{node.offspring} \link{GetAncestors}
#' @examples
#' tree <- rtree(10)
#' parent.node(tree, 15)

parent.node <- function(phy, node){
  r <- which(phy$edge[,2] == node)
  return(phy$edge[r,1])
}
