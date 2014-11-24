#' Collapse a Node in a Tree (ie, create a polytomy)
#' 
#' This function collapse a node in a tree to a polytomy  
#' @param phy A phylogenetic tree in the class "phylo"
#' @param node A numerical node in the tree to collapse
#' @export
#' @return Returns pruned phylogeny
#' @examples
#' tree <- rtree(10)
#' collapseNode(tree, 17)

collapseNode <- function(phy, node) {
  phy$edge.length[which(phy$edge[,2] == node)] <- 0
  phy2 <- di2multi(phy)
  return(phy2)
}