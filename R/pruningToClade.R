#' Prune Tree to a subclade
#' 
#' This function will prune to a subclade f the tree    
#' @param phy A phylogenetic tree in the class "phylo"
#' @param node An internal node in the tree phy
#' @export
#' @return Returns pruned tree
#' @examples
#' tree <- rcoal(10)
#' pruningToClade(tree, 14)

pruningToClade <- function(phy, node) {
  g <- as.matrix(c(node.leaves(phy, node)))
  rownames(g) <- g[,1]
  d <- name.check(phy, g)
  phy2 <- drop.tip(phy, d[[1]])
  return(phy2)
}