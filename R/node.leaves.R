#' Node Leaves
#' 
#' This function will return all the tip taxa from a node in the tree
#' @param phy A phylogenetic tree in the class "phylo"
#' @param node A numerical node within phy
#' @export
#' @return Returns a vector of taxa
#' @seealso \link{node.offspring}
#' @examples
#' tree <- rtree(10)
#' node.leaves(tree, 15)

node.leaves <- function(phy, node){
  numTips <- length(phy$tip.label)
  if(node <= numTips) 
    return(phy$tip.label[as.numeric(node)])
  lista <- NULL
  descs <- node.offspring(phy, node)
  for(j in descs){
    if(j <= numTips)
      lista <- c(lista, phy$tip.label[as.numeric(j)])
    else
      lista <- c(lista, node.leaves(phy, j))
  }
  return(lista)
}