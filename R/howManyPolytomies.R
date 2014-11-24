#' How many Polytomies in a tree
#' 
#' This function will a single value for the number of polytomies in a tree
#' @param phy A phylogenetic tree in the class "phylo"
#' @export
#' @return Returns single value for the number of polytomies in a tree  
#' @seealso \link{whichNodesArePolytomies} \link{node.offspring}
#' @examples
#' tree <- rtree(10)
#' howManyPolytomies(tree)

howManyPolytomies <- function(phy){
  nPoly <- 0
  for (i in max(phy$edge[,1]):min(phy$edge[,1])){
    if (length(which(phy$edge[,1] == i)) > 2)
      nPoly <- nPoly + 1
  }
  return(nPoly)
}

