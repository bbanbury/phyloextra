#' Find Polytomies
#' 
#' Find all polytomies in a tree
#' @param phy A phylogenetic tree in the class "phylo"
#' @export
#' @return Returns a vector of node numbers in a tree that are polytomies  
#' @seealso \link{is.polytomy} \link{node.offspring}

whichNodesArePolytomies <- function(phy){
  PolytomyList <- NULL
  for (i in max(phy$edge[,1]):min(phy$edge[,1])){
    if(length(which(phy$edge[,1] == i)) > 2)
      PolytomyList <- append(PolytomyList, i)
  }
  return(PolytomyList)
}