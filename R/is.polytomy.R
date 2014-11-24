#' Is a Polytomy
#' 
#' This function will return if a node in a tree is a polytomy (has more than two offspring)
#' @param phy A phylogenetic tree in the class "phylo"
#' @param node A numerical node within phy
#' @export
#' @return Returns boolean response; TRUE for is a polytomy  
#' @seealso \link{whichNodesArePolytomies} \link{node.offspring}
#' @examples
#' tree <- rtree(10)
#' is.polytomy(tree, 11)

is.polytomy <- function(phy, node){
  if (node %in% whichNodesArePolytomies(phy)) 
    return(TRUE)
  else 
    return(FALSE)
}