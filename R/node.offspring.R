#' node offspring
#' 
#' Find offspring nodes
#' @param phy A phylogenetic tree in the class "phylo"
#' @param node A numerical node within phy
#' @export
#' @return Returns offspring nodes from a tree.  If it is bifurcating, then this function will return two nodes.  This function does not return tip values, for that see node.leaves()  
#' @seealso \link{node.leaves} \link{collapseNode}
#' @examples
#' tree <- rtree(10)
#' node.offspring(tree, 11)

node.offspring <- function(phy, node){
	r<-which(phy$edge[,1] == node)
	return(phy$edge[r,2])
}