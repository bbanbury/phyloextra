#' Randomly Prune Out Polytomies
#' 
#' This function will randomly remove taxa from a tree with polytomies until all polytomies are gone and the tree is bifurcating. Note: this does not optimize the number of taxa, it simply randomly removes them.  To optimize over a tree, you will need to loop it or use optimizePolytomyPruning.   
#' @param phy A phylogenetic tree in the class "phylo"
#' @export
#' @return Returns pruned tree
#' @seealso \link{optimizePolytomyPruning}
#' @examples
#' tree <- rcoal(10)
#' tree <- collapseNode(tree, 18)
#' pruneFromPolytomies(tree)

pruneFromPolytomies <- function(phy){
  while(!is.binary.tree(phy)) {
    for (i in max(phy$edge[,1]):min(phy$edge[,1])){
      if (length(which(phy$edge[,1] == i)) > 2) {
        decNodes <- phy$edge[which(phy$edge[,1] == i), 2]
        nodeToKill <- decNodes[floor(runif(1, min=1, 1 + max(length(decNodes))))]
        phy <- drop.tip(phy, node.leaves(phy, nodeToKill))
      }
    }
  }
  return(phy)
}