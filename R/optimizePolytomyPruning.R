#' Optimize Pruning Out Polytomies
#' 
#' This function will randomly remove taxa and keep the tree with the most taxa.
#' @param phy A phylogenetic tree in the class "phylo"
#' @param maxTries A number of tries to beat the best tree (ie, tree with the most taxa)
#' @param stopAtNumberSameHits A kill number.  If it hits this value, then it terminates the search. 
#' @export
#' @return Returns bifurcating tree with the most number of taxa
#' @seealso \link{pruneFromPolytomies}
#' @examples
#' tree <- rcoal(10)
#' tree <- collapseNode(tree, 18)
#' NotTheBest <- pruneFromPolytomies(tree)
#' MaybeBetter <- optimizePolytomyPruning(tree)

optimizePolytomyPruning <- function(phy, maxTries=100, stopAtNumberSameHits=5){
  bestTree <- pruneFromPolytomies(phy)
  bestNTip <- Ntip(bestTree)
  numberSameHits <- 0
  for(attempt in sequence(maxTries)){
    print(paste("now in attempt ",attempt, "--Tree has ", Ntip(bestTree), "taxa."))
    newTree <- pruneFromPolytomies(phy)
    if(Ntip(newTree) == bestNTip){
      if (dist.topo(newTree, bestTree) == 0){
        numberSameHits <- numberSameHits + 1
        print(paste("NumberSameHits increased to ", numberSameHits))
        if(numberSameHits == stopAtNumberSameHits)
          return(bestTree)
      }
    }
    if(Ntip(newTree) > bestNTip){
      bestTree <- newTree
      bestNTip <- Ntip(bestTree)	
      numberSameHits <- 0
    }
  }
  return(bestTree)
}