#' Drop taxa From Phy
#' 
#' This function will check that taxonomic names in a tree and a dataset and if they do not, it will write the pruned trees to file.  
#' @param phy A phylogenetic tree in the class "phylo"
#' @param char A character matrix with rownames as taxa
#' @param toFile If TURE, it will save pruned tree in working directory
#' @param treeFormat either "newick" or "nexus" depending on how you want the tree written
#' @export
#' @return Returns pruned phylogeny 
#' @seealso \link{matchTreeTaxa} \link{TreeNameCheck}
#' @examples
#' tree <- rtree(10)
#' chars <- matrix(runif(18), ncol=2)
#' rownames(chars) <- paste0("t", 1:9)
#' dropTaxaFromPhy(tree, chars, toFile=FALSE)

dropTaxaFromPhy <- function(phy, char, toFile=TRUE, treeFormat=match.arg(arg=priorFn,choices=c("newick", "nexus"),several.ok=FALSE)){
  a <- name.check(phy, char)
  if (a[1] == "OK" || length(a$tree_not_data) == 0)
    warning("You do not need to drop taxa from the phylogeny")
  else if (length(a$tree_not_data) > 0) {
    pruned.phy <- drop.tip(phy, a$tree_not_data)
    if(toFile){
      if (treeFormat=="newick")
        write.tree(pruned.phy, file=paste("pruned_phy.tree"))
      if (treeFormat=="nexus")
        write.nexus(pruned.phy, file=paste("pruned_phy.nex"))
    }
  }
  return(pruned.phy)	
}