#' Check taxa match in two trees
#' 
#' This function will check that taxonomic names in two trees match and if they do not, it will write the pruned trees to file.  
#' @param phy1 A phylogenetic tree in the class "phylo"
#' @param phy2 A phylogenetic tree in the class "phylo"
#' @param toFile If TRUE, it will save pruned tree files in the working directory
#' @param treeFormat either "newick" or "nexus" depending on how you want the tree written
#' @export
#' @return Returns either "OK" if all taxa match, or it returns a list with taxa from each tree that does not match the other. 
#' @seealso \link{phyDataMatch} \link{TreeNameCheck}
#' @examples
#' tree1 <- rtree(10)
#' tree2 <- rtree(9)
#' matchTreeTaxa(tree1, tree2, toFile=FALSE)

matchTreeTaxa <- function(phy1, phy2, toFile=TRUE, treeFormat=match.arg(arg=priorFn,choices=c("newick", "nexus"), several.ok=FALSE)){
  a <- TreeNameCheck(phy1, phy2)
  if(a[1] == "OK")
    warning("You do not need to drop taxa from either phylogeny")
  else if (length(a$phy1.not.phy2) > 0){
    pruned.phy1 <- drop.tip(phy1, a$phy1.not.phy2)
    if(toFile){
      if (treeFormat=="newick")
        write.tree(pruned.phy1, file=paste("pruned_phy1.tree"))
      if (treeFormat=="nexus")
        write.nexus(pruned.phy1, file=paste("pruned_phy1.nex"))
    print("Saved pruned_phy1")
    }
  }
  else if (length(a$phy2.not.phy1) > 0){
    pruned.phy2 <- drop.tip(phy2, a$phy2.not.phy1)
    if(toFile){
      if (treeFormat=="newick")
        write.tree(pruned.phy2, file=paste("pruned_phy2.tree"))
      if (treeFormat=="nexus")
        write.nexus(pruned.phy2, file=paste("pruned_phy2.nex"))
    print("Saved pruned_phy2")
    }
  }
  return(a)
}
