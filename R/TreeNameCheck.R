#' Check taxa match in two trees
#' 
#' This function will check that taxonomic names in two trees match
#' @param phy1 A phylogenetic tree in the class "phylo"
#' @param phy2 A phylogenetic tree in the class "phylo"
#' @export
#' @return Returns either "OK" if all taxa match, or it returns a list with taxa from each tree that does not match the other. 
#' @seealso \link{phyDataMatch} \link{matchTreeTaxa}
#' @examples
#' tree1 <- rtree(10)
#' tree2 <- rtree(9)
#' TreeNameCheck(tree1, tree2)

TreeNameCheck <- function(phy1, phy2){
  t1 <- phy1$tip.label[which(is.na(match(phy1$tip.label, phy2$tip.label)))]
  t2 <- phy2$tip.label[which(is.na(match(phy2$tip.label, phy1$tip.label)))]
  t <- list(t1, t2)
  names(t) <- c("phy1.not.phy2", "phy2.not.phy1")
  if (length(t1) ==0 && length(t2) == 0)
    return("OK")
  else
    return(t)
}

