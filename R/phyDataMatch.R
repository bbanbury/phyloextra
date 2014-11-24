#' Phy-Data Match
#' 
#' This function will compare a dataset and a phylogenetic tree and compare taxa.
#' @param phy phylogenetic tree in the class "phylo" 
#' @param data A matrix or data.frame with rownames as taxa
#' @param saveMismatches Boolean; to save the mismatched data to a file
#' @export

phyDataMatch <- function(phy, data, saveMismatches=FALSE){
  a <- name.check(phy, data)
  if (a[1] == "OK") {
    return("Phylogeny and character matrix are in agreement")
  }
  else{
    cat("phylogeny and character matrix are mismatched:\n")
    if(saveMismatches){
      if(length(a$Tree.not.data) > 0){
        b <- a$Tree.not.data
        write(b, file="Tree.not.data.txt")
      }
      if(length(a$Data.not.tree) > 0){
        c <- a$Data.not.tree
        write(c, file="Data.not.tree.txt")
      }
    }
  }	
  return(a)
}