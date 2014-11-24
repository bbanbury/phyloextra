#' drop taxa from a dataset
#' 
#' This function will compare rownames of a dataset with tree taxa and drop the non-matches from the dataset.    
#' @param phy A phylogenetic tree in the class "phylo"
#' @param char A matrix or data.frame with meaningful rownames
#' @param toFile If TRUE, will save csv file to working directory
#' @export
#' @return Returns pruned dataset

dropTaxaFromData <- function(char, phy, toFile=TRUE){
  a <- name.check(phy, char)
  if (a[1] == "OK" || length(a$Data.not.tree)==0)
    warning("You do not need to drop taxa from character matrix")
  else if (length(a$Data.not.tree) > 0) {
    pruned.char <- char[-which(rownames(data) %in% a$Data.not.tree),]
    write.csv(pruned.char, file="pruned.char.csv")
  }	
  return(pruned.char)
}
