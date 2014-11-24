GetAncestors <- function(tree, tip) {
#function to tree traverse back to the root to get node numbers
  Root <- Ntip(tree) + 1
  is.done <- FALSE
  desc <- tip
  desc.vector <- tip
  while(!is.done){
    a <- which(tree$edge[, 2] == desc)
    b <- tree$edge[a, 1]
    desc.vector <- c(desc.vector, b)
    if(b == Root)
      is.done <- TRUE
    else
      desc <- b
  }
}