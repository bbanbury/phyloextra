#' Find Repeats
#' 
#' This function will find where repeats exist in a vector   
#' @param x A vector of numeric data
#' @export
#' @return Returns pruned data1
#' @examples
#' v <- c(1,2,3,3,3,4,5)
#' findRepeats(v)

findRepeats <- function(x){
  x1 <- NULL
  sx <- sort(x)
  for(i in sequence(length(x)-1)){
    x2 <- c(abs(sx[i]) - abs(sx[i+1]))
    x1 <- c(x1, x2)
  }
  if(0 %in% x1)
    print(as.matrix(paste("repeat",  sort(x)[which(x1 == 0)])))
  if (!0 %in% x1)
    print("yahoo all are good!")
} 