#' Compare Datasets
#' 
#' This function will compare rownames of two datasets   
#' @param data1 A matrix or data.frame with meaningful rownames
#' @param data2 A matrix or data.frame with meaningful rownames
#' @export
#' @return Returns list of data that is not in the other set.

DataNameCheck <- function(data1, data2){
  t1 <- rownames(as.matrix(data1[which(is.na(match(rownames(data1), rownames(data2)))),], dimnames=TRUE))
  t2 <- rownames(as.matrix(data2[which(is.na(match(rownames(data2), rownames(data1)))),], dimnames=TRUE))
  t <- list(t1, t2)
  names(t) <- c("data1.not.data2", "data2.not.data1")
  if (length(t1) == 0 && length(t2) == 0)
    return("OK")
  else
    return(t)
}
