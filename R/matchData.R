#' Match Datasets
#' 
#' This function will compare rownames of two datasets and write pruned files to the working directory.   
#' @param data1 A matrix or data.frame with meaningful rownames
#' @param data2 A matrix or data.frame with meaningful rownames
#' @param toFile If TRUE, then will write a csv file to the WD
#' @export
#' @return Returns pruned data1

matchData <- function(data1, data2, toFile){
  a<-DataNameCheck(data1, data2)
  if (a[1] == "OK")
    warning("You do not need to drop taxa from character matrix")
    else if (length(a$data1.not.data2) > 0) {
      pruned.data1 <- as.matrix(data1[-which(rownames(data1) %in% a$data1.not.data2),], dimnames=TRUE)
      write.csv(pruned.data1, file="pruned.data1.csv")
      print("Wrote data1 to file")
    }	
    else if (length(a$data2.not.data1) > 0) {
      pruned.data2 <- as.matrix(data2[-which(rownames(data2) %in% a$data2.not.data1),], dimnames=TRUE)
      write.csv(pruned.data2, file="pruned.data2.csv")
      print("Wrote data2 to file")
    }
  return(pruned.data1)	
}

