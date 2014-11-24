#' Convert a FASTA formatted file to Phylip
#' 
#' This function will concatenate a fasta file and write it in a phylip
#' @param fasta.file A fasta formatted file
#' @param file.name A new file name
#' @export

ConvertFastaToPhylip <- function(fasta.file, file.name="phylipFile.phy"){
  phylipFile<-paste(length(fasta.file), length(fasta.file[[1]]))
  write(phylipFile, file="phylipFile.phy")
  for (i in sequence(length(fasta.file))){
    if(length(fasta.file[[i]]) != length(fasta.file[[1]]))
      stop("sequences vary in length")
    phylipFile <- append(phylipFile, attr(fasta.file[[i]], "name"))
    phylipFile <- append(phylipFile, paste(fasta.file[[i]], collapse=""))
    write(phylipFile, file=file.name)
  }
}
