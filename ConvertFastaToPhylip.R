#convert fasta file to phylip
ConvertFastaToPhylip <- function(fasta.file, file.name="phylipFile.phy") {
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
