##Set of functions to deal with SNP data
##Barb Banbury  14 Nov 13
##See the example batch code for which functions to run (many of these are internal)


ReturnNucs <- function(NucCode, forSNAPP=FALSE) {
#Function for returning possible bases
#SNAPP missing data needs to be handled differently, so for now just return "-"
  possibilities <- NULL
  if(NucCode == "A" || NucCode == "G" || NucCode == "C" || NucCode == "T" || NucCode == "U")
    possibilities <- NucCode
  if(NucCode == "N" || NucCode == "-" || NucCode == "?") {
    if(forSNAPP)  possibilities <- "-"
    else  possibilities <- c("A", "G", "C", "T", "U")
  }
  if(NucCode == "R")  possibilities <- c("A", "G")
  if(NucCode == "Y")  possibilities <- c("C", "T")
  if(NucCode == "W")  possibilities <- c("A", "T")
  if(NucCode == "S")  possibilities <- c("G", "C")
  if(NucCode == "M")  possibilities <- c("A", "C")
  if(NucCode == "K")  possibilities <- c("G", "T")
  if(NucCode == "B")  possibilities <- c("G", "C", "T")
  if(NucCode == "H")  possibilities <- c("A", "C", "T")
  if(NucCode == "D")  possibilities <- c("A", "G", "T")
  if(NucCode == "V")  possibilities <- c("A", "G", "C")
  return(possibilities)
}


IsVariable <- function(SNP){
# Checks ambiguous sites for certain unique bases
# AAAM will return FALSE (because M can be either A or C--not unique)
# But AAAS qill return T
  var <- FALSE
  bases <- c("A", "C", "G", "T", "U")
  basesInSNP <- bases[which(bases %in% SNP)]
  if(all(SNP == " "))
    return(TRUE)
  if(length(basesInSNP) == 0)  #all "N"
    return(FALSE)
  if(length(basesInSNP) > 1)  #more than one base anyway, so skip checking ambigs
    return(TRUE)
  if(length(basesInSNP) == 1) { #if only one base plus ambigs
    for(i in 2:length(SNP)) {
      var <- c(var, !any(ReturnNucs(SNP[i]) %in% basesInSNP))
    }
  }
  return(any(var))
}

preProcess <- function(SNPdataset) {
#get rid of any underscores in data (weird output from PyRad)
  if(any(which(SNPdataset == "_"))){
    underscores <- which(SNPdataset == "_", arr.ind=T)
    SNPdataset  <- SNPdataset[,-unique(underscores[,2])]
  }
  else return(SNPdataset)
}

SplitSNP <- function(SNPdataset){
#splits a single element into many (ex "ATA" to "A" "T" "A")
  loci <- dim(SNPdataset)[2]
  initialLociLengths <- nchar(SNPdataset[1,])
  splitSNP <- matrix(nrow=dim(SNPdataset)[1], ncol=sum(initialLociLengths)+loci-1)
  for(j in sequence(dim(SNPdataset)[1])) {
print(j) 
   splitSNP[j,] <- strsplit(paste(SNPdataset[j,], collapse=" "), "")[[1]]
  }
  rownames(splitSNP) <- rownames(SNPdataset)
  return(splitSNP)
}

cSNP <- function(splitSNP, KeepVector=NULL, maintainLoci=TRUE){
#this function will concatenate split datasets, and reduce them either by a KeepVector and/or remove spaces indicating loci.  
  if(is.null(KeepVector))
    KeepVector <- rep(TRUE, dim(splitSNP)[2])
  catSNP <- matrix(nrow=dim(splitSNP)[1], ncol=length(which(splitSNP[1,] == " "))+1) 
  rownames(catSNP) <- rownames(splitSNP)
  for(j in sequence(dim(catSNP)[1])) {
    #totally annoying strsplit issue where it cleaves off one space at the end
    string <- strsplit(paste(splitSNP[j,][which(KeepVector=="TRUE")], collapse=""), " ")[[1]]
    if(length(string) +1 == dim(catSNP)[2]){
      string <- c(string, "")
    }
    catSNP[j,] <- string
  }
  if(any(which(catSNP[1,] == "")))
    catSNP <- catSNP[,-which(catSNP[1,] == "")]  #rewrite with lost loci
  if(!maintainLoci)
    catSNP <- apply(catSNP, 1, paste, collapse="")
  return(data.frame(catSNP, stringsAsFactors=FALSE))
}

RemoveInvariantSites <- function(SNPdataset){
  SNPdataset <- preProcess(SNPdataset)
  loci <- dim(SNPdataset)[2]
  initialLociLengths <- nchar(SNPdataset[1,])
  splitdata <- SplitSNP(SNPdataset)
  KeepVector <- apply(splitdata, 2, IsVariable)  
  breaks <- which(splitdata[1,] == " ")
  newSNPdataset <- cSNP(splitdata, KeepVector=KeepVector, maintainLoci=TRUE)
  newLoci <- length(which(newSNPdataset[1,] != ""))  #number of new loci 
  print(paste("removed", loci-newLoci, "of", loci, "loci"))
  return(newSNPdataset)
}

WriteToPhy <- function(data, fileName="file.phy") {
  file <- paste(dim(data)[1], nchar(paste(data[1,], collapse="")))
  write(file, file=fileName)
  write.table(data, file=fileName, append=TRUE, quote=FALSE, col.names=FALSE)  
}


removeOutgroups <- function(SNPdataset, taxon="PH"){
#probably only relevant to our study
  return(SNPdataset[grep(taxon, rownames(SNPdataset)),])
}


GetSpecies <- function(taxa){
  return(unique(gsub("\\d$", "", taxa)))
}

GetNumberOfSitesForLocus <- function(data, locus){
  return(nchar(data[1,])[locus])
}

MissingSpeciesVector <- function(data, SpeciesNames){
#returns T/F vector, TRUE is good to use
  species <- GetSpecies(SpeciesNames)
  for(i in sequence(length(species))) {
    combinedLocus <- paste(data[grep(species[i], names(data))], collapse="")
    if(all(strsplit(combinedLocus, "")[[1]] == "N")) {
      return(FALSE)
    }
  }
  return(TRUE)
}


RemoveMissingSpeciesLoci <- function(data) {
  speciesWithN <- apply(data, 2, MissingSpeciesVector, SpeciesNames=rownames(data))
  print(paste("removed", length(which(speciesWithN == "FALSE")), "of", length(speciesWithN), "loci"))
  data2 <- as.data.frame(data[,speciesWithN])
  rownames(data2) <- rownames(data)  
  return(data2) 
}

IsBinary <- function(SNP){
  if(all(SNP == " "))
    return (TRUE)
  bases <- unique(c(sapply(SNP, ReturnNucs, forSNAPP=TRUE), recursive=T)) #forSNAPP arg
  if(any(bases == "-"))  #remove any missing data
    bases <- bases[-which(bases == "-")]
  if(length(bases) == 2)  return(TRUE)
  else return(FALSE)
}

RemoveNonBinary <- function(data){
  splitdata <- SplitSNP(data)
  binaryVector <- apply(splitdata, 2, IsBinary)
  data2 <- cSNP(splitdata, KeepVector=binaryVector)
  return(data2)
}

CalculateMissingData <- function(data){
  splitdata <- SplitSNP(data)
  MissingSites <- apply(splitdata, 2, function(x) length(which(x == "N")))
  PercentMissing <- MissingSites/dim(splitdata)[1]
  return(PercentMissing)
}

TakeSingleSNPfromEachLocus <- function(data) { 
#this function will take a SNP from each locus by 1)solving for the SNP with the least amount of missing data and 2)taking a random SNP from any ties.
  nuSites <- GetNumberOfSitesForLocus(data)
  singleSNPfromLocus <- matrix(nrow=dim(data)[1], ncol=length(nuSites))
  rownames(singleSNPfromLocus) <- rownames(data)
  whichRandomSites <- NULL
  randomOrMax <- NULL
  for(locus in sequence(length(nuSites))){
    singleLocus <- as.matrix(data[,locus])
    keepSNPs <- which(CalculateMissingData(singleLocus) == min(CalculateMissingData(singleLocus)))
    if(length(keepSNPs) > 1) {  #if there are ties, then take a random SNP
      randomSite <- floor(runif(1, min=1, max=1+length(keepSNPs)))
      whichRandomSites <- c(whichRandomSites, randomSite)
      randomOrMax <- c(randomOrMax, "R")
      singleSNPfromLocus[,locus] <- SplitSNP(singleLocus)[, randomSite]    
    }
    else {  #this is if there is only one SNP with the most data
      whichRandomSites <- c(whichRandomSites, keepSNPs)
      randomOrMax <- c(randomOrMax, "M")
      singleSNPfromLocus[,locus] <- SplitSNP(singleLocus)[, keepSNPs]
    }
  }
  singleSNPfromLocus <- cSNP(singleSNPfromLocus)
  return(list(singleSNPfromLocus, whichRandomSites, randomOrMax))
}


ReturnUniqueBases <- function(SNP){
#function only works with binary data
  bases <- unique(c(sapply(SNP, ReturnNucs, forSNAPP=TRUE), recursive=T)) #forSNAPP arg
  if(any(bases == "-"))
    bases <- bases[-which(bases == "-")]
  return(bases)
}

GetBaseFrequencies <- function(SNP){
#note these may not add up to 1 if there is missing data or heteros
  bases <- ReturnUniqueBases(SNP)
  return(sapply(bases, function(x) length(which(SNP == x))/length(SNP)))
}

ReturnAmbyCode <- function(bases){
  if(all(bases %in% c("A","G")))  return("R")
  if(all(bases %in% c("C","T")))  return("Y")
  if(all(bases %in% c("A","T")))  return("W")
  if(all(bases %in% c("G","C")))  return("S")
  if(all(bases %in% c("A","C")))  return("M")
  if(all(bases %in% c("G","T")))  return("K")
  if(all(bases %in% c("G","C","T")))  return("B")
  if(all(bases %in% c("A","C","T")))  return("H")
  if(all(bases %in% c("A","G","T")))  return("D")
  if(all(bases %in% c("A","G","C")))  return("V")
  if(all(bases %in% c("A","G","C","T")))  return("N")
  if(all(bases %in% c("A","G","C","T","U")))  return("N")
}


TranslateBases <- function(data){
  catData <- cSNP(data, maintainLoci=FALSE)
  splitdata <- SplitSNP(catData)
  splitdata[which(splitdata == "N")] <- "-"  #translate missing data
  splitdata[which(splitdata == "?")] <- "-"
  bases <- apply(splitdata, 2, ReturnUniqueBases)
  baseFrequencies <- apply(splitdata, 2, GetBaseFrequencies)
  for(i in sequence(dim(splitdata)[2])){
    MostFrequentBase <- bases[which(baseFrequencies[,i] == max(baseFrequencies[,i])), i]
    LeastFrequentBase <- bases[which(baseFrequencies[,i] == min(baseFrequencies[,i])), i]
    zeros <- which(splitdata[,i] == MostFrequentBase)
    splitdata[zeros,i] <- 0
    if(any(splitdata[,i] == ReturnAmbyCode(bases[,i]))){
      ones <- which(splitdata[,i] == ReturnAmbyCode(bases[,i]))
      splitdata[ones,i] <- 1
    }
    if(any(which(splitdata[,i] == LeastFrequentBase))) {
      twos <- which(splitdata[,i] == LeastFrequentBase)
      splitdata[twos,i] <- 2
    }    
  }
  return(cSNP(splitdata))
}
















