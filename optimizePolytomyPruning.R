optimizePolytomyPruning<-function(phy,maxTries=100,stopAtNumberSameHits=5) {
#author: B. Banbury
#This function uses pruneFromPolytomies to randomly prune branches out of a tree with polytomies and optimize the number of taxa kept. 
	bestTree<-pruneFromPolytomies(phy)
	bestNTip<-Ntip(bestTree)
	numberSameHits<-0
	for (attempt in sequence(maxTries)) {
		print(paste("now in attempt ",attempt, "--Tree has ", Ntip(bestTree), "taxa."))
		newTree<-pruneFromPolytomies(phy)
		if (Ntip(newTree)==bestNTip) {
			if (dist.topo(newTree,bestTree)==0) {
				numberSameHits<-numberSameHits+1
				print(paste("NumberSameHits increased to ",numberSameHits))
				if(numberSameHits==stopAtNumberSameHits) {
					return(bestTree)
				}
			}
		}
		if (Ntip(newTree)>bestNTip) {
			bestTree<-newTree
			bestNTip<-Ntip(bestTree)	
			numberSameHits<-0
		}		
	}
	return(bestTree)
}