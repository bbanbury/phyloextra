whichNodesArePolytomies<-function(phy) {
#author: B. Banbury
#Returns a vector of nodes in a tree that are polytomies
	PolytomyList<-c()
	for (i in max(phy$edge[,1]):min(phy$edge[,1])){
		if (length(which(phy$edge[,1]==i)) > 2) {
			PolytomyList<-append(PolytomyList, i)
		}
	}
	return(PolytomyList)
}