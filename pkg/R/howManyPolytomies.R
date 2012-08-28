howManyPolytomies<-function(phy) {
#author: B. Banbury
#Returns the number of polytomies in a tree
		nPoly<-0
		for (i in max(phy$edge[,1]):min(phy$edge[,1])){
		#print(i)
		#print(which(phy$edge[,1]==i))
			if (length(which(phy$edge[,1]==i)) > 2) {
				nPoly<-nPoly+1
			}
		}
	return(nPoly)
}

