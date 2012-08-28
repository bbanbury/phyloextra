pruneFromPolytomies<-function(phy) {
#author: B. Banbury
#randomly prunes taxa out of a polytomy until the tree (phy) is binary (note: the tree does not necessarily retain the most number of taxa--if it randomly prunes a clade) 
	while(!is.binary.tree(phy)) {
		for (i in max(phy$edge[,1]):min(phy$edge[,1])){
		#print(i)
		#print(which(phy$edge[,1]==i))
			if (length(which(phy$edge[,1]==i)) > 2) {
				decNodes<-phy$edge[which(phy$edge[,1]==i),2]
				#print(decNodes)
				nodeToKill<-decNodes[floor(runif(1,min=1, 1+max(length(decNodes))))]
				#print(nodeToKill)
				phy<-drop.tip(phy, node.leaves(phy, nodeToKill))
			}
		}
	}
return(phy)
}