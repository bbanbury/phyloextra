collapseNode<-function(phy, node) {
#author: B. Banbury
#function to collapse a node (make a polytomy at a parent node)
	phy$edge.length[which(phy$edge[,2]==node)]<-0
	phy2<-di2multi(phy)
	return(phy2)
}
