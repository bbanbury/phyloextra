pruningToClade<-function(phy, node) {
#author: B. Banbury
#function for pruning a tree to a single clade
	g<-as.matrix(c(node.leaves(phy,node)))
	rownames(g)<-g[,1]
	d<-name.check(phy, g)
	phy2<-drop.tip(phy, d[[1]])
	return(phy2)
}