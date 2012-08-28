node.leaves<-function(phy, node) {
#author: B. Banbury
#Get all the taxa from a parent node
	n<-length(phy$tip.label)
	if(node<= n) return(phy$tip.label[as.numeric(node)])
	
	l<-character();
	d<-node.offspring(phy, node);
	for(j in d) {
		if(j <= n) l<-c(l, phy$tip.label[as.numeric(j)])
		else l<-c(l, node.leaves(phy, j));
	}
	return(l);
}