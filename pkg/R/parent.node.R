parent.node<-function(phy, node){
#author: B. Banbury
#function for getting the parent node from an offspring node
	r<-which(phy$edge[,2]==node)
	return(phy$edge[r,1])
}
