node.offspring<-function(phy, node) {
#author: B. Banbury
# function to get offspring nodes from a parent node
	r<-which(phy$edge[,1]==node)
	return(phy$edge[r,2])
}