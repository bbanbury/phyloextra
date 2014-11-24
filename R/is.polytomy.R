is.polytomy<-function(phy, node){
#author: B. Banbury
#Checks if a node is a polytomy
	if (node %in% whichNodesArePolytomies(phy)) return(TRUE)
	else return(FALSE)
}