phyDataMatch<-function(phy, data, saveMismatches=FALSE){
#author: B. Banbury
#builds upon name.check in geiger, and saves any misMatches
	a<-name.check(phy, data)
	if (a[1] == "OK") {
		cat("Phylogeny and character matrix are in agreement")
	}
	else {
		cat("phylogeny and character matrix are mismatched:\n")
		return (a)
		
		if (saveMismatches) {
				if (length(a$Tree.not.data) > 0) {
					b<-a$Tree.not.data
					write(b, file="Tree.not.data.txt")
				}
				if (length(a$Data.not.tree) > 0) {
					c<-a$Data.not.tree
					write(c, file="Data.not.tree.txt")
				}
		}
	}	
}