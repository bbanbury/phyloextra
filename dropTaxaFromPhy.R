dropTaxaFromPhy<-function(phy, char, treeFormat=match.arg(arg=priorFn,choices=c("newick", "nexus"),several.ok=FALSE)){
#author: B. Banbury
#Deletes taxa from a tree file that are not found in the data; saves as nexus or newick
	a<-name.check(phy, char)
	if (a[1] == "OK" || length(a$Tree.not.data)==0) {
		warning("You do not need to drop taxa from the phylogeny")
	}
	else if (length(a$Tree.not.data) > 0) {
		drop.tip(phy, a$Tree.not.data)->pruned.phy
		if (treeFormat=="newick"){
			write.tree(pruned.phy, file=paste("pruned_phy.tree"))
		}
		if (treeFormat=="nexus"){
			write.nexus(pruned.phy, file=paste("pruned_phy.nex"))
		}
	}	
}