matchTreeTaxa<-function(phy1, phy2, treeFormat=match.arg(arg=priorFn,choices=c("newick", "nexus"),several.ok=FALSE)) {
#author: B. Banbury
#Takes two phylogenies and compares taxa; drops non matching taxa from either tree
	a<-TreeNameCheck(phy1, phy2)
	if (a[1] == "OK") {
		warning("You do not need to drop taxa from either phylogeny")
	}
	else if (length(a$phy1.not.phy2) > 0) {
		drop.tip(phy1, a$phy1.not.phy2)->pruned.phy1
		if (treeFormat=="newick"){
			write.tree(pruned.phy1, file=paste("pruned_phy1.tree"))
		}
		if (treeFormat=="nexus"){
			write.nexus(pruned.phy1, file=paste("pruned_phy1.nex"))
		}
		return("Dropped tips from phy1")
	}
	else if (length(a$phy2.not.phy1) > 0) {
		drop.tip(phy2, a$phy2.not.phy1)->pruned.phy2
		if (treeFormat=="newick"){
			write.tree(pruned.phy2, file=paste("pruned_phy2.tree"))
		}
		if (treeFormat=="nexus"){
			write.nexus(pruned.phy2, file=paste("pruned_phy2.nex"))
		}
		return("Dropped tips from phy2")
	}		
}
