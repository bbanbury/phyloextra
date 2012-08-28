dropTaxaFromData<-function(phy, char){
#author: B. Banbury
#Deletes taxa from a data file that are not found in the tree; saves as a csv file
	a<-name.check(phy, char)
	if (a[1] == "OK" || length(a$Data.not.tree)==0) {
		warning("You do not need to drop taxa from character matrix")
	}
	else if (length(a$Data.not.tree) > 0) {
		char[-which(rownames(data) %in% a$Data.not.tree),]->pruned.char
		write.csv(pruned.char, file="pruned.char.csv")
	}	
}
