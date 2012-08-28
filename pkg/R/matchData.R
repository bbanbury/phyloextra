matchData<-function(data1, data2){
#author: B. Banbury 
#Compares two data files for rownames and deletes any that don't match. Saves result as a csv file
	a<-DataNameCheck(data1, data2)
	if (a[1] == "OK") {
		warning("You do not need to drop taxa from character matrix")
	}
	else if (length(a$data1.not.data2) > 0) {
		as.matrix(data1[-which(rownames(data1) %in% a$data1.not.data2),], dimnames=TRUE)->pruned.data1
		write.csv(pruned.data1, file="pruned.data1.csv")
		return("Dropped rows from data1")
	}	
	else if (length(a$data2.not.data1) > 0) {
		as.matrix(data2[-which(rownames(data2) %in% a$data2.not.data1),], dimnames=TRUE)->pruned.data2
		write.csv(pruned.data2, file="pruned.data2.csv")
		return("Dropped rows from data2")
	}	
}

