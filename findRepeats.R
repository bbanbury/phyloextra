findRepeats<-function(x) {
#author: B. Banbury
#Finds repeated values from a string of numbers (helpful for checking unique seeds on cluster runs)
	x1<-c()
	sort(x)->sx
	for (i in 1: length(x)-1) {
		x2<-c(abs(sx[i])-abs(sx[i+1]))
		x1<-c(x1, x2)
	}
	if ( 0 %in% x1) {
		print(as.matrix(paste("repeat",  sort(x)[which(x1 == 0)])))
	}
	if (!0 %in% x1) {
		print("yahoo all are good!")
	}
} 