check.seed<-function(x) {
	x1<-c()
	sort(x)->sx
	for (i in 1: length(x)-1) {
		x2<-c(abs(sx[i])-abs(sx[i+1]))
		x1<-c(x1, x2)
	}
	if ( 0 %in% x1) {
		print(as.matrix(paste("repeat seed.number",  sort(x)[which(x1 == 0)])))
	}
	if (!0 %in% x1) {
		print("yahoo all are good!")
	}
} 




setwd("")
system("cat *Rout | grep seed", intern=TRUE)->l
as.matrix(l)->m

for (i in 1: length(m)){
	result[i]<-as.numeric(paste(grep(pattern="\\d+",x=(strsplit(m[i],"")[[1]]),perl=TRUE,value=TRUE),sep="",collapse=""))
	
}


check.seed(result)