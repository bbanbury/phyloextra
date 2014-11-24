TreeNameCheck<-function(phy1, phy2) {
#author: B. Banbury
#Compares taxa from two trees
	t1<-phy1$tip.label[which(is.na(match(phy1$tip.label, phy2$tip.label)))]
	t2<-phy2$tip.label[which(is.na(match(phy2$tip.label, phy1$tip.label)))]
	t<-list(t1, t2)
	names(t)<-c("phy1.not.phy2", "phy2.not.phy1")
	if (length(t1) ==0 && length(t2) == 0) {
		return("OK")
	}
	else {
		return(t)
	}
}

