#' Available resource citekey browser
brwsck<-function(){
	citekeys
	if("menu"%in%citekeys){
		cat("\nEnter numbers or names separated by spaces, a series x:y, or \"all\":\n",paste(1:nrow(vd),vd$ck))
		citekeys<-type.convert(unlist(strsplit(readLines(n=1)," ")),as.is=T)
		if(grepl(":",citekeys)) {citekeys<-type.convert(unlist(strsplit(citekeys,":")));citekeys<-citekeys[1]:citekeys[2]}
		if(citekeys=="all") citekeys<-as.character(1:nrow(vd))
		if(!any(is.na(as.numeric(citekeys)))) citekeys<-vd$ck[as.numeric(citekeys)]
	}
	if(!"menu"%in%citekeys){
		citekeys<-gsub("[{}: ]","",citekeys) #user defined citekeys
		citekeys<-unlist(strsplit(citekeys,","))
	}
	citekeys
}
