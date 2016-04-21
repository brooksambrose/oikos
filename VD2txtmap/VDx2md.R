#' Outline in Markdown from Voice Dream Reader
VDx2md.f<-function(VDx='VDx',VDtxt='VDtxt',out='markdown',citekeys='menu'){
	require(data.table)
	source('VDsource.R')
	vd<-data.table(x=dir(VDx),txt=dir(VDtxt))
	p<-'^.+_([0-9]*)-?([0-9]*)-([^.]+)\\.txt'
	vd[,`:=`(
		ck=sub(p,'\\3',txt)
		,bp=as.integer(sub(p,'\\1',txt))
		,ep=as.integer(sub(p,'\\2',txt))
	)]
	#citekeys<-brwsck()
	setkey(vd,ck)

	ppp<-'^Page ([0-9]+), ([0-9.]+)%: $'
	np<-'^Note: ([#%@!®]*)(.*)'
	for(i in citekeys){
		h<-readLines(paste(VDx,vd[i,x],sep=.Platform$file.sep),warn=F)
		t<-readLines(paste(VDtxt,vd[i,txt],sep=.Platform$file.sep),warn=F)
		tchr<-sum(sapply(t,nchar),length(t),-1) #should each newline be counted?
		md<-data.table(ppl=grep(ppp,h))
		md[,npl:=c(md$ppl[-1]-2,length(h)-1)]
		adj<-!grepl(np,h[md$npl])
		md[,`:=`(
			pp=as.numeric(sub(ppp,'\\1',h[ppl]))
			,pct=as.numeric(sub(ppp,'\\2',h[ppl]))
			,mrk=sub(np,'\\1',h[npl])
			,exc=mapply(function(x,y) paste(h[(x+1):(y-1)],collapse=''),x=ppl,y=npl)
		)]
		md[,op:=vd[i,bp]+pp-1]
		md[adj,mrk:='']
		md[,com:=replace(mapply(function(x,y) paste(sub('^Note: [#%@!®]*','',h[x]),h[(x+1):(y-1)],collapse=''),x=md$npl,y=c(md$ppl[-1],length(h)+1)),which(adj)," ")]
		md[,com:=ifelse(com==' ','',com)]
	writeLines(md[,
								paste(
									gsub('%','>',mrk),' ',exc,', *pp. ',op,'*\n'
									,' (pdf.pp. ',pp,', ',pct,'%)\n\n'
									,'',com,'\n'
									,sep='')
								],con=paste(paste(out,vd[i,ck],sep=.Platform$file.sep),'md',sep='.'))
		}
}

VDx2md.f(citekeys='Roberts2014es')
