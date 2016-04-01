VDtextmap<-function( #from http://stackoverflow.com/questions/9802680/importing-files-with-extension-sqlite-into-r
    vdl=stop("vdl: specify complete \"Voice Dream Library\" folder",call.=F)
    ,outdir=sub("[^/]+","",vdl)
    ,citekeys=stop("citekeys: specify Magic Manuscript citekeys, or use \"menu\" to see all options",call.=F)
	,debug=F
)
{
	require('RSQLite')
	require('fpc')
	require('DBI')
	system(paste("sqlite3 \"",vdl,"/Voice_Dream_Reader.sqlite\" \"VACUUM;\"",sep="")) # checkpoint WAL file and force VoiceDream.sqlite to update
	con <- 	dbConnect(RSQLite::SQLite(), dbname=paste(vdl,"/Voice_Dream_Reader.sqlite",sep="")) ## connect to db
	tables <- dbListTables(con) ## list all tables
	vddb <- vector("list", length=length(tables))
	for (i in seq(along=tables)) vddb[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep="")) ## create a data.frame for each table
	names(vddb)<-tables
	notes<-vddb$ZMARK[,c("ZARTICLE","ZMARKTYPE","ZMARKNAME","ZLENGTH","ZLOCATION")] ##just notes
	pages<-notes[notes$ZMARKTYPE=="Page",]
	pages<-split(pages,f=pages$ZARTICLE)
	notes<-notes[notes$ZMARKTYPE=="Highlight",]
	notes<-split(notes,f=notes$ZARTICLE)

	s<-vddb$ZARTICLE$Z_PK%in%names(notes)&grepl("^.+-(.+[0-9]{4}...?)$", vddb$ZARTICLE$ZTITLE) ##begin plot data
	plot<-data.frame(vddb$ZARTICLE[s,c("Z_PK","ZTITLE")],vddb$ZARTICLETEXT[s,"ZTEXT"])
	row.names(plot)<-make.unique(sub("^.+-(.+[0-9]{4}...?)$","\\1",plot$ZTITLE))
	alldat<-intersect(intersect(plot[,1],names(notes)),names(pages))
	plot<-plot[plot[,1]%in%alldat,]
	notes<-notes[names(notes)%in%plot[,1]]
	names(notes)<-row.names(plot)
	pages<-pages[names(pages)%in%plot[,1]]
	names(pages)<-row.names(plot)

	for(j in names(pages)) {
		pages[[j]]<-pages[[j]][order(as.numeric(pages[[j]]$ZMARKNAME)),]
		notes[[j]]<-notes[[j]][order(notes[[j]]$ZLOCATION),]
	}

	if(!debug){
		citekeys
		if("menu"%in%citekeys){
		  cat("\nEnter numbers or names separated by spaces, a series x:y, or \"all\":\n",paste(1:length(names(notes)),names(notes)))
		  citekeys<-type.convert(unlist(strsplit(readLines(n=1)," ")),as.is=T)
		  if(grepl(":",citekeys)) {citekeys<-type.convert(unlist(strsplit(citekeys,":")));citekeys<-citekeys[1]:citekeys[2]}
		  if(citekeys=="all") citekeys<-as.character(1:length(notes))
		  if(!any(is.na(as.numeric(citekeys)))) citekeys<-names(notes)[as.numeric(citekeys)]
		}
		if(!"menu"%in%citekeys){
		  citekeys<-gsub("[{}: ]","",citekeys) #user defined citekeys
		  citekeys<-unlist(strsplit(citekeys,","))
		}
	}

	titindex<-sub(".+-","",vddb$ZARTICLE$ZTITLE)
	titref<-list()
	for(i in citekeys) titref[[i]]<-max(which(titindex==i)) #only takes the last version; should update for using multiple, time stamped versions
	titref<-unlist(titref)

	orange1<-hsv(h=.075,s=.5)
	orange2<-hsv(h=.075,s=.15)
	red1<-hsv(h=1,s=.5)
	red2<-hsv(h=1,s=.15)

	#  "#" #Heading 1
	#  ,"##" #Heading 2
	#  ,"###" #Heading 3
	#  ,"%" #Abstract, Figure or Table title
	#  ,"%%" #Term, formula, proposition, or hypothesis
	#  ,"%%%"  #Unformatted list, block quote, table or figure contents
	#  ,"!" #This is interesting
	#  ,"!!" #This is very interesting
	#  ,"!!!"  #This is very very interesting

	mystrwrap<-function(chstr){
	  wrap<-list()
	  breaks<-list()
	  for(j in 1:length(chstr)) {
	  words<-NULL
	  words<-unlist(strsplit(chstr[[j]]," "))
	  breaks[[j]]<-suppressWarnings(c(0,which(!!c(diff(cumsum(sapply(words,strwidth,family=family,units="user",font=1))%/%(rhs)),1))))
	  wrap[[j]]<-rep("",length(breaks[[j]])-1)
	  for(k in 2:length(breaks[[j]])) wrap[[j]][k-1]<-paste(words[(breaks[[j]][k-1]+1):breaks[[j]][k]],collapse=" ")
	  }
	  wrap
	}
#PLOTS

#debug
for(i in citekeys){
	cat("\n",i,sep="")

	####### Identify file to use as text source, now supplied by VD database
	ftext<-list.files(vdl,recursive=T,full.names=T)
	pdf<-ftext[grepl(paste(i,"pdf",sep="."),ftext)]
	ftext<-sub("[^/]+$","VoiceDreamBodyText.txt",pdf)
	if(length(ftext)>1) {warning("Multiple txt source files detected, taking first.",immediate.=T);print(ftext);ftext<-ftext[1]}
	tit<-gsub("_"," ",sub("^.+/[^_]*_[^_]*_(.+)_([0-9]+)-([0-9]+)-[^-]+$","\\1",pdf))
	bp<-as.numeric(sub("^.*_([0-9]+)-([0-9]+)-[^-]*$","\\1",pdf))
	ep<-as.numeric(sub("^.*_([0-9]+)-([0-9]+)-[^-]*$","\\2",pdf))

   	####### Load full text
   	ftext<-readLines(ftext,warn=F)
	ftext<-paste(ftext,collapse="\n")

	####### Match pages
	if(length(pages[[i]]$ZLOCATION)-length(bp:ep)==1) {bpep<-c(0,bp:ep)} else {bpep<-bp:ep}
	pp<-data.frame(page=bpep,zloc=pages[[i]]$ZLOCATION+1,span=NA)

	###### Begin graphical data
	grph<-notes[[i]][,!names(notes[[i]])%in%c("ZARTICLE","ZMARKTYPE")]
	grph$ZMARKNAME[is.na(grph$ZMARKNAME)]<-""

	### adjust location to match R
	grph$ZLOCATION<-grph$ZLOCATION+1
	grph$ZLENGTH<-grph$ZLENGTH-1

	### identify structural comments
	heads<-grepl("^#",grph$ZMARKNAME)&!grepl("^##",grph$ZMARKNAME)
	subheads<-grepl("^##",grph$ZMARKNAME)&!grepl("^###+",grph$ZMARKNAME)
	greens<-(grepl("^%",grph$ZMARKNAME)&!grepl("^%%%+",grph$ZMARKNAME))|grepl("^###+",grph$ZMARKNAME)
	subsubheads<-grepl("^###+",grph$ZMARKNAME)
	shi<-subheads[heads|subheads|greens]
	hi<-heads[heads|subheads|greens]
	gi<-greens[heads|subheads|greens]
	gt<-grepl("^%%%+",grph$ZMARKNAME)
	oe<-!(heads|subheads|greens|gt) #only excerpts

	### calculate span locations
	tlen<-vddb$ZARTICLE$ZTEXTLENGTH[titref[i]]
	spans<-c(1,grph$ZLOCATION[heads],tlen)
	spans<-cbind(spans[-length(spans)],spans[-1]-1)
	d<-dim(spans)
	spans[d[1],d[2]]<-spans[d[1],d[2]]+1
	spans<-cbind(spans,apply(spans,1,diff)+1)

	### graphical locations of spans
	grph$x1<-NA
	grph$x2<-NA
	grph$y<-NA
	grph$span<-NA
	for(j in 1:dim(grph)[1]){
	grph$span[j]<-which(grph$ZLOCATION[j]>=spans[,1]&grph$ZLOCATION[j]<=spans[,2])
	grph$x1[j]<-grph$ZLOCATION[j]-sum(spans[1:(grph$span[j]-1),3])
	grph$x2[j]<-grph$x1[j]+grph$ZLENGTH[j]-1
	}

	### housekeeping
	spans<-spans[-1,] #You have to mark the title as a level 1 heading for this to work, cuts the detritous above the title out as a span, if title is plotted significantly to the right then there is a lot of detritous
	grph$span<-grph$span-1
	spans[1,1]<-1

	### match page boundary locations to spans
	for(j in 1:length(pp$zloc)) pp$span[j]<-which(pp$zloc[j]>=spans[,1]&pp$zloc[j]<=spans[,2])
	pp<-data.frame(pp,x=pp$zloc-(spans[pp$span,1]-1))


	####### Begin plots for "TOC" spans
	segs<-list()
	for(j in which(heads)){
		grph$y[j]<-which(grph$ZLOCATION[j]>=spans[,1]&grph$ZLOCATION[j]<=spans[,2])
		grph$x2[j]<-grph$ZLOCATION[j]+grph$ZLENGTH[j]-1
		segs[[j]]<-cbind(c(grph$ZLOCATION[j],grph$x2[j]),rep(grph$y[j],2))
	}

	### color coding by type of comment
	grph$xcit<-attr(regexpr("^!+", grph$ZMARKNAME),"match.length")
	grph$xcit[grph$xcit==-1]<-0
	grph$xcit[grph$xcit>3]<-3
	grph$greens<-attr(regexpr("^%+", grph$ZMARKNAME),"match.length")
	grph$greens<-grph$greens+3
	grph$greens[grph$greens>6]<-6
	grph$greens[grph$greens==2]<-0
	grph$col<-grph$xcit+grph$greens+1
    grph$col1<-c(rainbow(4,start=.7,end=1,s=.4),hsv(h=5:3/16,s=.4))[grph$col]
    grph$col2<-c(rainbow(4,start=.7,end=1,s=.05),hsv(h=5:3/16,s=.1))[grph$col]
    grph$col1[heads|subheads]<-NA
    grph$col2[heads|subheads]<-NA
	grph$col1[subsubheads]<-orange1

	####### Begin graph excerpts
    x<-grph[,c("ZLOCATION","ZLENGTH")]
    x[,2]<-x[,2]+x[,1]
    exc<-list()
    for(j in 1:dim(x)[1]) exc[[j]]<-substr(ftext,x[j,1],x[j,2])
    grph$exc<-gsub("\r|\n|\t","",exc)
    rm(exc)

	### graphing green and orange sticks on "TOC"
    grph$subspan<-sapply(apply(sapply(grph$ZLOCATION,">=",grph$ZLOCATION[heads|subheads]),2,which),max)
    t<-gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(grph$exc[heads|subheads|greens]), perl=TRUE)
    att<-rep(1,sum(heads,subheads,greens))
    att[!gi]<-as.numeric(table(grph$subspan)) # not sure what I was doing here, but seems to be simply counting comments in each subsection
    t<-data.frame(t=t # graph object for TOC
		,x1=grph$x1[heads|subheads|greens] # horizontal position or left side of the label
		,x2=grph$x2[heads|subheads|greens]
		,y=NA # vertical position
		,pos=4 # labels should be to the right of point location by default
		,off=as.numeric(!heads[heads|subheads|greens])-.8 # different offsets for label type
		,font=as.numeric(!heads[heads|subheads|greens])+2 # bold, italic, etc.
		,seg=diff(c(grph$ZLOCATION[heads|subheads|greens],spans[dim(spans)[1],2]))
		,att=att)
    rm(att)
	t$rsl<-#the right side of the label
	t$font[subheads[heads|subheads|greens]]<-1
	####### Begin graphical device to measure paramaters
    rhs<-max(spans[,3])
    family<-"Times"
    pdf(paste("txtmap_",i,".pdf",sep=""),width=8.5,height=11)
    plot.new()
    par(family=family,font=1
      ,mar = c(b=1,l=.75,r=.75,t=1)
      ,oma = rep(.5, 4)
    ,yaxs="i")
    plot.window(xlim=c(1,rhs), ylim=c(1,0))
	axis(1)
	axis(2)

	### trying to discover which titles have space above them to be "squished" to take up less room vertically

    t$strw<-strwidth(t$t,font=t$font,family=family)
	t$rsl<-t$x1+t$strw #the right side of the label
	t$pos[t$x1+t$strw>=rhs&t$x1>rhs/2]<-2 # if labels run against the right hand side flip from right (4) to left (2) if stem is more than halfway across page
	t$cex<-NA
	for(j in 1:dim(t)[1]) {
		if(t$pos[j]==4) t$cex[j]<-seq(.01,1,.01)[max(which(mapply(strwidth,s=t$t[j],font=t$font[j],family=family,cex=seq(.01,1,.01))<rhs-t$x1[j]))]
		if(t$pos[j]==2) t$cex[j]<-seq(.01,1,.01)[max(which(mapply(strwidth,s=t$t[j],font=t$font[j],family=family,cex=seq(.01,1,.01))<t$x1[j]))]
		t$strw[j]<-strwidth(t$t[j],font=t$font[j],family=family,cex=t$cex[j])
	}# calculate cex for labels that still are too long to fit, and update strw
	t$cex[t$cex>.75&gi]<-.75

	# default max of 10 clusters here, could be an argument...
	pk<-c(1,diff(as.numeric(factor(pamk(grph$ZLOCATION[subheads|greens],krange=2:ifelse(sum(subheads|greens)>10,10,sum(subheads|greens)-1))$pamobject$clustering+grph$span[subheads|greens]))))

	wh<-which(heads[heads|subheads|greens])
	whn<-which(!heads[heads|subheads|greens])
    t$squish<-t$seg>t$strw&(!heads[heads|subheads|greens])&!(c(0,diff(!heads[heads|subheads|greens]))==1)
    for(j in 1:dim(t)[1]) t$y[j]<-j+sum(heads[heads|subheads|greens][1:j])
	for(j in 1:length(whn)) {
		whm<-max(wh[wh<whn[j]])
		o<-t$y[whn[j]]
		t$y[whn[j]]<-t$y[whn[j]]-(whn[j]-whm-1) # move to just below the baseline
		k<-which(t$y[whn[j]]==t$y[1:(whn[j]-1)])
		if(!length(k)) next
		k<-k[which.max(t$rsl[k])] # of all labels at the same y, which has the furthest right hand side
		while(t$y[whn[j]]<o&(t$rsl[k]>t$x1[whn[j]])|(t$pos[whn[j]]==2&t$rsl[k]>t$x1[whn[j]]-t$strw[whn[j]])) {
			t$y[whn[j]]<-t$y[whn[j]]+1
			k<-which(t$y[whn[j]]==t$y[1:(whn[j]-1)])
			if(!length(k)) break
			k<-k[which.max(t$rsl[k])] # of all labels at the same y, which has the furthest right hand side
			} # if label crosses another label before it's on the same line, move it up once but stop when it gets back to its starting point
	}
	for(j in wh[-1]) t$y[j:dim(t)[1]]<-t$y[j:dim(t)[1]]-(t$y[j]-(max(t$y[1:(j-1)])+3)) # eliminate excess space from squishing


	#####
    grph$exc<-cbind(mystrwrap(grph$exc))
    grph$exc[heads|subheads|greens]<-sapply(grph$exc[heads|subheads|greens],paste,collapse=" ")
    grph$annchar<-nchar(grph$ZMARKNAME)
    grph$ZMARKNAME<-cbind(mystrwrap(grph$ZMARKNAME))
    grph$excl<-sapply(grph$exc,length)
    grph$annl<-sapply(grph$ZMARKNAME,length)


    lintran<-function(x,s1=c(0,1),s2=c(0,1)) {a=diff(s2)/diff(s1);b=s2[1]-a*s1[1];return(a*x+b)} # linear transformation of scales http://stackoverflow.com/questions/1471370/normalizing-from-0-5-1-to-0-1

	### colors and shading for density of attention and excitement of attention
	t$dens<-NA
	grph$gtr<-0
	grph$gtr[!oe]<-grph$ZLENGTH[!oe]
	gs<-aggregate(grph$gtr,by=grph["span"],FUN=sum)$x #adjustment for required lengths
	gss<-aggregate(grph$gtr,by=grph["subspan"],FUN=sum)$x #adjustment for required lengths
	t$dens[!gi]<-(aggregate(grph$ZLENGTH,by=grph["subspan"],FUN=sum)$x-gss)/(diff(c(grph$ZLOCATION[heads|subheads],tlen)-grph$ZLOCATION[heads|subheads][1])-gss+1) #density of subspans
	t$sdens[heads[heads|subheads|greens]]<-(aggregate(grph$ZLENGTH,by=grph["span"],FUN=sum)$x-gs)/(spans[,3]-gs) #density of spans less %%% sections
	ph<-sum(grph$ZLENGTH[oe])/(tlen-sum(grph$ZLENGTH[!oe]))
	cat(".\tHighlighted:",round(ph*100),"%")
	t$odens<-round(t$dens*100)
	t$dens<-round(lintran(t$dens,c(0,1/3),c(.3,1)),3) # linear transform to make [0,1/3] range a [.3,1] range
	t$dens[t$dens>1]<-1 # in case some real value is higher than 25%
	t$sdens<-round(lintran(t$sdens,c(0,1/3),c(.2,1)),3)
	t$sdens[t$sdens>1]<-1


 	## for excitment, assume maximum redness would be !!! for every comment made (so excitement is as subset of all comments made, will wash out in sections with tons of comments)
 	t$xcit<-NA
	grph$sc<-as.integer(grepl("[A-Za-z]",grph$ZMARKNAME)&grph$ZMARKNAME!="NA") # identify "substantive commment", measure if letters present
	is<- aggregate((!(!oe|grph$sc))*grph$ZLENGTH*6,by=grph["span"],FUN=sum)$x # adjustment for ignored portion of text
	iss<- aggregate((!(!oe|grph$sc))*grph$ZLENGTH*6,by=grph["subspan"],FUN=sum)$x# adjustment for ignored portion of text
	t$xcit[!gi]<-(aggregate(grph$xcit*grph$ZLENGTH,by=grph["subspan"],FUN=sum)$x+aggregate(grph$sc*grph$ZLENGTH*3,by=grph["subspan"],FUN=sum)$x)/((diff(c(grph$ZLOCATION[heads|subheads],tlen)))*6-iss)
	 #xcit density of subspans
	t$sxcit[heads[heads|subheads|greens]]<-(aggregate(grph$xcit*grph$ZLENGTH,by=grph["span"],FUN=sum)$x+aggregate(grph$sc*grph$ZLENGTH,by=grph["span"],FUN=sum)$x*3)/((spans[,3])*6-is) #also ad 3 to each highlight where I wrote in a comment
	phx<-sum(grph$ZLENGTH*(grph$xcit+grph$sc*3))/tlen/6
	cat(".\tExciting:",round(phx*100),"%")
	t$oxcit<-round(t$xcit*100)
	t$xcit<-round(lintran(t$xcit,c(0,1/4),c(0,1)),3) # linear transform to make [0,.25] range a [0,1] range
	t$xcit[t$xcit>1]<-1 # in case some real value is higher than 25%
	t$sxcit<-round(lintran(t$sxcit,c(0,1/4),c(0,1)),3)
	t$sxcit[t$sxcit>1]<-1

    t$tcol[gi]<-"black"
 	t$tcol[!gi]<-hsv(h=1,s=1,v=t$xcit[!gi],alpha=t$dens[!gi])
	t$tcol[hi]<-hsv(h=1,s=1,v=t$sxcit[hi],alpha=t$sdens[hi])
	if(F){
		graphics.off()
		tst<-unique(hsv(h=1,s=1,v=round(t$xcit[!gi],1),alpha=round(t$dens[!gi],1)))
		pie(rep(1,length(tst)),labels=paste("v",round(t$xcit[!gi],1),"a",round(t$dens[!gi],1),sep=""), col = tst)
	}

    t$y[heads[heads|subheads|greens]]<-t$y[heads[heads|subheads|greens]]-1 # for some reason, shifting each label up one (to get it off the line?)

    t$attcol<-rainbow(max(t$att),start=.6,end=1)[t$att]


    lh<-list()
    for(j in 1:sum(heads)) lh[[j]]<-cbind(x=c(0,spans[j,3]),y=rep(t$y[which(heads[heads|subheads|greens])[j]]+1,2))
    lv<-list()
    for(j in which(subheads|greens)) lv[[length(lv)+1]]<-cbind(x=rep(grph$x1[j],2),y=c(tail(as.vector(lh[[grph$span[j]]]),1),t$y[sum((heads|subheads|greens)[1:j])]))
	for(j in which(subheads[subheads|greens])) lv[[j]][2,2]<-lv[[j]][2,2]+.3

    s<-max(t$y)+3 # starting position for excerpts

	##### Create text object, where every line is a line of text on the output
    txt<-list()
    eref<-seq(1,2*dim(grph)[1],by=2)
    aref<-eref+1
    txt[eref]<-grph$exc
    txt[aref]<-grph$ZMARKNAME
    grph<-data.frame(grph,kind="exc",stringsAsFactors=F)
    grph$kind[heads]<-"hed"
    grph$kind[subheads]<-"shed"
    grph$kind[gt&!grph$sc]<-"gt"
    id<-list()
    adj<-list()
    kind<-list()
    for(j in 1:length(eref)){
    kind[[eref[j]]]<-rep(grph$kind[j],grph$excl[j])
    adj[[eref[j]]]<-rep(grph$span[j],grph$excl[j])
    id[[eref[j]]]<-rep(eref[j], grph$excl[j])
    }
    suppressWarnings(id[aref]<-rep(1:length(grph$annl), grph$annl))
    for(j in 1:length(aref)){
    kind[[aref[j]]]<-rep("com",grph$annl[j])
    adj[[aref[j]]]<-rep(grph$span[j],grph$annl[j])
    id[[aref[j]]]<-rep(aref[j], grph$annl[j])
    }
    txt<-data.frame(txt=unlist(txt),id=unlist(id),kind=unlist(kind),y=unlist(adj),stringsAsFactors=F)
    txt<-txt[!txt$txt%in%c("<NA>","NA","")&!grepl("^#",txt$txt)&!(nchar(txt$txt)==sapply(gregexpr("^[!%]*",txt$txt),attr,"match.length")),]
	txt$txt<-sub("^[!#%]+ ","",txt$txt) # remove !, %, or # from comments
	txt$txt<-gsub("\r|\n"," ",txt$txt)# remove stray returns

	### for every %%% delete all but first two lines of text, unless a comment was made there

	if(sum(gt)){
		tran<-aggregate(x=which(txt$kind=="gt"),by=list(txt$id[txt$kind=="gt"]),FUN=range)
		tran<-cbind(tran[,-1],apply(tran[,-1],1,diff))
		tran<-rbind(tran[tran[,3]>1,])
		if(length(tran)) txt<-txt[-unlist(mapply(seq,tran[,1]+2,tran[,2])),]
	}

	txt$y<-((s+1):(s+dim(txt)[1]))+txt$y-1
    rm(list=c("adj","kind","id"))
    l<-txt$y[which(!!(c(2,diff(txt$y)-1)))]-1
    dev.off()


	####### Begin second run, actual graphical device
    h<-max(txt$y)
    pdf(paste("txtmap_",i,".pdf",sep=""),width=8.5,height=(h/6)+3)
    plot.new()
    par(family=family,font=1
        ,mar = c(b=1,l=.75,r=.75,t=1)
        ,oma = rep(.5, 4)
    ,yaxs="i")
    plot.window(xlim=c(1,rhs), ylim=c(h+1,-1))
    #{axis(1);axis(2,at=seq(from=0,to=h,by=5))}
    lapply(lh,lines,lwd=5,lend=1,col="black")
    lapply(lv[(greens&!subsubheads)[subheads|greens]],lines,lwd=1,lend=0,col=hsv(h=5/16,s=1))
    lapply(lv[subsubheads[subheads|greens]],lines,lwd=1,lend=0,col=orange1)
    lapply(lv[subheads[subheads|greens]],lines,lwd=1,lend=0,col=orange1) # TOC stems so they appear on top of densities

	lhs<-list()
	if(sum(subheads)){
		for(j in 1:sum(subheads)) lhs[[j]]<-cbind(x=c(t$x1[shi][j],t$x1[shi][j]+ifelse(t$pos[shi][j]==2,-1,1)*t$strw[shi][j]),y=rep(t$y[shi][j]+.3,2))
			lapply(lhs,lines,lwd=1,lend=0,col=orange1)
	}
    for(j in 1:dim(t)[1]) {
		#text(x=grph$x1[heads|subheads|greens][j],y=t$y[j],labels=t$t[j],pos=t$pos[j],offset=t$off[j]-.01,font=t$font[j],col="black",cex=t$cex[j])
		#text(x=grph$x1[heads|subheads|greens][j],y=t$y[j],labels=t$t[j],pos=t$pos[j],offset=t$off[j]+.02,font=t$font[j],col="black",cex=t$cex[j])
		text(x=grph$x1[heads|subheads|greens][j],y=t$y[j],labels=t$t[j],pos=t$pos[j],offset=t$off[j],font=t$font[j],col="white",cex=t$cex[j])
		text(x=grph$x1[heads|subheads|greens][j],y=t$y[j],labels=t$t[j],pos=t$pos[j],offset=t$off[j],font=t$font[j],col=t$tcol[j],cex=t$cex[j])
	}
    g<-t[gi,]
	cat("\n")
    t<-t[!gi,]
    for(j in 1:sum(greens)) t$seg[grph$subspan[greens][j]]<-t$seg[grph$subspan[greens][j]]+g$seg[j]

    #lines(x=c(0,rhs),y=c(s,s),lty=3,lwd=5,col=c(orange1,hsv(h=5/16,s=1))[gi+1])
    for(j in 1:length(l)) lh[[j]][,2]<-l[j]

    shl<-list()
    shly<-txt$y[txt$kind=="shed"]
    if(!!length(shly)) for(j in 1:length(shly)) shl[[j]]<-matrix(c(0,rhs,rep(shly[j],2)),nrow=2)
    yb<-c(l[-1]-1,max(txt$y))
    if(!length(lv)) {for(j in 1:length(lv)){
    if(c(1,diff(grph$span[subheads|greens]))[j]) {lv[[j]][1,2]<-l[j]+2} else {lv[[j]][1,2]<-shl[[j-1]][1,2]-1}
    if(c(diff(grph$span[subheads|greens]),1)[j]) {lv[[j]][2,2]<-yb[j]} else {lv[[j]][2,2]<-shl[[j+1]][1,2]}
    }}

    segcoor<-data.frame(top=txt$y[txt$kind%in%c("hed","shed")]+1,bot=c(txt$y[txt$kind%in%c("hed","shed")][-1]-1,max(txt$y)),kind=txt$kind[txt$kind%in%c("hed","shed")],stringsAsFactors=F)
    segcoor$bot<-segcoor$bot-c((txt$kind[txt$kind%in%c("hed","shed")]=="hed")[-1],0)

    pad<-max(strwidth(unique(unlist(strsplit(unlist(grph$exc),character(0))))))*.5

    pp<-data.frame(pp,y=txt$y[txt$kind=="hed"][pp$span]-1,ybot=txt$y[txt$kind=="hed"][-1][pp$span]-1.5)
    pp$ybot[is.na(pp$ybot)]<-max(txt$y)

    if(any(txt$kind%in%c("exc","gt"))){
    	ybottom<-txt$y[!!c(diff(txt$id),1)&txt$kind%in%c("exc","gt")]+.5
    	ytop<-txt$y[!!c(1, diff(txt$id))&txt$kind%in%c("exc","gt")]-.5
    	rect(xleft=0-rhs*.0125
    			 ,ybottom=ybottom
    			 ,xright=rhs+.0125*rhs
    			 ,ytop=ytop
    			 ,border="white",lwd=1,col=grph$col1[grph$kind%in%c("exc","gt")],ljoin=1
    	) # excerpt side box
    }
    rect(xleft=0-rhs*.0125,xright=rhs+rhs*.0125,ytop=segcoor$top-1.5,ybottom=segcoor$top-.5,border="white",lwd=1,col=orange1,ljoin=1)

    samp<-grph$exc!=""&!grepl("^#", grph$exc)&!(nchar(grph$exc)==sapply(gregexpr("^!*", grph$exc),attr,"match.length"))&grph$kind%in%c("exc","gt")
	grph$page<-pp$page[apply(sapply(as.list(grph$ZLOCATION),">", pp$zloc),2,which.min)]
	### white page numbers on left side ###
	if(any(txt$kind%in%c("exc","gt"))) text(x=0-(rhs*.0125/3),y=apply(cbind(ytop,ybottom),1,mean),grph$page[samp]-1,col="white",srt=90,cex=.5,offset=0,adj=c(.5,.25))
    text(x=0-(rhs*.0125/3),y=segcoor$top-1,grph$page[!samp]-1,col="white",srt=90,cex=.5,offset=0,adj=c(.5,.25))
	### white percentage locations on right side ###
    if(any(txt$kind%in%c("exc","gt"))) text(x=rhs+(rhs*.0125/3),y=apply(cbind(ytop,ybottom),1,mean),round(grph$ZLOCATION[samp]/tlen*100,0),col="white",srt=270,cex=.5,offset=0,adj=c(.5,.25))
    text(x=rhs+(rhs*.0125/3),y=segcoor$top-1,round(grph$ZLOCATION[!samp]/tlen*100,0),col="white",srt=270,cex=.5,offset=0,adj=c(.5,.25))
	### copyright sign next to comments


    if(any(txt$kind%in%c("exc","gt")))    rect(xleft=0
    ,ybottom=ybottom
    ,xright=rhs
    ,ytop=ytop
    ,border="white",lwd=1,col="white"
    ,ljoin=1
	) # excerpt frame box

	### Identifying "top" sections based on excitment
	xr<-vector("numeric",length=sum(heads|subheads))
	xr[heads[heads|subheads]]<-0-(rhs*.0125/1.7)
	xr[subheads[heads|subheads]]<-t$x1[subheads[heads|subheads]]
	rr<-ave(rank(-t$oxcit, ties.method = "first"),t$xcit,FUN=min)
	t$rank<-rr
	rr[t$oxcit==0]<-""
	yr<-t$y[heads[heads|subheads]][grph$span[heads|subheads]]+1
	tp<-rr%in%as.character(1:ceiling(dim(t)[2]/5)) #adjust number of top segments by total number of segements (1/5 of total)


    rect(xleft=t$x1[!tp]
    ,xright=t$seg[!tp]+t$x1[!tp]-1
    ,ytop=segcoor$top[!tp]-.5
    ,ybottom=segcoor$bot[!tp]+.5
    ,border=orange1,lwd=1,ljoin=1,lty="solid",col=orange2
	) #subsegment boxes

    rect(xleft=t$x1[tp]
    ,xright=t$seg[tp]+t$x1[tp]-1
    ,ytop=segcoor$top[tp]-.5
    ,ybottom=segcoor$bot[tp]+.5
    ,border=red1,lwd=1,ljoin=1,lty="solid",col=red2
	) #subsegment boxes for tops


	y<-txt$y[as.logical(diff(txt$id))]+.5
    rect(xleft=0,xright=rhs,ytop=y,ybottom=y,border="gray85",lwd=0)

    try(rect(xleft=0-rhs*.0125
    ,ybottom=txt$y[!!c(diff(txt$id),1)&txt$kind=="com"]+.5
    ,xright=rhs+.0125*rhs
    ,ytop=txt$y[!!c(1, diff(txt$id))&txt$kind=="com"]-.5
    ,border="white",lwd=1,col=colors()[2],ljoin=1),silent=T
	) # comment frame box, error if no comments

    rect(xleft=grph$x1[samp]
    ,ybottom=segcoor$bot[grph$subspan[samp]]+.5
    ,xright=grph$x2[samp]
    ,ytop=segcoor$top[grph$subspan[samp]]-.5
    ,border=NA,lwd=1,col="white",ljoin=1
	) # tall excerpt box

    if(any(txt$kind%in%c("exc","gt")))    rect(xleft=grph$x1[samp]
    ,ybottom=ybottom
    ,xright=grph$x2[samp]
    ,ytop=ytop
    ,border=NA,lwd=1,col=grph$col1[grph$kind%in%c("exc","gt")],ljoin=1
	) # excerpt location box

	lhf<-list() # fill lines on top of TOC to show attention densities
	for(j in 1:dim(grph)[1]) lhf[[j]]<-cbind(x=c(grph$x1[[j]],grph$x2[[j]]),y=rep(t$y[heads[heads|subheads]][grph$span[[j]]]+1,2))
	for(j in unique(grph$col1)) lapply(lhf[grph$col1==j],lines,lwd=5,lend=1,col=j)
	if(sum(grph$sc)) text(x=apply(grph[!!grph$sc,c("x1","x2")],1,mean),y=t$y[heads[heads|subheads]][grph$span[!!grph$sc]]+.6,labels=rep("©",sum(grph$sc)),col="blue",cex=.5,font=2)

	### boxes at head of TOC lines to indicate total section color densities
	points(cbind(x=0-(rhs*.0125/1.7),y=t[!is.na(t$sdens),"y"]+1),bg=hsv(h=1,s=1,v=t$xcit[!is.na(t$sdens)],alpha=t$dens[!is.na(t$sdens)]),pch=22,ljoin=1,lwd=.5,cex=1.75)
    ### subsection and figure icons
	points(do.call(rbind,lv[(greens&!subsubheads)[subheads|greens]]),lend=0,col=hsv(h=5/16,s=1),pch=rep(c(8,20),length(lv[greens[subheads|greens]])),cex=.5)
	points(do.call(rbind,lv[subsubheads[subheads|greens]]),lend=0,col=orange1,pch=rep(c(8,20),length(lv[greens[subheads|greens]])),cex=.5)
	points(do.call(rbind,lv[!greens[subheads|greens]]),lend=0,lwd=0,ljoin=1,col="black",bg="orange",pch=rep(c(23,NA),length(lv[!greens[subheads|greens]])),cex=1.75)


	### Rank of xcit written inside points
	text(x=xr,y=yr,labels=rr,col="white",cex=.4,offset=0,font=2)

	### Targets drawn around top 3 sections
	tpx<-t$x1[tp]
	tpx[heads[heads|subheads][tp]]<-0-(rhs*.0125/1.7)
	tpy<-t$y[tp]+1
	tpy[subheads[heads|subheads][tp]]<-t$y[heads[heads|subheads]][grph$span[heads|subheads][tp&subheads[heads|subheads]]]+1

	points(x=tpx,y=tpy,pch=1,cex=2.5,col=hsv(1,1,1,1)) #actual targets

    rect(xleft=0,xright=rhs,ytop=segcoor$top-1.5,ybottom=segcoor$top-.5,border="white",lwd=1,col=c("gray85","gray85")[factor(segcoor$kind)],ljoin=1)
    rect(xleft=t$x1[!tp],xright=t$seg[!tp]+t$x1[!tp]-1,ytop=segcoor$top[!tp]-1.5,ybottom=segcoor$top[!tp]-.5,border="white",lwd=1,col=orange1,ljoin=1) # subsegment heading box
    rect(xleft=t$x1[tp],xright=t$seg[tp]+t$x1[tp]-1,ytop=segcoor$top[tp]-1.5,ybottom=segcoor$top[tp]-.5,border="white",lwd=1,col=red1,ljoin=1) # subsegment heading box for top 5

    #for(j in 2:length(lh)) {lh[[j]][1,1]<-lh[[j]][2,1];lh[[j]][1,2]<-shl[[j-1]][1,2];lh[[j]][2,2]<-yb[j]}
    x<-rep(pad,dim(txt)[1])
    x[txt$kind=="hed"]<-0
  #  lapply(lapply(txt$txt,strwidth),"/",rhs)
  #  lapply(lapply(grph$exc,strwidth),"/",rhs)
  #  max(unlist(sapply(lapply(txt$txt,strwidth),"/",rhs)))
  #  max(unlist(sapply(lapply(grph$exc,strwidth),"/",rhs)))

  text(x=pp$x,y=pp$y+.25,paste(pp$page,"[",1:dim(pp)[1],"]",sep=""),pos=4,col="darkgray",font=1,cex=.5,offset=.25) #page numbers

  rect(xleft=pp$x
  ,ybottom=pp$ybot
  ,xright=pp$x
  ,ytop=pp$y #-.5
  ,border=gray(0,alpha=.05),lwd=2,col=NA,ljoin=1,lty=1
  ) # page separator


  wh.sh<-gray(1,.75) # white shadow

  rect(xleft=spans[,3]
  ,ybottom=c(txt$y[txt$kind=="hed"][-1]-2,max(txt$y))+.5
  ,xright=rhs
  ,ytop=txt$y[txt$kind=="hed"]+.5-1
  ,border=wh.sh,lwd=1,col=wh.sh,ljoin=1,lty="blank"
  ) # span boxes  transparency boxes to mute

  rect(xleft=spans[,3]
  ,ybottom=c(txt$y[txt$kind=="hed"][-1]-2,max(txt$y))+.5
  ,xright=spans[,3]
  ,ytop=txt$y[txt$kind=="hed"]+.5-1
  ,border=orange1,lwd=1,col=NA,ljoin=1,lty=1
  ) # span boxes



if(sum(grph$sc)){
	cpy<-aggregate(txt[,c("kind","y")],by=txt["id"],unique)
	text(x=0-(rhs*.0125/2),y=sapply(cpy$y[cpy$kind=="com"],min),rep("©",sum(grph$sc)),col="white",srt=0,cex=.5,offset=0)
}


    text(x=rhs*1/8/8/2,y=txt$y,txt$txt,pos=4,offset=0,cex=1/max(unlist(sapply(lapply(grph$exc,strwidth),"/",rhs))),col=gray(.4,alpha=1))
	text(x=rhs,y=0,labels=paste(i," Highlighted: ",round(ph*100),"%"," Exciting: ",round(phx*100),"%",sep=""),pos=2,font=1,cex=.75,family="Helvetica")



    dev.off()
	graphics.off()
	print(t[,c("t","rank","att","odens","dens","oxcit","xcit")],outer=T)
}
cat("\n")
}

if(F){
rm(list=ls())
vdl="Voice Dream Library" ## delete this
citekeys="Bourdieu1985wh"
i=citekeys
debug=T
graphics.off()
}

if(F){
source("VD2txtmap.R")
VDtextmap(vdl="Voice Dream Library",citekeys="menu")
}
