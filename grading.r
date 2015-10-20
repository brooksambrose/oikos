grading<-function(
	breps=1,reps,dir=getwd()
)
{
	timestamp<-format(Sys.time(), "%Y%h%d_%I-%M%p")
	setwd(dir)
	suppressWarnings({
	t<-list()
	m<-list()
	c<--1
	grad_diag<-list()
	rep_s<-as.vector(rbind(breps:reps,breps:reps))
	for(i in rep_s){
		c<-c*-1
		cat("\nPress [enter] to log",ifelse(c>0,"beginning","end"),"of rep",i,"of",reps,'or \'q\' to quit.')
		line<-readline()
		if(line%in%c('q','Q')) return(grad_diag)
		t[[length(t)+1]]<-Sys.time()
		pt<-unlist(t)
		if(length(t)==1) next
		gt<-(pt[seq(2,length(pt),by=2)]-pt[seq(1,length(pt),by=2)])/60
		gt<-gt[gt>0]
		if(c<0) m$gt[[length(m$gt)+1]]<-mean(gt)
		if(length(t)==2) {
			barplot(gt
				,main="Grading Time Diagnostics", xlab="Rep", ylab="Minutes",xaxt="n",col=c("black","gray"),border=NA)
			next
		}
		tt<-(pt[seq(1,length(pt),by=2)][-1]-pt[seq(2,length(pt),by=2)])/60
		tt<-tt[tt>0]
		if(c>0) m$tt[[length(m$tt)+1]]<-mean(tt)
		gttt<-as.vector(rbind(gt,tt))
		if(length(gt)>length(tt)) gttt<-gttt[-length(gttt)]
		cat(rbind(c("grading:","transition:","total:"),format(as.POSIXct(c(sum(gt)*60,sum(tt)*60,sum(gttt)*60),origin="1960-01-01"),"%H:%M:%S")))
		ax<-barplot(gttt
			,main="Grading Time Diagnostics", xlab="Rep", ylab="Minutes",xaxt="n",col=c("black","gray"),border=NA,space=0)
		axis(labels=rep_s[1:length(gttt)],side=1,at=ax)
		labs<-1:max(c(gttt))
		labs[!!labs%%5]<-NA
		cx<-rep(1,length(labs))
		cx[!labs%%5]<-2
		axis(side=2,at=1:max(c(gttt)),labels=labs,col=cx)
		lines(cbind(ax[seq(1,length(ax),by=2)],m$gt),lty="solid",lwd=5)
		lines(cbind(ax[seq(2,length(ax),by=2)],m$tt),lty="solid",lwd=5,col="gray")
		points(cbind(ax[seq(1,length(ax),by=2)],m$gt),lwd=4,col="white")
		points(cbind(ax[seq(2,length(ax),by=2)],m$tt),lwd=4,col="white")
		mtext(round(mean(gt),digits=1),side=4,at=mean(gt),par(las=1),adj=.5)
		mtext(round(mean(tt),digits=1),side=4,at=mean(tt),par(las=1),adj=.5)
		dev.copy2pdf(file=paste("grading_",timestamp,".pdf",sep=""), out.type = "pdf")
		grad_diag<-list(t,gttt,m)
		save(grad_diag,file=paste("grading_",timestamp,".RData",sep=""))
	}
	return(grad_diag)
	})
}

grading(breps=1,reps=13,dir='/Users/bambrose/Dropbox/2015-2016/2015-2016_A-Fall/MIDS/RDADA/07/Section 3/Submissions')

if(F){
grading(breps=1,reps=21,dir='/Users/bambrose/Dropbox/Summer 2015/SOC1Online/admin/grading/')

grading(breps=1,reps=21,dir='/Users/bambrose/Dropbox/Summer 2015/MIDS/RDADA/8/midterms/diagnostics')

grading(breps=17,reps=21,dir='/Users/bambrose/Dropbox/Summer 2015/SOC1Online/admin/grading/')
if(F){ #obsolete
x<-function() {x<-rpois(10^6,85);x<-x[x%in%(67:100)];x}
ret<-hist(x(),breaks=c(67,70,73,77,80,83,87,90,93,97,101)-.5)
ret<-prop.table(ret$counts)*60
names(ret)<-c('D','C-','C','C+','B-','B','B+','A-','A','A+')
ret<-cbind(round(ret))
ret[3]<-ret[3]-1
ret

cat(
	paste('<column id=\"',rev(rownames(ret)),'\" colour=\"',sub('^.(.+)..$','\\1',rainbow(3*6)[c(8:1,18:17)]),'\">',rev(ret),'</column>',sep='')
	,sep='\n')}


#system('textutil -convert txt /Users/bambrose/Dropbox/Summer\ 2015/SOC1Online/6/final/I/*.doc')
#system('textutil -convert txt /Users/bambrose/Dropbox/Summer\ 2015/SOC1Online/6/final/I/*.docx')

prof.und.writ<-function(dir,k=5,drop.letters=T,drop.numbers=T,drop.periods=T,drop.small.paras=F,dictionary=NULL){
 files<-grep('txt$',list.files(dir,full.names=T,recursive=T),value=T)
 docs<-lapply(files,readLines)
 names(docs)<-sub('.+final/','',files)
 docs<-lapply(docs,function(x) x[x!=''])
 pl<-sapply(docs,nchar)
 h<-hist(unlist(pl),breaks=quantile(unlist(pl),p=seq(0,1,.05)))
 thresh<-h$breaks[names(h$density)[which(names(h$density)==names(which(diff(diff(h$density)<0)==-1)[1]))-2]]
 abline(v=thresh,col='red')
 text(x=thresh,y=mean(range(h$density)),labels=ifelse(drop.small.paras,'assumed to be non-body text and discarded','would-be drop threshold, set \'drop.bottom=F\' to discard'),pos=4,cex=.75,offset=.5)
 if(!drop.small.paras) thresh=0
 ll<-lapply(pl,function(x) x>thresh)


 df<-data.frame(
words=mapply(function(a,b){
 length(unlist(gregexpr(' +',paste(a[b],collapse=' '))))
},a=docs,b=ll)
,paras=sapply(ll,sum)
,sents=mapply(function(a,b){
	length(unlist(gregexpr('[^ ]{2}\\. |$',paste(a[b],collapse=' '))))
},a=docs,b=ll)
,mwp=mapply(function(a,b){ #mean words per paragraph
	mean(sapply(gregexpr(' +',a[b]),length))
},a=docs,b=ll)
,sdwp=mapply(function(a,b){ #sd words per paragraph
	sd(sapply(gregexpr(' +',a[b]),length))
},a=docs,b=ll)
,mws=mapply(function(a,b){ #mean words per sentence
	mean(sapply(gregexpr(' +',strsplit(gsub('([^ ]{2}\\.) ','\\1%%',paste(a[b],collapse=' ')),'%%')[[1]]),length))
},a=docs,b=ll)
,sdws=mapply(function(a,b){ #sd words per sentence
	sd(sapply(gregexpr(' +',strsplit(gsub('([^ ]{2}\\.) ','\\1%%',paste(a[b],collapse=' ')),'%%')[[1]]),length))
},a=docs,b=ll)
)
if(!is.null(dictionary)) for(i in names(dictionary)){
df[[i]]<-mapply(function(a,b){
	ret<-sapply(dictionary[[i]],function(x) gregexpr(x,paste(a[b],collapse=' ')))
	w<-sapply(ret,function(x) x[1]==-1)
	ret<-sapply(ret,length)
	ret[w]<-0
	#ret<-ret/c(1,2)[grepl('\\)\\|\\(',names(ret))+1] #to avoid double counting multiple authors, but problematic
	list(ret)
	},a=docs,b=ll)
df[[paste(i,'unique',sep='.')]]<-sapply(df[[i]],function(x) sum(x>0))
df[[paste(i,'total',sep='.')]]<-sapply(df[[i]],sum)
}

require(tm)
require(SnowballC)
require(stm)

bow<-lapply(docs,FUN=paste,collapse=' ') # each doc is one long character string
bow<-lapply(bow,FUN=tolower) # transform to lower case
bow<-lapply(bow,FUN=removePunctuation) # ...
bow<-lapply(bow,FUN=iconv,sub=' ') # remove multibyte strings
bow<-lapply(bow,FUN=removeNumbers)
bow<-sapply(bow,FUN=strsplit,split='\\s+') # split docs into words "\\s+" is a regex. "[[:space:]]+" also works but is R specific
bow<-lapply(bow,FUN=removeWords,stopwords('english'))
bow<-lapply(bow,FUN=function(x) x[!!nchar(x)]) #remove blanks
bow<-lapply(bow,FUN=stemDocument,language='english')
bow<-lapply(bow,FUN=function(x) {n<-nchar(x);x<-x[n>1];x}) # remove single letter words

df$diction<-sapply(bow,function(x) length(unique(x)))

just.punct.f<-function(y){
y<-tolower(unlist(strsplit(y,'')))
y<-y[!y%in%c(' ','\t','')]
y[y%in%c('“','”')]<-'\"'
y[y%in%c('‘','’')]<-'\''
y[y%in%c('‐')]<-'-'
y[y%in%c('—')]<-'–'
if(drop.letters) y<-y[!y%in%letters]
if(drop.periods) y<-y[y!='.']
if(drop.numbers) y<-y[!y%in%as.character(0:9)]
y
}

as.punct<-lapply(docs,just.punct.f)
names(as.punct)<-names(docs)
vocab<-sort(unique(unlist(as.punct))) # stm expects as input a list of matrices where each element of the list is a document and where the first row of each matrix is the index of a word and the second row is the count of the word. This is a more memory efficient form since zeros are not stored.
stm.docs<-list()

for(i in names(as.punct)){
	t<-table(as.punct[[i]])
	stm.docs[[i]]<-rbind(vocab.index=which(vocab%in%names(t)),frequency=t)
}
pre2stm<-list()
alpha=50/k
pre2stm$model<-stm(documents=stm.docs,vocab=vocab,K=k,control=list(alpha=alpha))
pre2stm$top.word.phi.beta<-sapply(data.frame(pre2stm$model$beta$logbeta),function(x) sapply(x,function(y) ifelse(is.infinite(y),.Machine$double.eps,exp(y)))) # called beta by stm, epsilon closest thing to zero the machine can represent, necessary to prevent error
colnames(pre2stm$top.word.phi.beta)<-pre2stm$model$vocab
pre2stm$doc.top.theta<-pre2stm$model$theta
rownames(pre2stm$doc.top.theta)<-names(as.punct)
pre2stm$doc.length<-sapply(as.punct,length)
pre2stm$vocab<-pre2stm$model$vocab
tn<-table(unlist(as.punct))
pre2stm$term.frequency<-as.integer(tn)
names(pre2stm$term.frequency)<-names(tn)
pre2stm$as.punct<-as.punct
save(pre2stm,file='pre2stm.RData')

ret<-list(docs=docs,df=df,top=round(pre2stm$doc.top.theta,3),bow=bow)
ret
}

docsI<-prof.und.writ(dir='/Users/bambrose/Dropbox/Summer 2015/SOC1Online/6/final',k=3,drop.letters=T,drop.numbers=T,dictionary=list(reading=c("Mills","Perrow","Miner","Swidler","(Ragin)|(Amoroso)","Best","Goffman","Emerson","Ehrenreich","Lamont","(Massey)|(Denton)","Pascoe","Chambliss","(Western)|(Pettit)","Kozol","Calhoun","Conrad","Becker","Dowd","Domhoff","Ammerman","Coontz","Moore","(Carruthers)|(Ariovich)","Almeling","Beck","(Salazar)|(Parre[nñ]as)"),lecture=c('([Ll]esson)|([Ll]ecture)'),roy='Roy'))

source('/Users/bambrose/Dropbox/GitHub/CTAWG/CTAWG.R')
load('pre2stm.RData')
round(pre2stm$doc.top.theta,3)
fit<-prcomp(data.frame(docsI$df[,sapply(docsI$df,class)!='list'&!colnames(docsI$df)%in%c('lecture.unique','roy.unique')],docsI$top[,-5]))
write.table(fit$rotation,file='pca/pca-final.tab',row.names=T,col.names=T,quote=F,sep='\t',na='')
screeplot(fit,type='lines')
pred<-predict(fit)
pcex.mx<-sapply(data.frame(pred),which.max)
pcex.mn<-sapply(data.frame(pred),which.min)
pcex.md<-sapply(data.frame(pred),function(x) which.min(abs(x-mean(x))))
for(i in names(pcex)) writeLines(c('MAX\n\n',docsI$docs[[pcex.mx[i]]],'\nMID\n\n',docsI$docs[[pcex.md[i]]],'\nMIN\n\n',docsI$docs[[pcex.mn[i]]]),con=paste('pca/',i,'.txt',sep=''))


stmviz(pre2stm)
findThoughts(pre2stm$model,texts=docsI$docs,topics=2,n=1)
round(pre2stm$doc.top.theta,3)[order(pre2stm$doc.top.theta[,6]),6]

mids<-diff(c(67,70,73,77,80,83,87,90,93,97,100))/2+c(67,70,73,77,80,83,87,90,93,97)
names(mids)<-c('D','C-','C','C+','B-','B','B+','A-','A','A+')
x<-function(grade) {x<-rpois(10^6,mids[grade]);x<-x[x%in%(67:100)];x}
ret<-hist(x('B+'),breaks=c(67,70,73,77,80,83,87,90,93,97,101)-.5)
pret<-prop.table(ret$counts)
ret<-pret*164
names(ret)<-names(mids)
text(x=mids,y=0,labels=names(ret),offset=1)
ret<-cbind(round(ret,1))
ret[3]<-ret[3]-1
ret

interpreted<-data.frame(pred)
interpreted<-interpreted[,-c(8,12)]
for(i in c('PC1','PC3','PC5','PC6','PC7','PC10')) interpreted[[i]]<-interpreted[[i]]*-1
scores<-lapply(interpreted,function(x) {
	q<-quantile(x,p=cumsum(pret))
x<-names(pret)[sapply(x,function(y) min(which(y<=q)))]
x
})

docsI$df$score<-factor(names(pret)[round(apply(data.frame(scores,stringsAsFactors=F),1,function(x) mean(as.numeric(factor(x,levels=names(pret))))))],levels=names(pret))
docsI$df$scores<-as.list(data.frame(
	apply(data.frame(scores,stringsAsFactors=F),1,function(x) table(factor(x,levels=names(pret))))
))
docsI$df$scores<-lapply(docsI$df$scores,function(x) {names(x)<-names(pret);x})
names(docsI$df$scores)<-NULL


## compile VoiceDream highlights
comp.VD.high<-function(dir){
	files<-grep('txt$',list.files(dir,full.names=T,recursive=T),value=T)
	docs<-lapply(files,readLines)
	names(docs)<-sub('.+notes/','',files)
	terms<-lapply(docs,function(x) {x<-sub('^.+: ','',x[grep(':',x)]);x<-x[x!=''];x})
	lev<-c('C-','C-/C','C','C/C+','C+','C+/B-','B-','B-/B','B','B/B+','B+','B+/A-','A-','A-/A','A','A/A+','A+')
	grades<-sapply(terms, function(x) na.omit(factor(x,levels=lev)))
	grades<-unlist(grades)
	grades<-factor(grades)
	levels(grades)<-lev
	grades
}
grades<-comp.VD.high(dir='/Users/bambrose/Dropbox/Summer 2015/SOC1Online/6/notes')
require(data.table)
final.db<-data.table(id=sub('^[1-8]/(.+) .+$','\\1',names(grades)),grade=as.character(grades),number=as.numeric(grades))
final.db[,.N,by=id][N!=3] # check for missing
lintran<-function(x,s1=c(0,1),s2=c(0,1)) {a=diff(s2)/diff(s1);b=s2[1]-a*s1[1];return(a*x+b)}
final.db[,scaled:=lintran(number,c(1,length(levels(grades))),c(.72*30,.99*30))]
final.db<-final.db[,list(score=round(sum(scaled)+10)),by=id]
hist(final.db$score,breaks=as.vector(sapply(6:9*10,function(x) c(0,3,7)+x)))
final.db[score<80]
write.table(final.db,file='final.tab',quote=F,row.names=F,col.names=F,na='')
}


