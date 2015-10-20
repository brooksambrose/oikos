t<-readLines('jacob.txt')
nc<-nchar(t)
plot(density(nc))
fr.sen<-function(matches,text) {
	if(matches[1]==-1) return('')
	r<-substr(text,0,matches[1]+1)
	r
}
ns<-gregexpr('(\\.|\\?|!)( |$)',t)
nsl<-sapply(ns,function(x) x[1]==-1)
prof<-mapply(fr.sen,matches=ns,text=t)
prof[nsl]<-t[nsl]
tbl<-table(prof)
prof[prof%in%names(tbl)[tbl>1]]<-''
writeLines(prof,'jacob-prof.txt')

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
require(openNLP)
require(NLP)
sta <- Maxent_Sent_Token_Annotator()
wta <- Maxent_Word_Token_Annotator()
pa <- Parse_Annotator()


#'@param pt parse tree
#'@param pck parse codes to keep, default nouns
#'@param pct parse codes to toss, default non-nouns
#'@references http://www.martinschweinberger.de/blog/part-of-speech-tagging-with-r/
parse.crawl<-function(
	pt
	,pcin=c('NN','NNS','NNP','NNPS')
	,pcout=c('CC','CD','DT','EX','FW','IN','JJ','JJR','JJS','LS','MD','PDT','POS','PRP','PRP$','RB','RBR','RBS','RP','SYM','TO','UH','VB','VBD','VBG','VBN','VBP','VBZ','WDT','WP','WP$','WRB')
	){
	pt<-cbind(unlist(pt))
	tt<-sub('.+\\.','',rownames(pt))
	rownames(pt)<-NULL
	pt<-data.frame(pos=pt[which(tt=='children')-1],tok=pt[which(tt=='children')])
	test<-pt$pos%in%pcin
	cumsum(pt$pos%in%pcin)
	ret<-list()
	l<-NULL
	for(i in 1:length(pt)) {
		if(pt[i]%in%pcin){
			if(is.null(l)) l<-pt[i]
		}
	}
}

tbl<-table(unlist(Tree_apply(ptrees[[1]],function(x){x<-try(x$value,silent=T);if(!inherits(x,'try-error')) unlist(x)},recursive=T)))
fun<-function(x) {
	if(is.null(names(x))) return(NULL)
		pos<-x$value
		x<-unlist(x$children)
		names(x)<-NULL
		x<-x[!x%in%names(tbl)]
		x<-paste(x,collapse=' ')
		names(x)<-pos
		x
}
el<-cbind(unlist(Tree_apply(ptrees[[1]],fun,recursive=T)))

require(data.table)
t<-data.table(t)

t[,s:=as.Strings(t)]
t[,sta] #sentences
t[,wta] #words
t[,pa] #parsing

install.packages('koRpus',dependencies=T)
require()
