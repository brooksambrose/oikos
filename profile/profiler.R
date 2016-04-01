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


#'@param ptree A single parse tree starting with value='TOP'.
#'@param mincount Return elements with this many or more instances.
#'@param pos Return elements whose part-of-speech tag matches this regular expression. Default is '^N' for nouns.
#'@references http://www.martinschweinberger.de/blog/part-of-speech-tagging-with-r/
parse.crawl<-function(
	ptree
	,mincount=1
	,postag='^N'
	){
	tbl<-table(unlist(Tree_apply(ptree,function(x){x<-try(x$value,silent=T);if(!inherits(x,'try-error')) unlist(x)},recursive=T)))
	print(tbl)
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
	el<-unlist(Tree_apply(ptree,fun,recursive=T))
	el<-el[grepl(postag,names(el))]
el<-table(el)
el[el>=mincount]
}
nouns<-parse.crawl(ptrees[[1]])


require(data.table)
t<-data.table(t)

t[,s:=as.Strings(t)]
t[,sta] #sentences
t[,wta] #words
t[,pa] #parsing

install.packages('koRpus',dependencies=T)
require()
