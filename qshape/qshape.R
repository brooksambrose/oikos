#' A function for determining the shape of bins for a qsorting procedure. Count is assumed to be a zero-inflated poisson based on a hurdle process.
#' @param tn The total number of cases.
#' @param npz The number or proportion of irrelevant zeroes. The number of relevant cases will be treated as tn-npz if npz>=1 or tn*npz if npz<1
#' @param inc The increment for lambda, default is 0.1.
#' @param mode The largest desired or expected bin, defaults to 1 or the second relevant bin (after 0).
#' @param maxlev The highest expected count, defaults to 10 but can safely be lower.
#' @param samps Sample size for poisson draws, defaults to 10000.
#' @param out Path and filename to save delimited result.
#' @param label Character vector of names. Unimplemented.
qshape.f<-function(tn,npz,inc=0.1,mode='1',maxlev=10,samps=10000,out='qsort-tmplt.txt',label=c('count','letter','bottom','middle','top')){
	require(data.table)
	scale<-data.table(
	letter=c('F','D-','D','D+','C-','C','C+','B-','B','B+','A-','A','A+')
	,bottom=seq()
	)
	ten<-seq(5,10,.5)

		if(npz==0) {rn<-tn} else {rn<-ifelse(npz>=1,tn-npz,round(tn*npz))}
	tabpois<-function(x) round(prop.table(table(factor(x,levels=0:maxlev)))*rn)
	row<-list(tabpois(rpois(samps,inc)),tabpois(rpois(samps,inc*2)))
	names(row)<-c(inc,inc*2)
	while(row[[length(row)]][mode]>=row[[length(row)-1]][mode]){
		row[[as.character(inc*length(row)+1)]]<-tabpois(rpois(samps,inc*(length(row)+1)))
	}
	row<-do.call(rbind,row)
	row<-tail(row,sum(row[,mode]==max(row[,mode]))+2)
	row<-row[,1:min(which(apply(row,2,function(x) all(x==0))))]
	row<-cbind(row,t=apply(row,1,sum))
	nm<-c(tn-rn,row[nrow(row)-1,-ncol(row)])
	names(nm)[1]<-'0'
	names(nm)<-paste(names(nm),if(label[1]=='letter') tail(scale$letter,length(nm)-1),nm,sep=',')
	tmplt<-matrix('',nrow=max(nm)+1,ncol=length(nm))
	colnames(tmplt)<-names(nm)
	for(i in 1:length(nm)) tmplt[nm[i]+1,i]<-'#'
write.table(tmplt,file=out,quote=F,sep='\t')
	dimnames(row)<-list('lambda'=rownames(row),'count'=colnames(row))
	attributes(row)$bernouli<-ifelse(npz<1,npz,1-(rn/tn))
	let<-t(apply(row,1,function(y) c(rep(0,sum(y==0)),y[y>0])))
colnames(let)<-c(tail(scale$letter,ncol(let)-1),'t')
	list(row,let)
}
#debugonce(qshape.f)
(x<-qshape.f(tn = 26,npz = 0,samps=10^5,mode='1',out='qsort-tmplt26m1.txt',label='letter'))

if(F){ # quick historgram for SPring 2015 Midterm final scores
	g<-c(44.15,44.15,45.82,45.82,44.48,44.81,45.8,44.48,43.16,44.15,43.16,42.83,45.16,45.8,45.49,46.81,44.15,45.15,46.14,45.49,44.16,42.5,46.82,42.5,44.15,44.81)
	b<-seq(40,50,10/6)
	l<-c('B-','B ','B+','A-','A ','A+')
hist(g,breaks=b,freq=T,labels=l,xlab='Total Score',ylim=c(0,12)
		 ,main='RDADA Midterm Grade Distribution\nSections 3 & 4, n=26',axes=F)
axis(1,at=round(b,2))
axis(2,at=0:12)
}
