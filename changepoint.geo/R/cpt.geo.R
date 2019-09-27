cpt.euclid <- function(X,penalty='MBIC',pen.value=0,test.stat='Normal',msl=2,nquantiles=0,class=TRUE){
	#Error catching
	if(!((test.stat=='Normal')||(test.stat=="Empirical"))){
		stop('Invalid test statistic, must be Normal or Empirical')
	}
	if((msl<2)&&(test.stat=='Normal')){
		msl <- 2
		wanring('Minimum segment length for detecting changes in mean and variance in projected series using a Normal test statistic is 2; automatically changed to be 2.')
	}else if(msl<1){
		msl <- 1
		warning('Minimum segment length for a change in empirical distribution of projected series is 1; automatically changed to be 1.')
	}
	if((nquantiles==0)&&(test.stat=='Empirical')){
		nquantiles <- ceiling(4*log(length(X[,1])))
	}
	if((nquantiles!=0)&&(test.stat=='Normal')){
		nquantiles <- 0
		wanring('nquantiles is not used with a Normal test.stat')
	}
	##

	#Projection
	Xm <- euclidean.map(X)
	##

	#Univariate changepoint detection
	if(test.stat=='Normal'){
		dist.cpts.ans <- cpt.meanvar(Xm[,1],penalty=penalty,pen.value=pen.value,method='PELT',param.estimates=FALSE,minseglen=msl,class=TRUE)
		ang.cpts.ans <- cpt.meanvar(Xm[,2],penalty=penalty,pen.value=pen.value,method='PELT',param.estimates=FALSE,minseglen=msl,class=TRUE)
	}
	else if(test.stat=='Empirical'){
		dist.cpt.ans <- cpt.np(Xm[,1],penalty=penalty,pen.value=pen.value,method='PELT',minseglen=msl,nquantiles=nquantiles,class=TRUE)
		ang.cpt.ans <- cpt.np(Xm[,2],penalty=penalty,pen.value=pen.value,method='PELT',minseglen=msl,nquantiles=nquantiles,class=TRUE)
	}else{
		stop('Invalid test statistic, must be Normal or Empirical')
	}
	##

	#Class Structure
	out <- class_input(data.set=X,mapped.data=Xm,dist.measure='Euclidean',penalty=penalty,pen.value=dist.cpts.ans@pen.value,test.stat=test.stat,msl=msl,nquantiles,dist.cpts=dist.cpts.ans@cpts,ang.cpts=ang.cpts.ans@cpts)
	return(out)
}

