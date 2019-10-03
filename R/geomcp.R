geomcp <- function(X,penalty='MBIC',pen.value=0,test.stat='Normal',msl=2,nquantiles=0,mad=FALSE,ref.vec='Default',ref.vec.value=0){
	#Error catching
	if(!((test.stat=='Normal')||(test.stat=="Empirical"))){
		stop('Invalid test statistic, must be Normal or Empirical')
	}
	if((nquantiles==0)&&(test.stat=='Empirical')){
		nquantiles <- ceiling(4*log(length(X[,1])))
	}
	if((nquantiles!=0)&&(test.stat=='Normal')){
		nquantiles <- 0
		warning('nquantiles is not used with a Normal test statistic')
	}
	if(ref.vec=='Default'){
		ref.vec.value <- rep(1,length(X[1,]))
	}
	else if(ref.vec=='Manual'){
		if(length(ref.vec.value)!=length(X[1,])){
			stop('Length of reference vector is not the same as number of series in data')
		}
	}
	else{
		stop('Reference vector type not recognized; should be Default or Manual.')
	}

	##
	
	##Copy of original data
	X.original <- X

	##mad Transformation
	if(mad){
		X <- apply(X,2,function(x){(x-median(x))/mad(x)})
	}

	##Data centralization
	min.X <- apply(X,2,min)-ref.vec.value
	X <- t(apply(X,1,function(x){x-min.X}))
	#Projection
	X.dist <- distance.mapping(X,ref.vec.value)
	X.ang <- angle.mapping(X,ref.vec.value)
	##

	#Univariate changepoint detection
	if(test.stat=='Normal'){
		dist.cpts.ans <- cpt.meanvar(X.dist,penalty=penalty,pen.value=pen.value,method='PELT',param.estimates=FALSE,minseglen=msl,class=TRUE)
		ang.cpts.ans <- cpt.meanvar(X.ang,penalty=penalty,pen.value=pen.value,method='PELT',param.estimates=FALSE,minseglen=msl,class=TRUE)
	}
	else if(test.stat=='Empirical'){
		dist.cpts.ans <- cpt.np(X.dist,penalty=penalty,pen.value=pen.value,method='PELT',minseglen=msl,nquantiles=nquantiles,class=TRUE)
		ang.cpts.ans <- cpt.np(X.ang,penalty=penalty,pen.value=pen.value,method='PELT',minseglen=msl,nquantiles=nquantiles,class=TRUE)
	}else{
		stop('Invalid test statistic, must be Normal or Empirical')
	}
	##

	#Class Structure
	out <- class_input(data.set=X.original,distance=X.dist,angle=X.ang,penalty=penalty,pen.value=dist.cpts.ans@pen.value,test.stat=test.stat,msl=msl,nquantiles=nquantiles,dist.cpts=dist.cpts.ans@cpts,ang.cpts=ang.cpts.ans@cpts)
	return(out)
}

