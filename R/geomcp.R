geomcp <- function(X,penalty='MBIC',pen.value=0,test.stat='Normal',msl=2,nquantiles=1,MAD=FALSE,ref.vec='Default',ref.vec.value=0,parallelise=TRUE){
	#Error catching
	if(!is.numeric(X)){
		stop("Only numeric data allowed")
	}
	if(!is.matrix(X)){
		if(is.data.frame(X)){
			X <- as.matrix(X)
		}else{
			stop("Data type must be a matrix or data frame")
		}
	}
	if(anyNA(X)){
		stop("Missing value: NA is not allowed in the data")
	}
	if(!((test.stat=='Normal')||(test.stat=="Empirical"))){
		stop('Invalid test statistic, must be Normal or Empirical')
	}
	nquantiles <- as.integer(nquantiles)
	if((nquantiles<1)){
		stop("Number of quantiles must be a positive integer")
	}
	if((nquantiles==1)&&(test.stat=='Empirical')){
		nquantiles <- ceiling(4*log(length(X[,1])))
	}
	if((nquantiles!=1)&&(test.stat=='Normal')){
		nquantiles <- 1
		warning('nquantiles is not used with a Normal test statistic')
	}
	if(ref.vec=='Default'){
		ref.vec.value <- rep(1,length(X[1,]))
	}
	else if(ref.vec=='Manual'){
		if(!is.numeric(ref.vec.value)){
			stop("Reference vector value should be a vector of type numeric")
		}
		if(length(ref.vec.value)!=length(X[1,])){
			stop('Length of reference vector is not the same as number of series in data')
		}
	}
	else{
		stop('Reference vector type not recognized; should be "Default" or "Manual".')
	}
	if(!is.logical(MAD)){
		stop('MAD should be logical; TRUE or FALSE.')
	}
	if(!is.logical(parallelise)){
		stop('parallelise should be logical; TRUE or FALSE. Will only parallelise on non-Windows operating systems.')
	}
	##
	
	##Copy of original data
	X.original <- X

	##mad Transformation
	if(MAD){
		X <- apply(X,2,function(x){(x-median(x))/mad(x)})
	}

	##Data centralization
	min.X <- apply(X,2,min)-ref.vec.value
	X <- t(apply(X,1,function(x){x-min.X}))

	if(!parallelise){
		#Projection
		X.dist <- distance.mapping(X,ref.vec.value)
		X.ang <- angle.mapping(X,ref.vec.value)
	#

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
		out <- class_input(data.set=X.original,distance=X.dist,angle=X.ang,penalty=penalty,pen.value=pen.value(dist.cpts.ans),test.stat=test.stat,msl=msl,nquantiles=nquantiles,dist.cpts=cpts(dist.cpts.ans),ang.cpts=cpts(ang.cpts.ans))
		return(out)
	}

	##

	fork <- function(i){
		if(i==1){
			X.dist <- distance.mapping(X,ref.vec.value)
			if(test.stat=='Normal'){
				dist.cpts.ans <- changepoint::cpt.meanvar(X.dist,penalty=penalty,pen.value=pen.value,method='PELT',param.estimates=FALSE,minseglen=msl,class=TRUE)
			}	
			else if(test.stat=='Empirical'){
				dist.cpts.ans <- changepoint.np::cpt.np(X.dist,penalty=penalty,pen.value=pen.value,method='PELT',minseglen=msl,nquantiles=nquantiles,class=TRUE)
			}else{
				stop('Invalid test statistic, must be Normal or Empirical')
			}
			return(list('X.dist'=X.dist,'dist.cpts'=dist.cpts.ans))
		}else{
			X.ang <- angle.mapping(X,ref.vec.value)
			if(test.stat=='Normal'){
				ang.cpts.ans <- changepoint::cpt.meanvar(X.ang,penalty=penalty,pen.value=pen.value,method='PELT',param.estimates=FALSE,minseglen=msl,class=TRUE)
			}	
			else if(test.stat=='Empirical'){
				ang.cpts.ans <- changepoint.np::cpt.np(X.ang,penalty=penalty,pen.value=pen.value,method='PELT',minseglen=msl,nquantiles=nquantiles,class=TRUE)
			}else{
				stop('Invalid test statistic, must be Normal or Empirical')
			}
			return(list('X.ang'=X.ang,'ang.cpts'=ang.cpts.ans))
		}
	}
	cl <- parallel::makeForkCluster(2)
	doParallel::registerDoParallel(cl)
	ans <- foreach(i=1:2) %dopar%{
		fork(i)
	}
	parallel::stopCluster(cl)

	#Class Structure
	out <- class_input(data.set=X.original,distance=ans[[1]]$X.dist,angle=ans[[2]]$X.ang,penalty=penalty,pen.value=pen.value(ans[[1]]$dist.cpts),test.stat=test.stat,msl=msl,nquantiles=nquantiles,dist.cpts=cpts(ans[[1]]$dist.cpts),ang.cpts=cpts(ans[[2]]$ang.cpts))
	return(out)
}

