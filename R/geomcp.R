geomcp <- function(X,penalty='MBIC',pen.value=0,test.stat='Normal',msl=2,nquantiles=1,MAD=FALSE,ref.vec='Default',ref.vec.value=0){
	##{{{Error catching
	#Data
	if(!is.matrix(X)){
		if(is.data.frame(X)){
			X <- as.matrix(X)
		}else{
			stop("Data type must be a matrix or data frame")
		}
	}
	if(length(X[1,])<2){
		stop('Univariate changepoint analysis is not supported')
	}
	if(anyNA(X)){
		stop("Missing value: NA is not allowed in the data")
	}
	if(!is.numeric(X)){
		stop("Only numeric data allowed")
	}

	#penalty type
	penalty <- toupper(penalty)
	if(!(penalty %in% c('MBIC','BIC','SIC','MANUAL','HANNAN-QUINN'))){
	       stop('Univariate penalty choice not recognized; should be "MBIC", "BIC", "SIC","Hannan-Quinn" or "Manual"')
	}else if(penalty=='MANUAL'){
		penalty <- 'Manual'
	}else if(penalty=='HANNAN-QUINN'){
		penalty <- 'Hannan-Quinn'
	}

	#pen.value - Errors caught in changepoint package

	#test statistic
	test.stat <- toupper(test.stat)
	if(!((test.stat=='NORMAL')||(test.stat=="EMPIRICAL"))){
		stop('Invalid test statistic, must be Normal or Empirical')
	}else if(test.stat=='NORMAL'){
		test.stat='Normal'
	}else{
		test.stat='Empirical'
	}

	#minimum segment length
	if(msl<1|msl>floor(length(X[,1])/2)){
		stop('Minimum segment length must be between 1 and half the no. of time points (rounded down)')
	}

	#no of quantiles
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
	
	#MAD transformation
	if(!is.logical(MAD)){
		stop('MAD should be logical; TRUE or FALSE.')
	}
	
	#Reference vector & Reference vector value
	ref.vec <- toupper(ref.vec)
	if(ref.vec=='DEFAULT'){
		ref.vec.value <- rep(1,length(X[1,]))
		ref.vec <- 'Default'
	}else if(ref.vec=='MANUAL'){
		if(!is.numeric(ref.vec.value)){
			stop("Reference vector value should be a vector of type numeric")
		}else if(length(ref.vec.value)!=length(X[1,])){
			stop('Length of reference vector is not the same as number of series in data')
		}else if(isTRUE(all.equal(ref.vec.value,rep(0,length(X[1,]))))){
			stop('Reference vector cannot be the origin as angle undefined.')
		}else{
			ref.vec <- 'Manual'
		}
	}else{
		stop('Reference vector type not recognized; should be "Default" or "Manual".')
	}
	##}}}
	
	##Copy of original data
	X.original <- X

	##mad Transformation
	if(MAD){
		X <- apply(X,2,function(x){(x-median(x))/mad(x)})
		if(sum(is.nan(X))>0){
			stop('Unable to perform MAD transformation')
		}
	}

	##Data Translation
	min.X <- apply(X,2,min)-ref.vec.value
	X <- t(apply(X,1,function(x){x-min.X}))

	#Projection
	X.dist <- distance.mapping(X,ref.vec.value)
	X.ang <- angle.mapping(X,ref.vec.value)

	#Univariate changepoint detection
	if(test.stat=='Normal'){
		dist.cpts.ans <- cpt.meanvar(X.dist,penalty=penalty,pen.value=pen.value,method='PELT',param.estimates=FALSE,minseglen=msl,class=TRUE)
		ang.cpts.ans <- cpt.meanvar(X.ang,penalty=penalty,pen.value=pen.value,method='PELT',param.estimates=FALSE,minseglen=msl,class=TRUE)
	}
	else if(test.stat=='Empirical'){
		dist.cpts.ans <- cpt.np(X.dist,penalty=penalty,pen.value=pen.value,method='PELT',minseglen=msl,nquantiles=nquantiles,class=TRUE)
		ang.cpts.ans <- cpt.np(X.ang,penalty=penalty,pen.value=pen.value,method='PELT',minseglen=msl,nquantiles=nquantiles,class=TRUE)
	}
	#Class Structure
	out <- class_input(data.set=X.original,distance=X.dist,angle=X.ang,penalty=penalty,pen.value=pen.value(dist.cpts.ans),test.stat=test.stat,msl=msl,nquantiles=nquantiles,dist.cpts=cpts(dist.cpts.ans),ang.cpts=cpts(ang.cpts.ans))

	return(out)
}

