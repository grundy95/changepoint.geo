euclidean.map <- function(X){
	iv <- apply(X,2,min)-1
	Xp <- t(apply(X,1,function(x){x-iv}))
	ref.vec <- rep(1,length(X[1,]))
	euclidean.dist <- function(x,ref.vec){
		d <- sqrt(sum((x-ref.vec)^2))
		return(d)
	}
	euclidean.ang <- function(x,ref.vec,norm.ref.vec){
		cosa <- (x%*%ref.vec)/(norm(x,type='2')*norm.ref.vec)
		if(cosa>1){
			warning('Angle was undefined, probably computational rounding error - set angle equal to 0')
			cosa <- 1
		}
		return(acos(cosa))
	}
	distance <- apply(X,1,euclidean.dist,ref.vec=ref.vec)
	angle <- apply(X,1,euclidean.ang,ref.vec=ref.vec,norm.ref.vec=norm(ref.vec,type='2'))
	return(cbind(distance,angle))
}
