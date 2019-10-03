distance.mapping <- function(X,ref.vec.value=rep(1,length(X[1,]))){
	dist.map <- function(x,ref.vec.value){
		Dist <- sqrt(sum((x-ref.vec.value)^2))
		return(Dist)
	}
	X.dist <- apply(X,1,FUN=dist.map,ref.vec.value=ref.vec.value)
	return(X.dist)
}
angle.mapping <- function(X,ref.vec.value=rep(1,length(X[1,]))){
	ang.map <- function(x,ref.vec.value){
		dot.prod <- sum(x*ref.vec.value)
		norm1 <- sqrt(sum(x^2))
		norm2 <- sqrt(sum(ref.vec.value^2))
		if(abs(dot.prod/(norm1*norm2))>1){
			ang <- 0
		} 
		else{
			ang <- acos(dot.prod/(norm1*norm2))
		}
		return(ang)
	}
	X.ang <- apply(X,1,FUN=ang.map,ref.vec.value=ref.vec.value)
	return(X.ang)
}
		

			
