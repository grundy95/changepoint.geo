class_input <- function(data.set,distance,angle,penalty,pen.value,test.stat,msl,nquantiles,dist.cpts,ang.cpts){
	ans <- new('cpt.geo')
	data.set(ans) <- data.set
	distance(ans) <- distance
	angle(ans) <- angle
	penalty(ans) <- penalty
	pen.value(ans) <- pen.value
	msl(ans) <- msl
	test.stat(ans) <- test.stat
	nquantiles(ans) <- nquantiles
	dist.cpts(ans) <- dist.cpts
	ang.cpts(ans) <- ang.cpts
	return(ans)
}
