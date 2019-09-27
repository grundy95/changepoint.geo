class_input <- function(data.set,mapped.data,dist.measure,penalty,pen.value,test.stat,msl,nquantiles,dist.cpts,ang.cpts){
	ans <- new('cpt.geo')
	data.set(ans) <- data.set
	mapped.data(ans) <- mapped.data
	dist.measure(ans) <- dist.measure
	penalty(ans) <- penalty
	pen.value(ans) <- pen.value
	msl(ans) <- msl
	test.stat(ans) <- test.stat
	nquantiles(ans) <- nquantiles
	dist.cpts(ans) <- dist.cpts
	ang.cpts(ans) <- ang.cpts
	return(ans)
}
