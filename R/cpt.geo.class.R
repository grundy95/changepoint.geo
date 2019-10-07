setClass('cpt.geo',representation(data.set='matrix',distance='numeric',angle='numeric',penalty='character',pen.value='numeric',test.stat='character',msl='numeric',nquantiles='numeric',dist.cpts='numeric',ang.cpts='numeric', date='character',version='character'),prototype=prototype(version=as(packageVersion("GeometricChangepoint"),'character'),date=date()))

if(!isGeneric('data.set')){	
	if(is.function('data.set')){
	fun <- data.set
	}
	else{
		fun <- function(object){
			standardGeneric('data.set')
		}
	}
	setGeneric('data.set',fun)
}
setMethod('data.set','cpt.geo',function(object) object@data.set)

if(!isGeneric('distance')){
	if(is.function('distance')){
	fun <- distance
	}
	else{
		fun <- function(object){
			standardGeneric('distance')
		}
	}
	setGeneric('distance',fun)
}
setMethod('distance','cpt.geo',function(object) object@distance)

if(!isGeneric('angle')){
	if(is.function('angle')){
		fun <- angle
	}
	else{
		fun <- function(object){
			standardGeneric('angle')
		}
	}
	setGeneric('angle',fun)
}
setMethod('angle','cpt.geo',function(object) object@angle)

if(!isGeneric('penalty')){
	if(is.function('penalty')){
		fun <- penalty
	}
	else{
		fun <- function(object){
			standardGeneric('penalty')
		}
	}
	setGeneric('penalty',fun)
}
setMethod('penalty','cpt.geo',function(object) object@penalty)

if(!isGeneric('pen.value')){
	if(is.function('pen.value')){
		fun <- pen.value
	}
	else{
		fun <- function(object){
			standardGeneric('pen.value')
		}
	}
	setGeneric('pen.value',fun)
}
setMethod('pen.value','cpt.geo',function(object) object@pen.value)

if(!isGeneric('test.stat')){
	if(is.function('test.stat')){
		fun <- test.stat
	}
	else{
		fun <- function(object){
			standardGeneric('test.stat')
		}
	}
	setGeneric('test.stat',fun)
}
setMethod('test.stat','cpt.geo',function(object) object@test.stat)

if(!isGeneric('msl')){
	if(is.function('msl')){
		fun <- msl
	}
	else{
		fun <- function(object){
			standardGeneric('msl')
		}
	}
	setGeneric('msl',fun)
}
setMethod('msl','cpt.geo',function(object) object@msl)

if(!isGeneric('nquantiles')){
	if(is.function('nquantiles')){
		fun <- nquantiles
	}
	else{
		fun <- function(object){
			standardGeneric('nquantiles')
		}
	}
	setGeneric('nquantiles',fun)
}
setMethod('nquantiles','cpt.geo',function(object){
		  if(object@nquantiles==0){
			  stop('nquantiles not used with Normal test statistic')
		  }
		  return(object@nquantiles)
})

if(!isGeneric('dist.cpts')){
	if(is.function('dist.cpts')){
		fun <- dist.cpts
	}
	else{
		fun <- function(object){
			standardGeneric('dist.cpts')
		}
	}
	setGeneric('dist.cpts',fun)
}
setMethod('dist.cpts','cpt.geo',function(object) object@dist.cpts)

if(!isGeneric('ang.cpts')){
	if(is.function('ang.cpts')){
		fun <- ang.cpts
	}
	else{
		fun <- function(object){
			standardGeneric('ang.cpts')
		}
	}
	setGeneric('ang.cpts',fun)
}
setMethod('ang.cpts','cpt.geo',function(object) object@ang.cpts)

#Replacement functions for slots
setGeneric('data.set<-', function(object,value) standardGeneric('data.set<-'))
setReplaceMethod('data.set','cpt.geo',function(object,value){
			 object@data.set <- value
			 return(object)
})
setGeneric('distance<-', function(object,value) standardGeneric('distance<-'))
setReplaceMethod('distance','cpt.geo',function(object,value){
			 object@distance <- value
			 return(object)
})
setGeneric('angle<-', function(object,value) standardGeneric('angle<-'))
setReplaceMethod('angle','cpt.geo',function(object,value){
			 object@angle <- value
			 return(object)
})
setGeneric('penalty<-', function(object,value) standardGeneric('penalty<-'))
setReplaceMethod('penalty','cpt.geo',function(object,value){
			 object@penalty <- value
			 return(object)
})
setGeneric('pen.value<-', function(object,value) standardGeneric('pen.value<-'))
setReplaceMethod('pen.value','cpt.geo',function(object,value){
			 object@pen.value <- value
			 return(object)
})
setGeneric('test.stat<-', function(object,value) standardGeneric('test.stat<-'))
setReplaceMethod('test.stat','cpt.geo',function(object,value){
			 object@test.stat <- value
			 return(object)
})
setGeneric('msl<-', function(object,value) standardGeneric('msl<-'))
setReplaceMethod('msl','cpt.geo',function(object,value){
			 object@msl <- value
			 return(object)
})
setGeneric('nquantiles<-', function(object,value) standardGeneric('nquantiles<-'))
setReplaceMethod('nquantiles','cpt.geo',function(object,value){
			 object@nquantiles <- value
			 return(object)
})
setGeneric('dist.cpts<-', function(object,value) standardGeneric('dist.cpts<-'))
setReplaceMethod('dist.cpts','cpt.geo',function(object,value){
			 object@dist.cpts <- value
			 return(object)
})
setGeneric('ang.cpts<-', function(object,value) standardGeneric('ang.cpts<-'))
setReplaceMethod('ang.cpts','cpt.geo',function(object,value){
			 object@ang.cpts <- value
			 return(object)
})

#Summary
setMethod('summary','cpt.geo',function(object){
		   cat('Created using GeometricChangepoint version',object@version,'\n')
		   cat('Univariate Test Stat	: ',test.stat(object),'\n')
		   cat('Type of penalty		: ',penalty(object),' with value ',pen.value(object),'\n')
		   cat('Minimum Segment Length	: ',msl(object),'\n')
		   cat('Distance Changepoints	: ',dist.cpts(object),'\n')
		   cat('Angle Changepoints	: ',ang.cpts(object),'\n')
})

#show functions
setMethod('show','cpt.geo',function(object){
		  cat('Class, cpt.geo	: Geometric Changepoint Object\n')
		  cat('			: S4 class containing', length(attributes(object))-1,'slots with names\n')
		  cat('			 ',names(attributes(object))[1:(length(attributes(object))-1)],'\n\n')
		  cat('Created on 	:', object@date,'\n\n')
		  cat('Summary(.)	:\n----------\n')
		  summary(object)
})

#Plot function
setMethod('plot','cpt.geo',function(x,ylab='Value',xlab='Time',changepoints=TRUE){
	Data <- data.frame('time'=1:length(distance(ans)),'Distance'=distance(ans),'Angle'=angle(ans))
	Data <- Data %>%
		select(time,Distance,Angle) %>%
		gather(key='mapping',value='value',-time)
	Data$mapping <- factor(Data$mapping, levels=c("Distance","Angle"))
	p <- ggplot(Data,aes(x=time,y=value))+geom_line()+facet_grid(mapping ~ .,scales='free')+labs(x=xlab,y=ylab)
	if(changepoints){
		cpts.all <- data.frame('mapping'=c(rep('Distance',length(dist.cpts(ans))),rep('Angle',length(ang.cpts(ans)))),'Cpts'=c(dist.cpts(ans),ang.cpts(ans)))
		p <- p+geom_vline(aes(xintercept=Cpts,color=mapping),cpts.all,linetype='longdash',size=1.2)+theme(legend.position='none')
	}
	return(p)
})

