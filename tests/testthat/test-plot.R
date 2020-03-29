context('cpt.geo plot tests')
library('changepoint.geo')
library('testthat')
library('MASS')

set.seed(1)
mu <- runif(50,-5,5)
Sigma <- runif(50,0.1,4)
MeanVarData <- rbind(mvrnorm(30,mu=mu,Sigma=diag(Sigma)),mvrnorm(30,mu=mu+0.5,Sigma=diag(Sigma*1.2)),mvrnorm(30,mu=mu-0.5,Sigma=diag(Sigma*0.9)),mvrnorm(30,mu=mu+0.5,Sigma=diag(Sigma*1.1)))
ConstantData <- matrix(rep(runif(100,-0.5,0.5),100),byrow=TRUE,ncol=100,nrow=200)


res <- list(geomcp(MeanVarData),geomcp(ConstantData))
plottype <- c('mappings','full.data','series','partial.data')
showseries <- list(c(1,2,3),c(1,2,100))
scaleseries <- c(TRUE,FALSE)
ChangePoints <- c(TRUE,FALSE)
addmappings=c(TRUE,FALSE)
t <- 1
for(i in 1:length(res)){
	for(j in 1:length(plottype)){
		for(k in 1:length(scaleseries)){
			for(l in 1:length(ChangePoints)){
				for(m in 1:length(showseries)){
					    for(n in 1:length(addmappings)){
						if(!(toupper(plottype[j])%in%c('MAPPINGS','FULL.DATA','SERIES'))){
							test_that(paste0('Test #',t,'. plot.type: ',plottype[j]),{
								  expect_error(plot(res[[i]],plot.type=plottype[j],scale.series=scaleseries[k],changepoints=ChangePoints[l],show.series=showseries[[m]],add.mappings=addmappings[n]),'plot.type not recognized. Use either "mappings", "full.data" or "series"')
})
							t <- t+1
						}else if((i==2)&(scaleseries[k]==TRUE)&(!(toupper(plottype[j])=='MAPPINGS'))){
							test_that(paste0('Test #',t,'. Data: Constant. Scale.series: ',scaleseries[k]),{
									  expect_error(plot(res[[i]],plot.type=plottype[j],scale.series=scaleseries[k],changepoints=ChangePoints[l],show.series=showseries[[m]],add.mappings=addmappings[n]),'Series can not be scaled appropriately')
})
							t <- t+1
						}else if((toupper(plottype[j])=='SERIES')&(sum(showseries[[m]]%in%1:length(data.set(res[[i]])[1,]))!=length(showseries[[m]]))){
							test_that(paste0('Test #',t,'. plot.type: ',plottype[j],'. show.series: ',m),{
								  expect_error(plot(res[[i]],plot.type=plottype[j],scale.series=scaleseries[k],changepoints=ChangePoints[l],show.series=showseries[[m]],add.mappings=addmappings[n]),'One or more of your selected series is invalid - alter the show.series variable')
})
							t <- t+1
						}else{
							test_that(paste0('Test #',t,'. Data: ',i,'. plot.type: ',plottype[j],' scale.series: ',scaleseries[k],'. changepoints: ',ChangePoints[l],'. show.series: ',m,'. add.mappings: ',addmappings[n]),{
									  expect_is(plot(res[[i]],plot.type=plottype[j],scale.series=scaleseries[k],changepoints=ChangePoints[l],show.series=m,add.mappings=addmappings[n]),'ggplot')
})
							t <- t+1
						}
					}
				}
			}
		}
	}
}

						   


