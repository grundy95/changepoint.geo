context('geomcp tests')
library('GeometricChangepoint')
library('testthat')
library('MASS')

##{{{Data Creation
set.seed(1)
mu <- runif(100,-5,5)
Sigma <- runif(100,0.1,4)
SingleMeanData <- rbind(mvrnorm(100,mu=mu,Sigma=diag(Sigma)),mvrnorm(100,mu=mu+0.3,Sigma=diag(Sigma)))
MultiMeanData <- rbind(mvrnorm(50,mu=mu,Sigma=diag(Sigma)),mvrnorm(50,mu=mu+0.3,Sigma=diag(Sigma)),mvrnorm(50,mu=mu-0.1,Sigma=diag(Sigma)),mvrnorm(50,mu=mu+0.3,Sigma=diag(Sigma)))

SingleVarData <- rbind(mvrnorm(100,mu=mu,Sigma=diag(Sigma)),mvrnorm(100,mu=mu,Sigma=diag(Sigma*1.3)))
MultiVarData <- rbind(mvrnorm(50,mu=mu,Sigma=diag(Sigma)),mvrnorm(50,mu=mu,Sigma=diag(Sigma*1.3)),mvrnorm(50,mu=mu,Sigma=diag(Sigma*0.9)),mvrnorm(50,mu=mu+0.3,Sigma=diag(Sigma*1.2)))

SingleMeanVarData <- rbind(mvrnorm(100,mu=mu,Sigma=diag(Sigma)),mvrnorm(100,mu=mu+0.2,Sigma=diag(Sigma*1.2)))
MultiMeanVarData <- rbind(mvrnorm(50,mu=mu,Sigma=diag(Sigma)),mvrnorm(50,mu=mu+0.2,Sigma=diag(Sigma*1.2)),mvrnorm(50,mu=mu-0.1,Sigma=diag(Sigma*0.9)),mvrnorm(50,mu=mu+0.1,Sigma=diag(Sigma*1.1)))

NullData <- mvrnorm(200,mu=mu,Sigma=diag(Sigma))

ConstantData <- matrix(rep(runif(100,-0.5,0.5),100),byrow=TRUE,ncol=100,nrow=200)

CharacterData <- matrix('test',ncol=100,nrow=200)

NAData <- SingleMeanData
rn <- c(sample(1:length(SingleMeanData[,1]),10,replace=FALSE),sample(1:length(SingleMeanData[1,]),10,replace=TRUE))
for(i in 1:(length(rn)/2)){
	NAData[rn[i],rn[i+10]] <- NA
}

UnivariateData <- rnorm(200)
##}}}
data <- list(SingleMeanData,MultiMeanData,SingleVarData,MultiVarData,SingleMeanVarData,MultiMeanVarData,NullData,ConstantData,CharacterData,NAData,UnivariateData)
data_names <- c('SingleMeanData','MultiMeanData','SingleVarData','MultiVarData','SingleMeanVarData','MultiMeanVarData','NullData','ConstantData','CharacterData','NAData','UnivariateData')
penalties <- c('MBIC','SIC','BIC','MBIC','Hannan-Quinn','Manual')
ManPenValue <- c(20,-1)
TestStats <- c('Normal','Empirical','NORMAL')
NoQuantiles <- c(10,-1)
msl <- c(20,-1,400)
Mad <- c(TRUE,FALSE)
ReferenceVector <- c('Default','Manual')
ReferenceVectorValue <- list(runif(100),rep(0,length(SingleMeanData[1,])),1)
ReferenceVectorValue_names <- c('Random','Zero','Scalar')
t <- 0


CheckManualPenalty <- function(){
		test_that(paste0('Test #',t,'. Data: ',data_names[d],'. Penalty: ',penalties[p],'. ManPenValue: ',ManPenValue[mpv],'. Test Stat: ',TestStats[ts],'. NoQunatiles: ',NoQuantiles[nq],'. MinSegLen: ',msl[m],'. MAD: ',Mad[md],'. ReferenceVector: ',ReferenceVector[rv],'. ReferenceVectorValue: ',ReferenceVectorValue_names[rvv],'.'),{
		if(data_names[d]=='ConstantData'&Mad[md]==TRUE){
			test_that(paste0('Test #',t,'. Data: ',data_names[d],'MAD: ',Mad[md]),{
				expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'Unable to perform MAD transformation')
			})
		}else if(!is.numeric(ManPenValue[mpv])){
      				expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'Your manual penalty cannot be evaluated')
			}else if(ManPenValue[mpv]<0){
      				expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'pen.value cannot be negative, please change your penalty value')
			}else{
			X <- geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]])
			expect_is(X,'cpt.geo')
			}
		})
}

CheckOtherPenalties <- function(){
		test_that(paste0('Test #',t,'. Data: ',data_names[d],'. Penalty: ',penalties[p],'. ManPenValue: ',ManPenValue[mpv],'. Test Stat: ',TestStats[ts],'. NoQunatiles: ',NoQuantiles[nq],'. MinSegLen: ',msl[m],'. MAD: ',Mad[md],'. ReferenceVector: ',ReferenceVector[rv],'. ReferenceVectorValue: ',ReferenceVectorValue_names[rvv]),{
			if(data_names[d]=='ConstantData'&Mad[md]==TRUE){
				test_that(paste0('Test #',t,'. Data: ',data_names[d],'MAD: ',Mad[md]),{
					expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'Unable to perform MAD transformation')
				})
			}else if(penalties%in%c('MBIC','SIC','BIC','MBIC','Hannan-Quinn')){
				X <- geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]])
				expect_is(X,'cpt.geo')
			}
			else{
				expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'Univariate penalty choice not recognized; should be "MBIC", "BIC", "SIC","Hannan-Quinn" or "Manual"')
			}
		})
}


t <- 0
for(d in 1:length(data)){
	if(is.element(NA,data[[d]])){
		test_that(paste0('Test #',t,'. Data: ',data_names[d]),{
			expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'Missing value: NA is not allowed in the data')
		})
		t <- t+1
	}else if(!is.matrix(data[[d]])&!is.data.frame(data[[d]])){
		test_that(paste0('Test #',t,'. Data: ',data_names[d]),{
			expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'Data type must be a matrix or data frame')
		})
		t <- t+1
	}else if(length(data[[d]][1,])<2){
		test_that(paste0('Test #',t,'. Data: ',data_names[d]),{
			expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'Univariate changepoint analysis not supported')
		})
		t <- t+1
	}else if(length(data[[d]][,1])<2){
		test_that(paste0('Test #',t,'. Data: ',data_names[d]),{
			expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'Minimum segment length is too large to include a change in this data')
		})
		t <- t+1
	}else if(!is.numeric(data[[d]])){
		test_that(paste0('Test #',t,'. Data: ',data_names[d]),{
			expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),'Only numeric data allowed')
		})
		t <- t+1
	}else{
#		print(paste('data',d))
#		invisible(readline(prompt="Press [enter] to continue"))
		for(p in 1:length(penalties)){
		#	print(paste('penalty',p))
		#	invisible(readline(prompt="Press [enter] to continue"))
			for(mpv in 1:length(ManPenValue)){
		#		print(paste('ManPenVal',mpv))
		#	invisible(readline(prompt="Press [enter] to continue"))
				for(ts in 1:length(TestStats)){
		#			print(paste('testStat',ts))
		#	invisible(readline(prompt="Press [enter] to continue"))
					for(nq in 1:length(NoQuantiles)){
		#				print(paste('NoQuantiles',nq))
		#	invisible(readline(prompt="Press [enter] to continue"))
						for(m in 1:length(msl)){
		#					print(paste('msl',m))
		#	invisible(readline(prompt="Press [enter] to continue"))
							for(md in 1:length(Mad)){
		#						print(paste('MAD',md))
		#	invisible(readline(prompt="Press [enter] to continue"))
								for(rv in 1:length(ReferenceVector)){
		#							print(paste('refVec',rv))
		#	invisible(readline(prompt="Press [enter] to continue"))
									for(rvv in 1:length(ReferenceVectorValue_names)){
		#								print(paste('refVecVal',rvv))
		#	invisible(readline(prompt="Press [enter] to continue"))
										if(penalties[p]=='Manual'){
											if(!(TestStats[ts]%in%c('Normal','Empirical'))|!is.numeric(NoQuantiles[nq])|NoQuantiles[nq]<1|msl[m]<1|msl[m]>(floor(length(data[[d]][,1])/2))|!is.logical(Mad[md])|!(ReferenceVector[rv]%in%c('Default','Manual'))){
												test_that(paste0('Test #',t,'. Data: ',data_names[d],'. Penalty: ',penalties[p],'. ManPenValue: ',ManPenValue[mpv],'. Test Stat: ',TestStats[ts],'. NoQunatiles: ',NoQuantiles[nq],'. MinSegLen: ',msl[m],'. MAD: ',Mad[md],'. ReferenceVector: ',ReferenceVector[rv],'. ReferenceVectorValue: ',ReferenceVectorValue_names[rvv]),{
													expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),)
												})
											}else if(ReferenceVector[rv]=='Manual'&(length(ReferenceVectorValue[[rvv]])!=length(data[[d]][1,])|isTRUE(all.equal(ReferenceVectorValue[[rvv]],rep(0,length(data[[d]][1,])))))){
												test_that(paste0('Test #',t,'. Data: ',data_names[d],'. Penalty: ',penalties[p],'. ManPenValue: ',ManPenValue[mpv],'. Test Stat: ',TestStats[ts],'. NoQunatiles: ',NoQuantiles[nq],'. MinSegLen: ',msl[m],'. MAD: ',Mad[md],'. ReferenceVector: ',ReferenceVector[rv],'. ReferenceVectorValue: ',ReferenceVectorValue_names[rvv]),{
													expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),)
												})
											}else{
												CheckManualPenalty()
											}
										}else{
											if(!(TestStats[ts]%in%c('Normal','Empirical'))|!is.numeric(NoQuantiles[nq])|NoQuantiles[nq]<1|msl[m]<1|msl[m]>(floor(length(data[[d]][,1])/2))|!is.logical(Mad[md])|!(ReferenceVector[rv]%in%c('Default','Manual'))){
												test_that(paste0('Test #',t,'. Data: ',data_names[d],'. Penalty: ',penalties[p],'. ManPenValue: ',ManPenValue[mpv],'. Test Stat: ',TestStats[ts],'. NoQunatiles: ',NoQuantiles[nq],'. MinSegLen: ',msl[m],'. MAD: ',Mad[md],'. ReferenceVector: ',ReferenceVector[rv],'. ReferenceVectorValue: ',ReferenceVectorValue_names[rvv]),{
													expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),)
												})
											}else if(ReferenceVector[rv]=='Manual'&(length(ReferenceVectorValue[[rvv]])!=length(data[[d]][1,])|isTRUE(all.equal(ReferenceVectorValue[[rvv]],rep(0,length(data[[d]][1,])))))){
												test_that(paste0('Test #',t,'. Data: ',data_names[d],'. Penalty: ',penalties[p],'. ManPenValue: ',ManPenValue[mpv],'. Test Stat: ',TestStats[ts],'. NoQunatiles: ',NoQuantiles[nq],'. MinSegLen: ',msl[m],'. MAD: ',Mad[md],'. ReferenceVector: ',ReferenceVector[rv],'. ReferenceVectorValue: ',ReferenceVectorValue_names[rvv]),{
													expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),)
												})
											}else{
												CheckOtherPenalties()
											}
										}
									t <- t+1
									}
								}
							}
						}
					}
				}
			}
		}
	}
}




		





