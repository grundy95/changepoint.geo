context('geomcp tests')
library('changepoint.geo')
library('testthat')
library('MASS')

##{{{Data Creation
set.seed(1)
mu <- runif(100,-5,5)
Sigma <- runif(100,0.1,4)
MeanVarData <- rbind(mvrnorm(50,mu=mu,Sigma=diag(Sigma)),mvrnorm(50,mu=mu+0.2,Sigma=diag(Sigma*1.2)),mvrnorm(50,mu=mu-0.1,Sigma=diag(Sigma*0.9)),mvrnorm(50,mu=mu+0.1,Sigma=diag(Sigma*1.1)))

ConstantData <- matrix(rep(runif(100,-0.5,0.5),100),byrow=TRUE,ncol=100,nrow=200)

CharacterData <- matrix('test',ncol=100,nrow=200)

NAData <- MeanVarData
rn <- c(sample(1:length(MeanVarData[,1]),10,replace=FALSE),sample(1:length(MeanVarData[1,]),10,replace=TRUE))
for(i in 1:(length(rn)/2)){
	NAData[rn[i],rn[i+10]] <- NA
}

UnivariateData <- rnorm(200)
##}}}
data <- list(MeanVarData,ConstantData,CharacterData,NAData,UnivariateData)
data_names <- c('MeanVarData','ConstantData','CharacterData','NAData','UnivariateData')
penalties <- c('MBIC','SIC','BIC','MBIC','Hannan-Quinn','Manual','mbic')
ManPenValue <- c(20,-1)
TestStats <- c('Normal','Empirical')
NoQuantiles <- c(10,-1)
msl <- c(20,-1,400)
Mad <- c(TRUE,FALSE)
ReferenceVector <- c('Default','Manual')
ReferenceVectorValue <- list(runif(100),rep(0,length(MeanVarData[1,])),1)
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
			}else if(toupper(penalties[p])%in%c('MBIC','SIC','BIC','MBIC','HANNAN-QUINN')){
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
		for(p in 1:length(penalties)){
			for(mpv in 1:length(ManPenValue)){
				for(ts in 1:length(TestStats)){
					for(nq in 1:length(NoQuantiles)){
						for(m in 1:length(msl)){
							for(md in 1:length(Mad)){
								for(rv in 1:length(ReferenceVector)){
									for(rvv in 1:length(ReferenceVectorValue_names)){
										if(toupper(penalties[p])=='MANUAL'){
											if(!(toupper(TestStats[ts])%in%c('NORMAL','EMPIRICAL'))|!is.numeric(NoQuantiles[nq])|NoQuantiles[nq]<1|msl[m]<1|msl[m]>(floor(length(data[[d]][,1])/2))|!is.logical(Mad[md])|!(toupper(ReferenceVector[rv])%in%c('DEFAULT','MANUAL'))){
												test_that(paste0('Test #',t,'. Data: ',data_names[d],'. Penalty: ',penalties[p],'. ManPenValue: ',ManPenValue[mpv],'. Test Stat: ',TestStats[ts],'. NoQunatiles: ',NoQuantiles[nq],'. MinSegLen: ',msl[m],'. MAD: ',Mad[md],'. ReferenceVector: ',ReferenceVector[rv],'. ReferenceVectorValue: ',ReferenceVectorValue_names[rvv]),{
													expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),)
												})
											}else if(toupper(ReferenceVector[rv])=='MANUAL'&(length(ReferenceVectorValue[[rvv]])!=length(data[[d]][1,])|isTRUE(all.equal(ReferenceVectorValue[[rvv]],rep(0,length(data[[d]][1,])))))){
												test_that(paste0('Test #',t,'. Data: ',data_names[d],'. Penalty: ',penalties[p],'. ManPenValue: ',ManPenValue[mpv],'. Test Stat: ',TestStats[ts],'. NoQunatiles: ',NoQuantiles[nq],'. MinSegLen: ',msl[m],'. MAD: ',Mad[md],'. ReferenceVector: ',ReferenceVector[rv],'. ReferenceVectorValue: ',ReferenceVectorValue_names[rvv]),{
													expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),)
												})
											}else{
												CheckManualPenalty()
											}
										}else{
											if(!(toupper(TestStats[ts])%in%c('NORMAL','EMPIRICAL'))|!is.numeric(NoQuantiles[nq])|NoQuantiles[nq]<1|msl[m]<1|msl[m]>(floor(length(data[[d]][,1])/2))|!is.logical(Mad[md])|!(toupper(ReferenceVector[rv])%in%c('DEFAULT','MANUAL'))){
												test_that(paste0('Test #',t,'. Data: ',data_names[d],'. Penalty: ',penalties[p],'. ManPenValue: ',ManPenValue[mpv],'. Test Stat: ',TestStats[ts],'. NoQunatiles: ',NoQuantiles[nq],'. MinSegLen: ',msl[m],'. MAD: ',Mad[md],'. ReferenceVector: ',ReferenceVector[rv],'. ReferenceVectorValue: ',ReferenceVectorValue_names[rvv]),{
													expect_error(geomcp(X=data[[d]],penalty=penalties[p],pen.value=ManPenValue[mpv],test.stat=TestStats[ts],msl=msl[m],nquantiles=NoQuantiles[nq],MAD=Mad[md],ref.vec=ReferenceVector[rv],ref.vec.value=ReferenceVectorValue[[rvv]]),)
												})
											}else if(toupper(ReferenceVector[rv])=='MANUAL'&(length(ReferenceVectorValue[[rvv]])!=length(data[[d]][1,])|isTRUE(all.equal(ReferenceVectorValue[[rvv]],rep(0,length(data[[d]][1,])))))){
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




		





