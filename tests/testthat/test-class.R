context('cpt.geo class tests')
library('GeometricChangepoint')
library('testthat')
library('MASS')

set.seed(1)
mu <- runif(50,-5,5)
Sigma <- runif(50,0.1,4)
MeanVarData <- rbind(mvrnorm(30,mu=mu,Sigma=diag(Sigma)),mvrnorm(30,mu=mu+0.2,Sigma=diag(Sigma*1.2)),mvrnorm(30,mu=mu-0.1,Sigma=diag(Sigma*0.9)),mvrnorm(30,mu=mu+0.1,Sigma=diag(Sigma*1.1)))

X <- geomcp(MeanVarData)
test_that(paste0('Test cpt.geo class'),{
		  expect_is(X,'cpt.geo')
		  expect_is(X@data.set,'matrix')
		  expect_is(X@distance,'numeric')
		  expect_is(X@angle,'numeric')
		  expect_is(X@penalty,'character')
		  expect_is(X@pen.value,'numeric')
		  expect_is(X@test.stat,'character')
		  expect_is(X@msl,'numeric')
		  expect_is(X@nquantiles,'integer')
		  expect_is(X@dist.cpts,'integer')
		  expect_is(X@ang.cpts,'integer')
})



