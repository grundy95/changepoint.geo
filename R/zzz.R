.onAttach <- function(libname,pkgname){
	f <- read.dcf(file.path(libname,pkgname,"DESCRIPTION"),
		c("Version","Date"))
	packageStartupMessage('\n Successfully loaded changepoint.geo package version ',f[1,1])
}
